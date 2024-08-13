package vexriscv.afrl


import vexriscv.plugin._
import vexriscv._
import vexriscv.ip.{DataCacheConfig, InstructionCacheConfig}
import spinal.core._
import spinal.lib._
import spinal.lib.bus.amba3.apb._
import spinal.lib.bus.amba4.axi._
import spinal.lib.bus.amba4.axilite._
import spinal.lib.com.jtag.Jtag
import spinal.lib.com.jtag.sim.JtagTcp
// import spinal.lib.com.uart.sim.{UartDecoder, UartEncoder}
import spinal.lib.com.uart._
import spinal.lib.com.spi._
import spinal.lib.com.eth._
import spinal.lib.com.i2c._
import spinal.lib.graphic.RgbConfig
import spinal.lib.graphic.vga._
import spinal.lib.io.TriStateArray
import spinal.lib.misc.{HexTools, InterruptCtrl, Prescaler, Timer}
import spinal.lib.system.debugger.{JtagAxi4SharedDebugger, JtagBridge, SystemDebugger, SystemDebuggerConfig}

import scala.collection.mutable.ArrayBuffer
import scala.collection.Seq

case class VeronicaConfig(cpuPlugins         : ArrayBuffer[Plugin[VexRiscv]])

object configBUS {
  def getAxi4Config() = Axi4Config(
    addressWidth = 32,
    dataWidth = 32,
    idWidth = 4,
    useId = true,
    useRegion = true,
    useBurst = true,
    useLock = false,
    useQos = false,
    useLen = true,
    useResp = false
  )

  def getAxi4ConfigNoID() = Axi4Config(
    addressWidth = 32,
    dataWidth = 32,
    useId = false,
    useRegion = false,
    useBurst = false,
    useLock = false,
    useQos = false,
    useResp = false
  )

  def getAxiLite4Config() = AxiLite4Config(
    addressWidth = 32,
    dataWidth = 32
  )

  def getPhyConfig() = PhyParameter(
    txDataWidth = 2,
    rxDataWidth = 2
  )
}

//axi lite connection
case class AxiLite4Output(axiConfig : Axi4Config) extends Component{
  val io = new Bundle {
    val input  = slave(Axi4Shared(axiConfig))
    val output = master(AxiLite4(AxiLite4Config(
                              addressWidth = axiConfig.addressWidth,
                              dataWidth = axiConfig.dataWidth,
                              readIssuingCapability = axiConfig.readIssuingCapability,
                              writeIssuingCapability = axiConfig.writeIssuingCapability,
                              combinedIssuingCapability = axiConfig.combinedIssuingCapability,
                              readDataReorderingDepth = axiConfig.readDataReorderingDepth
    )))
  }

  io.output <> AxiLite4Utils.Axi4Rich(io.input.toAxi4()).toLite()
}

case class Axi4Output(axiConfig : Axi4Config) extends Component{
  val io = new Bundle {
    val input  = slave(Axi4Shared(axiConfig))
    val output = master(Axi4(axiConfig))
  }

  io.output <> io.input.toAxi4()
}

case class Apb3MacEth(p : MacEthParameter,
                     txCd : ClockDomain,
                     rxCd : ClockDomain) extends Component{
  val io = new Bundle{
    val apb = slave(Apb3(
      addressWidth = 8,
      dataWidth = 32
    ))
    val phy = master(PhyIo(p.phy))
    val interrupt = out Bool ()
  }

  val mac = new MacEth(p, txCd, rxCd)
  io.phy <> mac.io.phy

  val busCtrl = Apb3SlaveFactory(io.apb)
  val bridge = mac.io.ctrl.driveFrom(busCtrl)
  io.interrupt := bridge.interruptCtrl.pending
}

class Apb3Timer extends Component{
  val io = new Bundle {
    val apb = slave(Apb3(
      addressWidth = 8,
      dataWidth = 32
    ))
    val interrupt = out Bool()
  }

  val prescaler = Prescaler(16)
  val timerA,timerB = Timer(16)

  val busCtrl = Apb3SlaveFactory(io.apb)
  val prescalerBridge = prescaler.driveFrom(busCtrl,0x00)

  val timerABridge = timerA.driveFrom(busCtrl,0x40)(
    ticks  = List(True, prescaler.io.overflow),
    clears = List(timerA.io.full)
  )

  val timerBBridge = timerB.driveFrom(busCtrl,0x50)(
    ticks  = List(True, prescaler.io.overflow),
    clears = List(timerB.io.full)
  )

  val interruptCtrl = InterruptCtrl(2)
  val interruptCtrlBridge = interruptCtrl.driveFrom(busCtrl,0x10)
  interruptCtrl.io.inputs(0) := timerA.io.full
  interruptCtrl.io.inputs(1) := timerB.io.full
  io.interrupt := interruptCtrl.io.pendings.orR
}

object VeronicaConfig{
  def default() =  VeronicaConfig(
    cpuPlugins = ArrayBuffer(
      new PcManagerSimplePlugin(0x80000000l, false),
      new IBusCachedPlugin(
        resetVector = 0x80000000l,
        prediction = STATIC,
        config = InstructionCacheConfig(
          cacheSize = 4096,
          bytePerLine =32,
          wayCount = 1,
          addressWidth = 32,
          cpuDataWidth = 32,
          memDataWidth = 32,
          catchIllegalAccess = true,
          catchAccessFault = true,
          asyncTagMemory = false,
          twoCycleRam = true,
          twoCycleCache = true
        )
        //            askMemoryTranslation = true,
        //            memoryTranslatorPortConfig = MemoryTranslatorPortConfig(
        //              portTlbSize = 4
        //            )
      ),
      //                    new DBusSimplePlugin(
      //                      catchAddressMisaligned = true,
      //                      catchAccessFault = true
      //                    ),
      new DBusCachedPlugin(
        config = new DataCacheConfig(
          cacheSize         = 4096,
          bytePerLine       = 32,
          wayCount          = 1,
          addressWidth      = 32,
          cpuDataWidth      = 32,
          memDataWidth      = 32,
          catchAccessError  = true,
          catchIllegal      = true,
          catchUnaligned    = true
        ),
        memoryTranslatorPortConfig = null
        //            memoryTranslatorPortConfig = MemoryTranslatorPortConfig(
        //              portTlbSize = 6
        //            )
      ),
      new StaticMemoryTranslatorPlugin(
        ioRange      = _(31 downto 28) === 0xF
      ),
      new DecoderSimplePlugin(
        catchIllegalInstruction = true
      ),
      new RegFilePlugin(
        regFileReadyKind = plugin.SYNC,
        zeroBoot = false
      ),
      new IntAluPlugin,
      new SrcPlugin(
        separatedAddSub = false,
        executeInsertion = true
      ),
      new FullBarrelShifterPlugin,
      new MulPlugin,
      new DivPlugin,
      new HazardSimplePlugin(
        bypassExecute           = true,
        bypassMemory            = true,
        bypassWriteBack         = true,
        bypassWriteBackBuffer   = true,
        pessimisticUseSrc       = false,
        pessimisticWriteRegFile = false,
        pessimisticAddressMatch = false
      ),
      new BranchPlugin(
        earlyBranch = false,
        catchAddressMisaligned = true
      ),
      new ExternalInterruptArrayPlugin,
      new CsrPlugin(
        config = CsrPluginConfig(
          catchIllegalAccess = false,
          mvendorid      = null,
          marchid        = null,
          mimpid         = null,
          mhartid        = null,
          misaExtensionsInit = 66,
          misaAccess     = CsrAccess.NONE,
          mtvecAccess    = CsrAccess.NONE,
          mtvecInit      = 0x80000020l,
          mepcAccess     = CsrAccess.READ_WRITE,
          mscratchGen    = false,
          mcauseAccess   = CsrAccess.READ_ONLY,
          mbadaddrAccess = CsrAccess.READ_ONLY,
          mcycleAccess   = CsrAccess.NONE,
          minstretAccess = CsrAccess.NONE,
          ecallGen       = false,
          wfiGenAsWait         = false,
          ucycleAccess   = CsrAccess.NONE,
          uinstretAccess = CsrAccess.NONE
        )
      ),
      new YamlPlugin("veronica_cpu0.yaml")
    )
  )
}



class Veronica(val config: VeronicaConfig) extends Component{

  import config._
  def vgaRgbConfig = RgbConfig(3,3,2)

  val io = new Bundle{
    //Clocks / reset
    val arst    = in Bool()
    val axi_clk = in Bool()
    val vga_clk = in Bool()
    val ddr_clk = in Bool()
    val eth_clk = in Bool()

    val s_axi_dma0_aclk   = in Bool()
    val s_axi_dma0_arstn  = in Bool()

    //Main components IO
    val jtag       = slave(Jtag())

    //external axi interfaces
    val m_axi_mbus = master(Axi4(configBUS.getAxi4Config()))
    val m_axi_acc  = master(AxiLite4(configBUS.getAxiLite4Config()))
    val s_axi_dma0 = slave(Axi4(configBUS.getAxi4ConfigNoID()))

    //Peripherals IO
    val gpioA         = master(TriStateArray(32 bits))
    val gpioB         = master(TriStateArray(32 bits))
    val uart          = master(Uart())
    val phy           = master(PhyIo(configBUS.getPhyConfig()))
    val spi           = master(SpiMaster(ssWidth = 8))
    val i2c           = master(I2c())
    val vga           = master(Vga(vgaRgbConfig))
    val irq           = in Bits(28 bits)
  }

  val resetCtrlClockDomain = ClockDomain(
    clock = io.axi_clk,
    config = ClockDomainConfig(
      resetKind = BOOT
    )
  )

  val resetCtrl = new ClockingArea(resetCtrlClockDomain) {
    val systemResetUnbuffered  = False
    //    val coreResetUnbuffered = False

    //Implement an counter to keep the reset axiResetOrder high 64 cycles
    // Also this counter will automaticly do a reset when the system boot.
    val systemResetCounter = Reg(UInt(6 bits)) init(0)
    when(systemResetCounter =/= U(systemResetCounter.range -> true)){
      systemResetCounter := systemResetCounter + 1
      systemResetUnbuffered := True
    }
    when(BufferCC(io.arst)){
      systemResetCounter := 0
    }

    //Create all reset used later in the design
    val systemReset  = RegNext(systemResetUnbuffered)
    val axiReset     = RegNext(systemResetUnbuffered)
    val vgaReset     = BufferCC(axiReset)
  }

  val axiClockDomain = ClockDomain(
    clock = io.axi_clk,
    reset = resetCtrl.axiReset
  )

  val debugClockDomain = ClockDomain(
    clock = io.axi_clk,
    reset = resetCtrl.systemReset
  )

  val vgaClockDomain = ClockDomain(
    clock = io.vga_clk,
    reset = resetCtrl.vgaReset
  )

  val ddrClockDomain = ClockDomain(
    clock = io.ddr_clk,
    reset = resetCtrl.axiReset
  )

  val ethClockDomain = ClockDomain(
    clock = io.eth_clk,
    reset = resetCtrl.axiReset
  )

  val axiSlaveDma0ClockDomain = ClockDomain(
    clock = io.s_axi_dma0_aclk,
    reset = io.s_axi_dma0_arstn
  )


  val axi = new ClockingArea(axiClockDomain) {
    val ram = Axi4SharedOnChipRam(
      dataWidth = 32,
      byteCount = 8 kB,
      idWidth   = 4
    )

    val uartCtrl = Apb3UartCtrl(UartCtrlMemoryMappedConfig(
      uartCtrlConfig = UartCtrlGenerics(
        dataWidthMax      = 8,
        clockDividerWidth = 20,
        preSamplingSize   = 1,
        samplingSize      = 5,
        postSamplingSize  = 2
      ),
      txFifoDepth = 16,
      rxFifoDepth = 16
    ))

    val spiCtrl = Apb3SpiMasterCtrl(SpiMasterCtrlMemoryMappedConfig(
      ctrlGenerics = SpiMasterCtrlGenerics(
        ssWidth = 8,
        timerWidth = 32,
        dataWidth = 8
      )
    ))

    val ethCtrl = new Apb3MacEth(MacEthParameter(
      phy = configBUS.getPhyConfig(),
      rxDataWidth = 32,
      rxBufferByteSize = 4096,
      txDataWidth = 32,
      txBufferByteSize = 4096
      ),
      ethClockDomain,
      ethClockDomain
    )

    val i2cCtrl = new Apb3I2cCtrl(I2cSlaveMemoryMappedGenerics(
      ctrlGenerics = I2cSlaveGenerics(
        samplingWindowSize = 3,
        samplingClockDividerWidth = 10 bits,
        timeoutWidth = 20 bits
      ),
      addressFilterCount = 4,
      masterGenerics = I2cMasterMemoryMappedGenerics(
        timerWidth = 12
      )
    ))

    val axi4dma0 = Axi4CC(configBUS.getAxi4ConfigNoID(), axiSlaveDma0ClockDomain, axiClockDomain, 16, 16, 16, 16, 16)

    val axi4acc   = AxiLite4Output(configBUS.getAxi4Config())

    val axi4mbus  = Axi4CC(configBUS.getAxi4Config(), axiClockDomain, ddrClockDomain, 16, 16, 16, 16, 16)

    val timerInterrupt = False

    val core = new Area{
      val config = VexRiscvConfig(
        plugins = cpuPlugins += new DebugPlugin(debugClockDomain)
      )

      val cpu = new VexRiscv(config)
      var iBus : Axi4ReadOnly = null
      var dBus : Axi4Shared = null

      for(plugin <- config.plugins) plugin match{
        case plugin : IBusCachedPlugin => iBus = plugin.iBus.toAxi4ReadOnly()
        case plugin : DBusCachedPlugin => dBus = plugin.dBus.toAxi4Shared(true)
        case plugin : ExternalInterruptArrayPlugin => {
          io.irq <> plugin.externalInterruptArray(27 downto 0)
          plugin.externalInterruptArray(28) := i2cCtrl.io.interrupt
          plugin.externalInterruptArray(29) := ethCtrl.io.interrupt
          plugin.externalInterruptArray(30) := spiCtrl.io.interrupt
          plugin.externalInterruptArray(31) := uartCtrl.io.interrupt
        }
        case plugin : CsrPlugin        => {
          plugin.timerInterrupt := timerInterrupt
        }
        case plugin : DebugPlugin      => debugClockDomain{
          resetCtrl.axiReset setWhen(RegNext(plugin.io.resetOut))
          io.jtag <> plugin.io.bus.fromJtag()
        }
        case _ =>
      }
    }

    val apbBridge = Axi4SharedToApb3Bridge(
      addressWidth = 20,
      dataWidth    = 32,
      idWidth      = 4
    )

    val timer = new Apb3Timer()
    timerInterrupt setWhen(timer.io.interrupt)

    val gpioACtrl = Apb3Gpio(
      gpioWidth = 32,
      withReadSync = true
    )
    val gpioBCtrl = Apb3Gpio(
      gpioWidth = 32,
      withReadSync = true
    )

    val vgaCtrlConfig = Axi4VgaCtrlGenerics(
      axiAddressWidth = 32,
      axiDataWidth    = 32,
      burstLength     = 8,
      frameSizeMax    = 2048*1512*2,
      fifoSize        = 512,
      rgbConfig       = vgaRgbConfig,
      vgaClock        = vgaClockDomain
    )
    val vgaCtrl = Axi4VgaCtrl(vgaCtrlConfig)

    val axiCrossbar = Axi4CrossbarFactory()

    axiCrossbar.addSlaves(
      ram.io.axi          -> (0x80000000L,   8 kB),
      axi4mbus.io.input   -> (0x90000000L,   1 GB),
      axi4acc.io.input    -> (0x70000000L,   256 MB),
      apbBridge.io.axi    -> (0xF0000000L,   1 MB)
    )

    axiCrossbar.addConnections(
      core.iBus           -> List(ram.io.axi, axi4mbus.io.input),
      core.dBus           -> List(ram.io.axi, axi4mbus.io.input, apbBridge.io.axi, axi4acc.io.input),
      vgaCtrl.io.axi      -> List(axi4mbus.io.input),
      axi4dma0.io.output  -> List(axi4mbus.io.input)
    )


    axiCrossbar.addPipelining(apbBridge.io.axi)((crossbar,bridge) => {
      crossbar.sharedCmd.halfPipe() >> bridge.sharedCmd
      crossbar.writeData.halfPipe() >> bridge.writeData
      crossbar.writeRsp             << bridge.writeRsp
      crossbar.readRsp              << bridge.readRsp
    })

    axiCrossbar.addPipelining(ram.io.axi)((crossbar,ctrl) => {
      crossbar.sharedCmd.halfPipe()  >>  ctrl.sharedCmd
      crossbar.writeData            >/-> ctrl.writeData
      crossbar.writeRsp              <<  ctrl.writeRsp
      crossbar.readRsp               <<  ctrl.readRsp
    })

    axiCrossbar.addPipelining(vgaCtrl.io.axi)((ctrl,crossbar) => {
      ctrl.readCmd.halfPipe()    >>  crossbar.readCmd
      ctrl.readRsp               <<  crossbar.readRsp
    })

    axiCrossbar.addPipelining(core.dBus)((cpu,crossbar) => {
      cpu.sharedCmd             >>  crossbar.sharedCmd
      cpu.writeData             >>  crossbar.writeData
      cpu.writeRsp              <<  crossbar.writeRsp
      cpu.readRsp               <-< crossbar.readRsp //Data cache directly use read responses without buffering, so pipeline it for FMax
    })

    axiCrossbar.build()


    val apbDecoder = Apb3Decoder(
      master = apbBridge.io.apb,
      slaves = List(
        gpioACtrl.io.apb -> (0x00000, 4 kB),
        gpioBCtrl.io.apb -> (0x01000, 4 kB),
        uartCtrl.io.apb  -> (0x10000, 4 kB),
        timer.io.apb     -> (0x20000, 4 kB),
        vgaCtrl.io.apb   -> (0x30000, 4 kB),
        spiCtrl.io.apb   -> (0x40000, 4 kB),
        ethCtrl.io.apb   -> (0x50000, 4 kB),
        i2cCtrl.io.apb   -> (0x60000, 4 kB)
      )
    )
  }

  AxiLite4SpecRenamer(master(io.m_axi_acc)  .setName("m_axi_acc"))

  Axi4SpecRenamer(master(io.m_axi_mbus).setName("m_axi_mbus"))
  Axi4SpecRenamer(slave(io.s_axi_dma0) .setName("s_axi_dma0"))

  io.gpioA          <> axi.gpioACtrl.io.gpio
  io.gpioB          <> axi.gpioBCtrl.io.gpio
  io.uart           <> axi.uartCtrl.io.uart
  io.spi            <> axi.spiCtrl.io.spi
  io.vga            <> axi.vgaCtrl.io.vga
  io.m_axi_mbus     <> axi.axi4mbus.io.output
  io.m_axi_acc      <> axi.axi4acc.io.output
  io.phy            <> axi.ethCtrl.io.phy
  io.i2c            <> axi.i2cCtrl.io.i2c
  io.s_axi_dma0     <> axi.axi4dma0.io.input
}

object Veronica{
  def main(args: Array[String]) {
    val config = SpinalConfig()
    config.generateVerilog({
      val toplevel = new Veronica(VeronicaConfig.default)
      toplevel
    })
  }
}
