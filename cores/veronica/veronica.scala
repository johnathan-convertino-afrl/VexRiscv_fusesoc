package vexriscv.afrl

import java.nio.{ByteBuffer, ByteOrder}

import spinal.core._
import spinal.lib._
import spinal.lib.bus.amba3.apb._
import spinal.lib.misc.HexTools
import spinal.lib.misc._
import spinal.lib.misc.plic._
import spinal.lib.misc.AxiLite4Clint
import spinal.lib.misc.plic.AxiLite4Plic
import spinal.lib.bus.amba4.axi._
import spinal.lib.bus.amba4.axilite._
import spinal.lib.com.jtag.{Jtag, JtagTapInstructionCtrl}
import spinal.lib.eda.altera.{InterruptReceiverTag, ResetEmitterTag}
import spinal.lib.bus.wishbone._
import spinal.lib.io.TriStateArray
import spinal.lib.cpu.riscv.RiscvHart
import spinal.lib.misc.Apb3Clint
import spinal.lib.{Flow, master}
import spinal.lib.com.usb.ohci._
import spinal.lib.com.usb.phy.UsbHubLsFs.CtrlCc
import spinal.lib.com.usb.phy._
import vexriscv.ip.{DataCacheConfig, InstructionCacheConfig}
import vexriscv.plugin._
import vexriscv.{DecoderService, Stageable, Riscv, VexRiscv, VexRiscvConfig, plugin}
import vexriscv.ip.fpu.FpuParameter

import scala.collection.mutable.ArrayBuffer

object Config {
  def spinal = SpinalConfig(
    targetDirectory = "."
  )
}

object configBUS {
  def getAxi4Config() = Axi4Config(
    addressWidth = 32,
    dataWidth = 32,
    idWidth = 4,
    useId = true,
    useRegion = false,
    useBurst = true,
    useLock = true,
    useQos = true,
    useLen = true,
    useResp = true
  )

  def getAxi4ConfigNoID() = Axi4Config(
    addressWidth = getAxi4Config().addressWidth,
    dataWidth = getAxi4Config().dataWidth,
    useId = false,
    useRegion = false,
    useBurst = true,
    useLock = false,
    useQos = false,
    useResp = true
  )
    
  def getAxiLite4Config() = AxiLite4Config(
    addressWidth = getAxi4Config().addressWidth,
    dataWidth = getAxi4Config().dataWidth
  )
  
  def getAxi4RamConfig(dataWidth : Int, byteCount : BigInt,idWidth : Int) = Axi4Config(
    addressWidth = log2Up(byteCount),
    dataWidth = dataWidth,
    idWidth = idWidth,
    useLock = false,
    useRegion = false,
    useCache = false,
    useProt = false,
    useQos = false
  )
}

//axi lite connection
case class Axi4SharedToAxiLite4(axiConfig : Axi4Config) extends Component{
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

//axi connection
case class Axi4SharedToAxi4V(axiConfig : Axi4Config) extends Component{
  val io = new Bundle {
    val input  = slave(Axi4Shared(axiConfig))
    val output = master(Axi4(axiConfig))
  }

  io.output <> io.input.toAxi4()
}

case class Axi4ToAxi4SharedV(axiConfig : Axi4Config) extends Component{
  val io = new Bundle {
    val input  = slave(Axi4(axiConfig))
    val output = master(Axi4Shared(axiConfig))
  }

  io.output <> io.input.toShared()
}

case class Axi4SharedOnChipInitRam(onChipRamBinFile : String, dataWidth : Int, byteCount : BigInt, idWidth : Int, arwStage : Boolean = false) extends Component{
  val axiConfig = configBUS.getAxi4RamConfig(dataWidth,byteCount,idWidth)
  
  val io = new Bundle {
    val axi = slave(Axi4Shared(axiConfig))
  }
  
  import java.nio.file.{Files, Paths}
  val wordCount = byteCount / axiConfig.bytePerWord
  val buffer = ByteBuffer.wrap(Files.readAllBytes(Paths.get(onChipRamBinFile))).order(ByteOrder.LITTLE_ENDIAN);
  val wordArray = (0 until wordCount.toInt).map(i => {
    if(buffer.hasRemaining())
    {
      val v = buffer.getInt
      if(v < 0)  BigInt(v.toLong & 0xFFFFFFFFl) else  BigInt(v)
    } 
    else
    {
      BigInt(0)
    }
  })

  val ram = Mem(axiConfig.dataType,wordCount.toInt) initBigInt(wordArray)
  val wordRange = log2Up(wordCount) + log2Up(axiConfig.bytePerWord)-1 downto log2Up(axiConfig.bytePerWord)

  val arw = if(arwStage) io.axi.arw.s2mPipe().unburstify.m2sPipe() else io.axi.arw.unburstify
  val stage0 = arw.haltWhen(arw.write && !io.axi.writeData.valid)
  io.axi.readRsp.data := ram.readWriteSync(
    address = stage0.addr(axiConfig.wordRange).resized,
    data = io.axi.writeData.data,
    enable = stage0.fire,
    write = stage0.write,
    mask = io.axi.writeData.strb
  )
  
  io.axi.writeData.ready :=  arw.valid && arw.write  && stage0.ready

  val stage1 = stage0.stage()
  stage1.ready := (io.axi.readRsp.ready && !stage1.write) || ((io.axi.writeRsp.ready || ! stage1.last) && stage1.write)

  io.axi.readRsp.valid  := stage1.valid && !stage1.write
  io.axi.readRsp.id  := stage1.id
  io.axi.readRsp.last := stage1.last
  io.axi.readRsp.setOKAY()
  if(axiConfig.useRUser) io.axi.readRsp.user  := stage1.user

  io.axi.writeRsp.valid := stage1.valid &&  stage1.write && stage1.last
  io.axi.writeRsp.setOKAY()
  io.axi.writeRsp.id := stage1.id
  if(axiConfig.useBUser) io.axi.writeRsp.user := stage1.user

  io.axi.arw.ready.noBackendCombMerge //Verilator perf
}

sealed trait jtag_type
object jtag_type {
  case object io            extends jtag_type
  case object xilinx_bscane extends jtag_type
  case object none          extends jtag_type
}

// class CsrMstatush extends Plugin[VexRiscv]{
//   override def build(pipeline: VexRiscv): Unit = {
//     import pipeline._
//     import pipeline.config._
// 
//     pipeline plug new Area{
//       val zero_reg = Reg(UInt(32 bits)) init(0)
//       
//       val csrService = pipeline.service(classOf[CsrInterface])
//       csrService.r(0x310, zero_reg)
//       csrService.onWrite(0x310){
//         zero_reg := 0
//       }
//     }
//   }
// }

case class VeronicaConfig(  jtag_select : jtag_type,
                            ddr_size    : BigInt = 1 GB,
                            ram_size    : BigInt = 16 kB,
                            rom_size    : BigInt = 16 kB,
                            rom_name    : String = "rom/zebbs.bin",
                            cpuPlugins  : ArrayBuffer[Plugin[VexRiscv]])

object VeronicaConfig{

  def default = {
    val config = VeronicaConfig(
      jtag_select = jtag_type.io,
      cpuPlugins = ArrayBuffer(
        new IBusCachedPlugin(
          resetVector = 0x20010000l,
          prediction = STATIC,
          compressedGen = true,
          relaxedPcCalculation = true,
          config = InstructionCacheConfig(
            cacheSize = 4096,
            bytePerLine = 32,
            wayCount = 1,
            addressWidth = 32,
            cpuDataWidth = 32,
            memDataWidth = 32,
            catchIllegalAccess = true,
            catchAccessFault = true,
            asyncTagMemory = true,
            twoCycleRam = false,
            twoCycleCache = false
          )
        ),
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
            catchUnaligned    = true,
            withLrSc          = true,
            withAmo           = true
          )
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
        new StaticMemoryTranslatorPlugin(
          ioRange = (x => x(31 downto 28) === 0x4 || x(31 downto 28) === 0x7 || x(31 downto 24) === 0x0C || x(31 downto 24) === 0x02 || x(31 downto 24) === 0x07)),
        new MstatushPlugin(readOnly = false),
        new CsrPlugin(
          CsrPluginConfig.linuxFull(0x20010020l).copy(misaExtensionsInit = Riscv.misaToInt(s"imacsu"),
                                                      mvendorid = 0,
                                                      marchid = 0,
                                                      mimpid = 0,
                                                      xtvecModeGen = true,
                                                      ebreakGen = true,
                                                      utimeAccess = CsrAccess.READ_ONLY
                                                      )
        ),
        new YamlPlugin("veronica_cpu0.yaml")
      )
    )
    config
  }

  def secure = {
    val config = default

    //Replace static memory translator with pmp translator
    config.cpuPlugins(config.cpuPlugins.indexWhere(_.isInstanceOf[StaticMemoryTranslatorPlugin])) = new PmpPluginNapot(regions = 8,granularity = 8, ioRange = (x => x(31 downto 28) === 0x4 || x(31 downto 28) === 0x7 || x(31 downto 24) === 0x0C || x(31 downto 24) === 0x02 || x(31 downto 24) === 0x07))

    config
  }

  def linux = {
    val config = default

    //Replace static memory translater with MMU plugin
    config.cpuPlugins(config.cpuPlugins.indexWhere(_.isInstanceOf[StaticMemoryTranslatorPlugin])) = new MmuPlugin(ioRange = (x => x(31 downto 28) === 0x4 || x(31 downto 28) === 0x7 || x(31 downto 24) === 0x0C || x(31 downto 24) === 0x02 || x(31 downto 24) === 0x07))

    //Change original ibus with mmu ibus
    config.cpuPlugins(config.cpuPlugins.indexWhere(_.isInstanceOf[IBusCachedPlugin])) =
      new IBusCachedPlugin(
        resetVector = 0x20010000l,
        prediction = STATIC,
        compressedGen = true,
        relaxedPcCalculation = true,
        injectorStage = true,
        config = InstructionCacheConfig(
          cacheSize = 4096,
          bytePerLine = 32,
          wayCount = 1,
          addressWidth = 32,
          cpuDataWidth = 32,
          memDataWidth = 32,
          catchIllegalAccess = true,
          catchAccessFault = true,
          asyncTagMemory = true,
          twoCycleRam = true,
          twoCycleCache = true
        ),
        memoryTranslatorPortConfig = MmuPortConfig(
          portTlbSize = 4,
          latency = 1,
          earlyRequireMmuLockup = true,
          earlyCacheHits = true
        )
      )

    //Change original dbus with mmu dbus
    config.cpuPlugins(config.cpuPlugins.indexWhere(_.isInstanceOf[DBusCachedPlugin])) =
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
          catchUnaligned    = true,
          withLrSc          = true,
          withAmo           = true,
          withWriteAggregation = false
        ),
        dBusCmdMasterPipe = true,
        dBusCmdSlavePipe = true,
        dBusRspSlavePipe = true,
        relaxedMemoryTranslationRegister = true,
        memoryTranslatorPortConfig = MmuPortConfig(
          portTlbSize = 4,
          latency = 1,
          earlyRequireMmuLockup = true,
          earlyCacheHits = true
        )
      )
    config
  }
}

case class Veronica (val config: VeronicaConfig) extends Component {

    import config._
    
    val usb_p = UsbOhciParameter(
      noPowerSwitching = true,
      powerSwitchingMode = true,
      noOverCurrentProtection = true,
      powerOnToPowerGoodTime = 10,
      dataWidth = configBUS.getAxi4Config().dataWidth,
      portsConfig = List.fill(1)(OhciPortParameter())
    )

    val io = new Bundle {
      val cpu_clk   = in Bool()
      val cpu_rst   = in Bool()
      val aclk      = in Bool()
      val arst      = in Bool()
      val debug_rst = ifGen(jtag_select != jtag_type.none)(out Bool())

      val usb_phy_clk   = in Bool()
      val usb_phy_rst   = in Bool()
      
      val ddr_clk   = in Bool()
      val ddr_rst   = in Bool()

      val jtag      = ifGen(jtag_select == jtag_type.io)(slave(Jtag()))

      val irq       = in Bits(127 bits)

      //System Port for memory (IBUS/DBUS)
      val m_axi_mbus = master(Axi4(configBUS.getAxi4Config()))

      //Peripheral Ports for devices (DBUS ONLY)
      val m_axi_acc  = master(AxiLite4(configBUS.getAxiLite4Config()))
      val m_axi_perf = master(AxiLite4(configBUS.getAxiLite4Config()))
      
      //dma0 port for memory access
      val s_axi_dma0 = slave(Axi4(configBUS.getAxi4Config().copy(idWidth = 2)))
      
      //usb phy
      val usb = master(UsbPhyFsNativeIo())
    }

    val resetCtrlClockDomain = ClockDomain(
      clock = io.cpu_clk,
      config = ClockDomainConfig(
        resetKind = BOOT,
        clockEdge = RISING
      )
    )

    val resetCtrl = new ClockingArea(resetCtrlClockDomain) {
      val systemResetUnbuffered = False

      //Implement an counter to keep the reset axiResetOrder high 64 cycles
      // Also this counter will automaticly do a reset when the system boot.
      val systemResetCounter = Reg(UInt(6 bits)) init(0)
      when(systemResetCounter =/= U(systemResetCounter.range -> true)){
        systemResetCounter := systemResetCounter + 1
        systemResetUnbuffered := True
      }

      when(BufferCC(io.cpu_rst)){
        systemResetCounter := 0
      }

      //Create all reset used later in the design
      val srst  = RegNext(systemResetUnbuffered)
      val arst  = RegNext(systemResetUnbuffered)
    }

    val cpuClockDomain = ClockDomain(
      clock = io.cpu_clk,
      reset = resetCtrl.arst,
      config = ClockDomainConfig(
        clockEdge        = RISING,
        resetKind        = spinal.core.SYNC,
        resetActiveLevel = HIGH
      )
    )

    val ddrClockDomain = ClockDomain(
      clock = io.ddr_clk,
      reset = io.ddr_rst,
      config = ClockDomainConfig(
        clockEdge        = RISING,
        resetKind        = spinal.core.SYNC,
        resetActiveLevel = HIGH
      )
    )
    
    val busClockDomain = ClockDomain(
      clock = io.aclk,
      reset = io.arst,
      config = ClockDomainConfig(
        clockEdge        = RISING,
        resetKind        = spinal.core.SYNC,
        resetActiveLevel = HIGH
      )
    )

    val debugClockDomain = ClockDomain(
      clock = io.cpu_clk,
      reset = resetCtrl.srst,
      config = ClockDomainConfig(
        clockEdge        = RISING,
        resetKind        = spinal.core.SYNC,
        resetActiveLevel = HIGH
      )
    )
    
    val phyClockDomain = ClockDomain(
      clock = io.usb_phy_clk,
      reset = io.usb_phy_rst,
      frequency = FixedFrequency(48 MHz),
      config = ClockDomainConfig(
        clockEdge        = RISING,
        resetKind        = spinal.core.SYNC,
        resetActiveLevel = HIGH
      )
    )
    
    val axi4busCC  = Axi4SharedCC(configBUS.getAxi4Config(), cpuClockDomain, busClockDomain, 16, 16, 16, 16)
    
    val axiBUS = new ClockingArea(busClockDomain) {
      val clint = new AxiLite4Clint(1)
      val plic = new AxiLite4Plic(sourceCount = 127, targetCount = 2)
      
      val axi4acc  = Axi4SharedToAxiLite4(configBUS.getAxi4Config())
      val axi4perf = Axi4SharedToAxiLite4(configBUS.getAxi4Config())
      
      val axi4clint = Axi4SharedToAxiLite4(configBUS.getAxi4Config().copy(addressWidth = 16))
      val axi4plic  = Axi4SharedToAxiLite4(configBUS.getAxi4Config().copy(addressWidth = 22))
      
      val rom = Axi4SharedOnChipInitRam(
        onChipRamBinFile = config.rom_name,
        dataWidth = configBUS.getAxi4Config().dataWidth,
        byteCount = config.rom_size,
        idWidth   = configBUS.getAxi4Config().idWidth,
        arwStage  = true
      )

      val ram = Axi4SharedOnChipRam(
        dataWidth = configBUS.getAxi4Config().dataWidth,
        byteCount = config.ram_size,
        idWidth   = configBUS.getAxi4Config().idWidth,
        arwStage  = true
      )
      Axi4ToAxi4SharedV
      val usb = UsbOhciAxi4(usb_p, busClockDomain, phyClockDomain)
      
      val axi4usb     = Axi4SharedToAxi4V(usb.ctrlParameter)
      val axi4usbdma  = Axi4ToAxi4SharedV(configBUS.getAxi4Config().copy(idWidth = 0, useLock = false, useQos = false))
      
      plic.io.sources  := BufferCC(B(usb.io.interrupt, io.irq >> 1))
      
      usb.io.ctrl         <> axi4usb.io.output
      clint.io.bus        <> axi4clint.io.output
      plic.io.bus         <> axi4plic.io.output
      axi4usbdma.io.input <> usb.io.dma
      
      val axiBusCrossbar = Axi4CrossbarFactory()
      
      axiBusCrossbar.addSlaves(
        axi4acc.io.input    -> (0x70000000L,   256 MB),
        axi4perf.io.input   -> (0x40000000L,   256 MB),
        axi4clint.io.input  -> (0x02000000L,    64 kB),
        axi4plic.io.input   -> (0x0C000000L,     4 MB),
        ram.io.axi          -> (0x08000000L,   config.ram_size),
        rom.io.axi          -> (0x20010000L,   config.rom_size),
        axi4usb.io.input    -> (0x07000000L,     4 kB)
      )
      
      axiBusCrossbar.addConnections(
        axi4busCC.io.output -> List(axi4acc.io.input, axi4perf.io.input, axi4clint.io.input, axi4plic.io.input, ram.io.axi, rom.io.axi, axi4usb.io.input)
      )
      
      axiBusCrossbar.build()

    }
    
    val axi4mbusCC  = Axi4SharedCC(configBUS.getAxi4Config().copy(idWidth = 2), cpuClockDomain, ddrClockDomain, 16, 16, 16, 16)
    val axi4usbdmaCC = Axi4SharedCC(configBUS.getAxi4Config().copy(idWidth = 0, useLock = false, useQos = false), busClockDomain, ddrClockDomain, 8, 8, 8, 8)
    
    val axiCPU = new ClockingArea(cpuClockDomain) {
    
      val itim = Axi4SharedOnChipRam(
        dataWidth = configBUS.getAxi4Config().dataWidth,
        byteCount = 1 kB,
        idWidth   = configBUS.getAxi4Config().idWidth,
        arwStage  = false
      )
      
      val core = new Area{

        var cpuConfig : VexRiscvConfig = null

        if(jtag_select != jtag_type.none) {
          cpuPlugins += new DebugPlugin(debugClockDomain)
        }

        cpuConfig = VexRiscvConfig(plugins = cpuPlugins)

        val cpu = new VexRiscv(cpuConfig)

        var iBus : Axi4ReadOnly = null
        var dBus : Axi4Shared = null

        for (plugin <- cpu.plugins) plugin match {
          case plugin: IBusCachedPlugin => {
            iBus = plugin.iBus.toAxi4ReadOnly()
          }
          case plugin: DBusCachedPlugin => {
            dBus = plugin.dBus.toAxi4Shared(true)
          }
          case plugin: DebugPlugin => debugClockDomain {
            resetCtrl.arst setWhen(RegNext(plugin.io.resetOut))
            io.debug_rst <> plugin.io.resetOut
            jtag_select match {
              case jtag_type.io => {
                io.jtag <> plugin.io.bus.fromJtag()
              }
              case jtag_type.xilinx_bscane => {
                val jtagCtrl = JtagTapInstructionCtrl()
                val tap = jtagCtrl.fromXilinxBscane2(userId = 2)
                jtagCtrl <> plugin.io.bus.fromJtagInstructionCtrl(ClockDomain(tap.TCK), 0)
              }
              case _ =>
            }
          }
          case plugin : CsrPlugin => {
            plugin.timerInterrupt             := BufferCC(axiBUS.clint.io.timerInterrupt(0))
            plugin.softwareInterrupt          := BufferCC(axiBUS.clint.io.softwareInterrupt(0))
            plugin.externalInterrupt          := BufferCC(axiBUS.plic.io.targets(0))
            plugin.externalInterruptS         := BufferCC(axiBUS.plic.io.targets(1))
            plugin.utime                      := BufferCC(axiBUS.clint.io.time) //could get corrupted if the clocks are not synchronus
          }
          case _ =>
        }
      }

      val axiCpuCrossbar = Axi4CrossbarFactory()

      axiCpuCrossbar.addSlaves(
        axi4busCC.io.input        -> (0x00800400L,  2039 MB),
        itim.io.axi               -> (0x00800000L,     1 kB),
        axi4mbusCC.io.input       -> (0x80000000L,   config.ddr_size)
      )

      axiCpuCrossbar.addConnections(
        core.iBus -> List(axi4busCC.io.input, axi4mbusCC.io.input, itim.io.axi),
        core.dBus -> List(axi4busCC.io.input, axi4mbusCC.io.input, itim.io.axi)
      )
      
      axiCpuCrossbar.addPipelining(itim.io.axi)((crossbar,ctrl) => {
        crossbar.sharedCmd.halfPipe()  >>  ctrl.sharedCmd
        crossbar.writeData            >/-> ctrl.writeData
        crossbar.writeRsp              <<  ctrl.writeRsp
        crossbar.readRsp               <<  ctrl.readRsp
      })

      axiCpuCrossbar.addPipelining(core.dBus)((cpu,crossbar) => {
        cpu.sharedCmd             >>  crossbar.sharedCmd
        cpu.writeData             >>  crossbar.writeData
        cpu.writeRsp              <<  crossbar.writeRsp
        cpu.readRsp               <-< crossbar.readRsp //Data cache directly use read responses without buffering, so pipeline it for FMax
      })

      axiCpuCrossbar.build()
    }
    
    val axiDDR = new ClockingArea(ddrClockDomain) {
      val axi4mbus = Axi4SharedToAxi4V(configBUS.getAxi4Config())
      val axi4dma0 = Axi4ToAxi4SharedV(configBUS.getAxi4Config().copy(idWidth = 2))
      
      val axiDdrCrossbar = Axi4CrossbarFactory()
      
      axiDdrCrossbar.addSlaves(
        axi4mbus.io.input -> (0x80000000L,   config.ddr_size)
      )
      
      axiDdrCrossbar.addConnections(
        axi4mbusCC.io.output    -> List(axi4mbus.io.input),
        axi4dma0.io.output      -> List(axi4mbus.io.input),
        axi4usbdmaCC.io.output  -> List(axi4mbus.io.input)
      )
      
      axiDdrCrossbar.build()
    }

    AxiLite4SpecRenamer(master(io.m_axi_acc)  .setName("m_axi_acc"))
    AxiLite4SpecRenamer(master(io.m_axi_perf) .setName("m_axi_perf"))

    Axi4SpecRenamer(master(io.m_axi_mbus) .setName("m_axi_mbus"))

    Axi4SpecRenamer(slave(io.s_axi_dma0) .setName("s_axi_dma0"))
    
    axi4usbdmaCC.io.input <> axiBUS.axi4usbdma.io.output
    
    io.m_axi_acc      <> axiBUS.axi4acc.io.output
    io.m_axi_perf     <> axiBUS.axi4perf.io.output
    io.m_axi_mbus     <> axiDDR.axi4mbus.io.output
    io.s_axi_dma0     <> axiDDR.axi4dma0.io.input
    
    io.usb <> axiBUS.usb.io.usb(0)
}

object Veronica_JTAG_Xilinx_Bscane{
  def main(args: Array[String]) {
    Config.spinal.generateVerilog(Veronica(VeronicaConfig.default.copy(jtag_select = jtag_type.xilinx_bscane)))
  }
}

object Veronica_JTAG_IO{
  def main(args: Array[String]) {
    Config.spinal.generateVerilog(Veronica(VeronicaConfig.default))
  }
}

object Veronica{
  def main(args: Array[String]) {
    Config.spinal.generateVerilog(Veronica(VeronicaConfig.default.copy(jtag_select = jtag_type.none)))
  }
}

object Veronica_Secure_JTAG_Xilinx_Bscane{
  def main(args: Array[String]) {
    Config.spinal.generateVerilog(Veronica(VeronicaConfig.secure.copy(jtag_select = jtag_type.xilinx_bscane)))
  }
}

object Veronica_Secure_JTAG_IO{
  def main(args: Array[String]) {
    Config.spinal.generateVerilog(Veronica(VeronicaConfig.secure))
  }
}

object Veronica_Secure{
  def main(args: Array[String]) {
    Config.spinal.generateVerilog(Veronica(VeronicaConfig.secure.copy(jtag_select = jtag_type.none)))
  }
}

object Veronica_Linux_JTAG_Xilinx_Bscane{
  def main(args: Array[String]) {
    Config.spinal.generateVerilog(Veronica(VeronicaConfig.linux.copy(jtag_select = jtag_type.xilinx_bscane)))
  }
}

object Veronica_Linux_JTAG_IO{
  def main(args: Array[String]) {
    Config.spinal.generateVerilog(Veronica(VeronicaConfig.linux))
  }
}

object Veronica_Linux{
  def main(args: Array[String]) {
    Config.spinal.generateVerilog(Veronica(VeronicaConfig.linux.copy(jtag_select = jtag_type.none)))
  }
}

