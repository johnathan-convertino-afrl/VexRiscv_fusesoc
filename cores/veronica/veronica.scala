package vexriscv.afrl

import spinal.core._
import spinal.lib._
import spinal.lib.misc.Clint
import spinal.lib.misc.HexTools
import spinal.lib.misc.plic._
import spinal.lib.bus.amba4.axi._
import spinal.lib.bus.amba4.axilite._
import spinal.lib.com.jtag.{Jtag, JtagTapInstructionCtrl}
import spinal.lib.eda.altera.{InterruptReceiverTag, ResetEmitterTag}
import spinal.lib.bus.wishbone._
import spinal.lib.io.TriStateArray
import spinal.lib.cpu.riscv.RiscvHart
import vexriscv.ip.{DataCacheConfig, InstructionCacheConfig}
import vexriscv.plugin._
import vexriscv.{Riscv, VexRiscv, VexRiscvConfig, plugin}
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
    useRegion = true,
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

//axi connection
case class Axi4Output(axiConfig : Axi4Config) extends Component{
  val io = new Bundle {
    val input  = slave(Axi4Shared(axiConfig))
    val output = master(Axi4(axiConfig))
  }

  io.output <> io.input.toAxi4()
}

case class Axi4Clint(hartCount : Int, bufferTime : Boolean = false) extends Component{
  val io = new Bundle {
    val bus = slave(Axi4(configBUS.getAxi4Config()))
    val timerInterrupt = out Bits(hartCount bits)
    val softwareInterrupt = out Bits(hartCount bits)
    val time = out UInt(64 bits)
  }

  val factory = new Axi4SlaveFactory(io.bus)
  val logic = Clint(0 until hartCount)
  logic.driveFrom(factory, bufferTime)

  for(hartId <- 0 until hartCount){
    io.timerInterrupt(hartId) := logic.harts(hartId).timerInterrupt
    io.softwareInterrupt(hartId) := logic.harts(hartId).softwareInterrupt
  }

  io.time := logic.time
}


class Axi4Plic(sourceCount : Int, targetCount : Int) extends Component{
  val priorityWidth = 2
  val plicMapping = PlicMapping.sifive
  import plicMapping._

  val io = new Bundle {
    val bus = slave(Axi4(configBUS.getAxi4Config()))
    val sources = in Bits (sourceCount bits)
    val targets = out Bits (targetCount bits)
  }

  val gateways = (for ((source, id) <- (io.sources.asBools, 1 to sourceCount).zipped) yield PlicGatewayActiveHigh(
    source = source,
    id = id,
    priorityWidth = priorityWidth
  )).toSeq

  val targets = for (i <- 0 until targetCount) yield PlicTarget(
    id = i,
    gateways = gateways,
    priorityWidth = priorityWidth
  )

  io.targets := targets.map(_.iep).asBits

  val bus = new Axi4SlaveFactory(io.bus)
  val mapping = PlicMapper(bus, plicMapping)(
    gateways = gateways,
    targets = targets
  )
}

sealed trait jtag_type
object jtag_type {
  case object io            extends jtag_type
  case object xilinx_bscane extends jtag_type
  case object none          extends jtag_type
}

case class VeronicaConfig(  jtag_select : jtag_type,
                            ram_size    : BigInt = 8 kB,
                            cpuPlugins  : ArrayBuffer[Plugin[VexRiscv]])

object VeronicaConfig{

  def default = {
    val config = VeronicaConfig(
      jtag_select = jtag_type.io,
      cpuPlugins = ArrayBuffer(
        new IBusCachedPlugin(
          resetVector = 0x80000000l,
          prediction = STATIC,
          config = InstructionCacheConfig(
            cacheSize = 4096,
            bytePerLine = 32,
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
            catchUnaligned    = true
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
          ioRange = _(31 downto 28) === 0xF
        ),
        new CsrPlugin(
          config = CsrPluginConfig(
            catchIllegalAccess  = false,
            mvendorid           = 1,
            marchid             = 2,
            mimpid              = 3,
            mhartid             = 0,
            misaExtensionsInit  = 0x101064, // RV32GCFMU???????
            misaAccess          = CsrAccess.READ_WRITE,
            mtvecAccess         = CsrAccess.READ_WRITE,
            mtvecInit           = 0x80000020l,
            mepcAccess          = CsrAccess.READ_WRITE,
            mscratchGen         = true,
            mcauseAccess        = CsrAccess.READ_WRITE,
            mbadaddrAccess      = CsrAccess.READ_WRITE,
            mcycleAccess        = CsrAccess.READ_WRITE,
            minstretAccess      = CsrAccess.READ_WRITE,
            ucycleAccess        = CsrAccess.READ_ONLY,
            uinstretAccess      = CsrAccess.READ_ONLY,
            wfiGenAsWait        = true,
            ecallGen            = true,
            userGen             = true,
            medelegAccess       = CsrAccess.READ_WRITE,
            midelegAccess       = CsrAccess.READ_WRITE,
            utimeAccess         = CsrAccess.READ_ONLY
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
    config.cpuPlugins(config.cpuPlugins.indexWhere(_.isInstanceOf[StaticMemoryTranslatorPlugin])) = new PmpPluginNapot(regions = 16,granularity = 8,ioRange = _(31 downto 28) === 0xf)

    //Replace standard CSR with secure CSR.
    config.cpuPlugins(config.cpuPlugins.indexWhere(_.isInstanceOf[CsrPlugin])) =
      new CsrPlugin(
          config = CsrPluginConfig(
            catchIllegalAccess  = true,
            mvendorid           = 1,
            marchid             = 2,
            mimpid              = 3,
            mhartid             = 0,
            misaExtensionsInit  = 0x101064, // RV32GCFMU???????
            misaAccess          = CsrAccess.READ_WRITE,
            mtvecAccess         = CsrAccess.READ_WRITE,
            mtvecInit           = 0x80000020l,
            mepcAccess          = CsrAccess.READ_WRITE,
            mscratchGen         = true,
            mcauseAccess        = CsrAccess.READ_WRITE,
            mbadaddrAccess      = CsrAccess.READ_WRITE,
            mcycleAccess        = CsrAccess.READ_WRITE,
            minstretAccess      = CsrAccess.READ_WRITE,
            ucycleAccess        = CsrAccess.READ_ONLY,
            uinstretAccess      = CsrAccess.READ_ONLY,
            wfiGenAsWait        = true,
            ecallGen            = true,
            userGen             = true,
            medelegAccess       = CsrAccess.READ_WRITE,
            midelegAccess       = CsrAccess.READ_WRITE,
            utimeAccess         = CsrAccess.READ_ONLY
          )
        )

    config
  }

  def linux = {
    val config = default

    //Replace static memory translater with MMU plugin
    config.cpuPlugins(config.cpuPlugins.indexWhere(_.isInstanceOf[StaticMemoryTranslatorPlugin])) = new MmuPlugin(ioRange = _(31 downto 28) === 0xF)

    //Replace standard CSR with linux CSR.
    config.cpuPlugins(config.cpuPlugins.indexWhere(_.isInstanceOf[CsrPlugin])) = new CsrPlugin(CsrPluginConfig.openSbi(mhartid = 0, misa = Riscv.misaToInt(s"imaf")).copy(utimeAccess = CsrAccess.READ_ONLY))

    //Change original ibus with mmu ibus
    config.cpuPlugins(config.cpuPlugins.indexWhere(_.isInstanceOf[IBusCachedPlugin])) =
      new IBusCachedPlugin(
        resetVector = 0x80000000l,
        prediction = STATIC,
        config = InstructionCacheConfig(
          cacheSize = 4096,
          bytePerLine = 64,
          wayCount = 1,
          addressWidth = 32,
          cpuDataWidth = 32,
          memDataWidth = 32,
          catchIllegalAccess = true,
          catchAccessFault = true,
          asyncTagMemory = false,
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
          bytePerLine       = 64,
          wayCount          = 1,
          addressWidth      = 32,
          cpuDataWidth      = 32,
          memDataWidth      = 32,
          catchAccessError  = true,
          catchIllegal      = true,
          catchUnaligned    = true,
          withLrSc = true,
          withAmo = true,
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

    // floating point unit wthout double support
    config.cpuPlugins += new FpuPlugin(externalFpu = false,p = FpuParameter(withDouble = false))

    config
  }
}

case class Veronica (val config: VeronicaConfig) extends Component {

    import config._

    val io = new Bundle {
      val aclk  = in Bool()
      val arst  = in Bool()
      val debug_rst = ifGen(jtag_select != jtag_type.none)(out Bool())

      val ddr_clk  = in Bool()
      val ddr_rst  = in Bool()

      val jtag  = ifGen(jtag_select == jtag_type.io)(slave(Jtag()))

      val irq       = in Bits(32 bits)
      val timer_irq = in Bool()

      val m_axi_mbus = master(Axi4(configBUS.getAxi4Config()))

      val m_axi_acc  = master(AxiLite4(configBUS.getAxiLite4Config()))
      val m_axi_perf = master(AxiLite4(configBUS.getAxiLite4Config()))
    }

    val resetCtrlClockDomain = ClockDomain(
      clock = io.aclk,
      config = ClockDomainConfig(
        resetKind        = BOOT,
        clockEdge        = RISING
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

      when(BufferCC(io.arst)){
        systemResetCounter := 0
      }

      //Create all reset used later in the design
      val srst  = RegNext(systemResetUnbuffered)
      val arst  = RegNext(systemResetUnbuffered)
    }

    val busClockDomain = ClockDomain(
      clock = io.aclk,
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

    val debugClockDomain = ClockDomain(
      clock = io.aclk,
      reset = resetCtrl.srst,
      config = ClockDomainConfig(
        clockEdge        = RISING,
        resetKind        = spinal.core.SYNC,
        resetActiveLevel = HIGH
      )
    )

    val axi = new ClockingArea(busClockDomain) {
      val ram = Axi4SharedOnChipRam(
        dataWidth = configBUS.getAxi4Config().dataWidth,
        byteCount = config.ram_size,
        idWidth = configBUS.getAxi4Config().idWidth,
        arwStage = true
      )
 
      val axi4acc  = AxiLite4Output(configBUS.getAxi4Config())
      val axi4perf = AxiLite4Output(configBUS.getAxi4Config())

      val axi4mbus = Axi4CC(configBUS.getAxi4Config(), busClockDomain, ddrClockDomain, 32, 32, 32, 32, 32)

      var clintCtrl : Axi4Clint = null
      var plicCtrl  : Axi4Plic  = null

      val core = new Area{

        var cpuConfig : VexRiscvConfig = null

        if(jtag_select != jtag_type.none) {
          cpuPlugins += new DebugPlugin(debugClockDomain)
        }

        cpuConfig = VexRiscvConfig(plugins = cpuPlugins)

        val cpu = new VexRiscv(cpuConfig)

        var iBus : Axi4ReadOnly = null
        var dBus : Axi4Shared = null

        val linux = cpu.plugins.exists(_.isInstanceOf[MmuPlugin])

        clintCtrl = new Axi4Clint(1)

        if(linux) {
          plicCtrl  = new Axi4Plic(sourceCount = 32, targetCount = 2)
        }
        else {
          plicCtrl  = new Axi4Plic(sourceCount = 32, targetCount = 1)
        }

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
            plugin.timerInterrupt     := clintCtrl.io.timerInterrupt(0)
            plugin.softwareInterrupt  := clintCtrl.io.softwareInterrupt(0)
            plugin.externalInterrupt  := plicCtrl.io.targets(0)
            if(linux) {
              plugin.externalInterruptS := plicCtrl.io.targets(1)
            }
            plugin.utime              := clintCtrl.io.time
            io.irq                    <> plicCtrl.io.sources
          }
          case _ =>
        }
      }

      val axiCrossbar = Axi4CrossbarFactory()

      axiCrossbar.addSlaves(
        ram.io.axi          -> (0x80000000L,   config.ram_size),
        axi4acc.io.input    -> (0x70000000L,   256 MB),
        axi4perf.io.input   -> (0x40000000L,   256 MB),
        axi4mbus.io.input   -> (0x90000000L,     1 GB),
        clintCtrl.io.bus    -> (0x02000000L,    48 kB),
        plicCtrl.io.bus     -> (0x0C000000L,     4 MB)
      )

      axiCrossbar.addConnections(
        core.iBus -> List(ram.io.axi, axi4mbus.io.input),
        core.dBus -> List(ram.io.axi, clintCtrl.io.bus, plicCtrl.io.bus, axi4acc.io.input, axi4perf.io.input, axi4mbus.io.input)
      )

      axiCrossbar.addPipelining(ram.io.axi)((crossbar,ctrl) => {
        crossbar.sharedCmd.halfPipe()  >>  ctrl.sharedCmd
        crossbar.writeData            >/-> ctrl.writeData
        crossbar.writeRsp              <<  ctrl.writeRsp
        crossbar.readRsp               <<  ctrl.readRsp
      })

      axiCrossbar.addPipelining(core.dBus)((cpu,crossbar) => {
        cpu.sharedCmd             >>  crossbar.sharedCmd
        cpu.writeData             >>  crossbar.writeData
        cpu.writeRsp              <<  crossbar.writeRsp
        cpu.readRsp               <-< crossbar.readRsp //Data cache directly use read responses without buffering, so pipeline it for FMax
      })

      axiCrossbar.build()
    }

    AxiLite4SpecRenamer(master(io.m_axi_acc)  .setName("m_axi_acc"))
    AxiLite4SpecRenamer(master(io.m_axi_perf) .setName("m_axi_perf"))

    Axi4SpecRenamer(master(io.m_axi_mbus) .setName("m_axi_mbus"))

    io.m_axi_acc      <> axi.axi4acc.io.output
    io.m_axi_perf     <> axi.axi4perf.io.output
    io.m_axi_mbus     <> axi.axi4mbus.io.output
}

object Veronica_Axi_JTAG_Xilinx_Bscane{
  def main(args: Array[String]) {
    Config.spinal.generateVerilog(Veronica(VeronicaConfig.default.copy(jtag_select = jtag_type.xilinx_bscane)))
  }
}

object Veronica_Axi_JTAG_IO{
  def main(args: Array[String]) {
    Config.spinal.generateVerilog(Veronica(VeronicaConfig.default))
  }
}

object Veronica_Axi{
  def main(args: Array[String]) {
    Config.spinal.generateVerilog(Veronica(VeronicaConfig.default.copy(jtag_select = jtag_type.none)))
  }
}

object Veronica_Axi_Secure_JTAG_Xilinx_Bscane{
  def main(args: Array[String]) {
    Config.spinal.generateVerilog(Veronica(VeronicaConfig.secure.copy(jtag_select = jtag_type.xilinx_bscane)))
  }
}

object Veronica_Axi_Secure_JTAG_IO{
  def main(args: Array[String]) {
    Config.spinal.generateVerilog(Veronica(VeronicaConfig.secure))
  }
}

object Veronica_Axi_Secure{
  def main(args: Array[String]) {
    Config.spinal.generateVerilog(Veronica(VeronicaConfig.secure.copy(jtag_select = jtag_type.none)))
  }
}

object Veronica_Axi_Linux_JTAG_Xilinx_Bscane{
  def main(args: Array[String]) {
    Config.spinal.generateVerilog(Veronica(VeronicaConfig.linux.copy(jtag_select = jtag_type.xilinx_bscane)))
  }
}

object Veronica_Axi_Linux_JTAG_IO{
  def main(args: Array[String]) {
    Config.spinal.generateVerilog(Veronica(VeronicaConfig.linux))
  }
}

object Veronica_Axi_Linux{
  def main(args: Array[String]) {
    Config.spinal.generateVerilog(Veronica(VeronicaConfig.linux.copy(jtag_select = jtag_type.none)))
  }
}

