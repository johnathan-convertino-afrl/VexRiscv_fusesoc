package vexriscv.afrl

import spinal.core._
import spinal.lib._
import spinal.lib.bus.amba3.apb._
import spinal.lib.misc.HexTools
import spinal.lib.misc._
import spinal.lib.misc.plic._
import spinal.lib.bus.amba4.axi._
import spinal.lib.bus.amba4.axilite._
import spinal.lib.com.jtag.{Jtag, JtagTapInstructionCtrl}
import spinal.lib.eda.altera.{InterruptReceiverTag, ResetEmitterTag}
import spinal.lib.bus.wishbone._
import spinal.lib.io.TriStateArray
import spinal.lib.cpu.riscv.RiscvHart
import spinal.lib.misc.Apb3Clint
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
case class Axi4SharedToAxi4(axiConfig : Axi4Config) extends Component{
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

// class Axi4SharedRom(onChipRamBinFile : String) extends Component{
//   import java.nio.file.{Files, Paths}
//   val byteArray = Files.readAllBytes(Paths.get(onChipRamBinFile))
//   val wordCount = (byteArray.length+3)/4
//   val buffer = ByteBuffer.wrap(Files.readAllBytes(Paths.get(onChipRamBinFile))).order(ByteOrder.LITTLE_ENDIAN);
//   val wordArray = (0 until wordCount).map(i => {
//     val v = buffer.getInt
//     if(v < 0)  BigInt(v.toLong & 0xFFFFFFFFl) else  BigInt(v)
//   })
// 
//   val io = new Bundle{
//     val bus = slave(Apb3(22,configBUS.getAxi4Config().dataWidth))
//   }
//   val rom = Mem(Bits(32 bits), wordCount) initBigInt(wordArray)
// //  io.bus.PRDATA := rom.readSync(io.apb.PADDR >> 2)
//   io.bus.PRDATA := rom.readAsync(RegNext(io.apb.PADDR >> 2))
//   io.bus.PREADY := True
// }

// case class Axi4ReadOnlyCC(axiConfig : Axi4Config,
//                           inputCd : ClockDomain,
//                           outputCd : ClockDomain,
//                           arFifoSize : Int,
//                           rFifoSize : Int) extends Component{
//   val io = new Bundle {
//     val input = slave(Axi4ReadOnly(axiConfig))
//     val output = master(Axi4ReadOnly(axiConfig))
//   }
// 
//   io.output.ar << io.input.ar.queue(arFifoSize, inputCd, outputCd)
//   io.input.r   << io.output.r.queue(rFifoSize, outputCd, inputCd)
// }

// need to remove write connections
// class Axi4SharedOnChipRom(dataWidth : Int, byteCount : BigInt, idWidth : Int, arwStage : Boolean = false, onChipRamBinFile : String) extends Component{
//   val axiConfig = Axi4SharedOnChipRam.getAxiConfig(dataWidth,byteCount,idWidth)
// 
//   import java.nio.file.{Files, Paths}
//   val byteArray = Files.readAllBytes(Paths.get(onChipRamBinFile))
//   val wordCount = (byteArray.length+3)/4
//   val buffer = ByteBuffer.wrap(Files.readAllBytes(Paths.get(onChipRamBinFile))).order(ByteOrder.LITTLE_ENDIAN);
//   val wordArray = (0 until wordCount).map(i => {
//     val v = buffer.getInt
//     if(v < 0)  BigInt(v.toLong & 0xFFFFFFFFl) else  BigInt(v)
//   })
//   
//   val io = new Bundle {
//     val axi = slave(Axi4Shared(axiConfig))
//   }
// 
//   val wordCount = byteCount / axiConfig.bytePerWord
// //   val ram = Mem(axiConfig.dataType,wordCount.toInt)
//   val rom = Mem(axiConfig.dataType, wordCount.toInt) initBigInt(wordArray)
//   val wordRange = log2Up(wordCount) + log2Up(axiConfig.bytePerWord)-1 downto log2Up(axiConfig.bytePerWord)
// 
//   val arw = if(arwStage) io.axi.arw.s2mPipe().unburstify.m2sPipe() else io.axi.arw.unburstify
//   val stage0 = arw.haltWhen(arw.write && !io.axi.writeData.valid)
//   io.axi.readRsp.data := ram.readWriteSync(
//     address = stage0.addr(axiConfig.wordRange).resized,
//     data = io.axi.writeData.data,
//     enable = stage0.fire,
//     write = stage0.write,
//     mask = io.axi.writeData.strb
//   )
//   io.axi.writeData.ready :=  arw.valid && arw.write  && stage0.ready
// 
//   val stage1 = stage0.stage()
//   stage1.ready := (io.axi.readRsp.ready && !stage1.write) || ((io.axi.writeRsp.ready || ! stage1.last) && stage1.write)
// 
//   io.axi.readRsp.valid  := stage1.valid && !stage1.write
//   io.axi.readRsp.id  := stage1.id
//   io.axi.readRsp.last := stage1.last
//   io.axi.readRsp.setOKAY()
//   if(axiConfig.useRUser) io.axi.readRsp.user  := stage1.user
// 
//   io.axi.writeRsp.valid := stage1.valid &&  stage1.write && stage1.last
//   io.axi.writeRsp.setSLVERR()
//   io.axi.writeRsp.id := stage1.id
//   if(axiConfig.useBUser) io.axi.writeRsp.user := stage1.user
// 
//   io.axi.arw.ready.noBackendCombMerge //Verilator perf
// }

sealed trait jtag_type
object jtag_type {
  case object io            extends jtag_type
  case object xilinx_bscane extends jtag_type
  case object none          extends jtag_type
}

case class VeronicaConfig(  jtag_select : jtag_type,
                            ddr_size    : BigInt = 1 GB,
                            ram_size    : BigInt = 16 kB,
                            rom_size    : BigInt = 16 kB,
                            rom_name    : String = "zebbs.bin",
                            cpuPlugins  : ArrayBuffer[Plugin[VexRiscv]])

object VeronicaConfig{

  def default = {
    val config = VeronicaConfig(
      jtag_select = jtag_type.io,
      cpuPlugins = ArrayBuffer(
        new IBusCachedPlugin(
          resetVector = 0x80000000l,
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
          ioRange = (x => x(31 downto 28) === 0x4 || x(31 downto 28) === 0x7 || x(31 downto 28) === 0x0)
        ),
        new CsrPlugin(
          CsrPluginConfig.openSbi(
            mhartid = 0,
            misa = Riscv.misaToInt(s"imac")).copy(mtvecInit = 0x80000020l, xtvecModeGen = true, utimeAccess = CsrAccess.READ_ONLY)
        ),
        new YamlPlugin("veronica_cpu0.yaml")
      )
    )
    config
  }

  def secure = {
    val config = default

    //Replace static memory translator with pmp translator
    config.cpuPlugins(config.cpuPlugins.indexWhere(_.isInstanceOf[StaticMemoryTranslatorPlugin])) = new PmpPluginNapot(regions = 8,granularity = 8,ioRange = (x => x(31 downto 28) === 0x4 || x(31 downto 28) === 0x7 || x(31 downto 28) === 0x0))

    //Replace standard CSR with secure CSR.
//     config.cpuPlugins(config.cpuPlugins.indexWhere(_.isInstanceOf[CsrPlugin])) =
//       new CsrPlugin(
//           config = CsrPluginConfig(
//             catchIllegalAccess  = true,
//             mvendorid           = 0,
//             marchid             = 0,
//             mimpid              = 0,
//             mhartid             = 0,
//             misaExtensionsInit  = Riscv.misaToInt(s"imac"),
//             misaAccess          = CsrAccess.READ_WRITE,
//             mtvecAccess         = CsrAccess.READ_WRITE,
//             xtvecModeGen        = true,
//             mtvecInit           = 0x80000020l,
//             mepcAccess          = CsrAccess.READ_WRITE,
//             mscratchGen         = true,
//             mcauseAccess        = CsrAccess.READ_WRITE,
//             mbadaddrAccess      = CsrAccess.READ_WRITE,
//             mcycleAccess        = CsrAccess.READ_WRITE,
//             minstretAccess      = CsrAccess.READ_WRITE,
//             ucycleAccess        = CsrAccess.READ_ONLY,
//             uinstretAccess      = CsrAccess.READ_ONLY,
//             wfiGenAsWait        = true,
//             ecallGen            = true,
//             userGen             = true,
//             medelegAccess       = CsrAccess.READ_WRITE,
//             midelegAccess       = CsrAccess.READ_WRITE,
//             utimeAccess         = CsrAccess.READ_ONLY
//           )
//         )

    config
  }

  def linux = {
    val config = default

    //Replace static memory translater with MMU plugin
    config.cpuPlugins(config.cpuPlugins.indexWhere(_.isInstanceOf[StaticMemoryTranslatorPlugin])) = new MmuPlugin(ioRange = (x => x(31 downto 28) === 0x4 || x(31 downto 28) === 0x7 || x(31 downto 28) === 0x0))

    //Change original ibus with mmu ibus
    config.cpuPlugins(config.cpuPlugins.indexWhere(_.isInstanceOf[IBusCachedPlugin])) =
      new IBusCachedPlugin(
        resetVector = 0x80000000l,
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

    // floating point unit wthout double support
    // config.cpuPlugins += new FpuPlugin(externalFpu = false,p = FpuParameter(withDouble = false))

    config
  }
}

case class Veronica (val config: VeronicaConfig) extends Component {

    import config._

    val io = new Bundle {
      val cpu_clk   = in Bool()
      val cpu_rst   = in Bool()
      val aclk      = in Bool()
      val arst      = in Bool()
      val debug_rst = ifGen(jtag_select != jtag_type.none)(out Bool())

      val ddr_clk   = in Bool()
      val ddr_rst   = in Bool()

      val jtag      = ifGen(jtag_select == jtag_type.io)(slave(Jtag()))

      val irq       = in Bits(128 bits)

      //System Port for memory (IBUS/DBUS)
      val m_axi_mbus = master(Axi4(configBUS.getAxi4Config()))

      //Peripheral Ports for devices (DBUS ONLY)
      val m_axi_acc  = master(AxiLite4(configBUS.getAxiLite4Config()))
      val m_axi_perf = master(AxiLite4(configBUS.getAxiLite4Config()))
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
    
    val axiDDR = new ClockingArea(ddrClockDomain) {
      val axi4mbus = Axi4SharedToAxi4(configBUS.getAxi4Config())
    }
    
    val axiBUS = new ClockingArea(busClockDomain) {
      val axi4acc  = Axi4SharedToAxiLite4(configBUS.getAxi4Config())
      val axi4perf = Axi4SharedToAxiLite4(configBUS.getAxi4Config())
      
      val axi4clint = Axi4SharedToAxi4(configBUS.getAxi4Config())
      val axi4plic  = Axi4SharedToAxi4(configBUS.getAxi4Config())
      
      val clint = new Axi4Clint(1)
      val plic = new Axi4Plic(sourceCount = 127, targetCount = 2)
      
      plic.io.sources  := BufferCC(io.irq >> 1)
      
      val ram = Axi4SharedOnChipRam(
        dataWidth = configBUS.getAxi4Config().dataWidth,
        byteCount = config.ram_size,
        idWidth   = configBUS.getAxi4Config().idWidth,
        arwStage  = true
      )
      
//       val rom = Axi4SharedOnChipRom(
//         dataWidth = configBUS.getAxi4Config().dataWidth,
//         byteCount = config.rom_size,
//         idWidth   = configBUS.getAxi4Config().idWidth,
//         arwStage  = true,
//         config.rom_name
//       )
    }
    
    val axi4perfCC  = Axi4SharedCC(configBUS.getAxi4Config(), cpuClockDomain, busClockDomain, 8, 8, 8, 8)
    val axi4accCC   = Axi4SharedCC(configBUS.getAxi4Config(), cpuClockDomain, busClockDomain, 8, 8, 8, 8)
    val axi4ramCC   = Axi4SharedCC(axiBUS.ram.axiConfig, cpuClockDomain, busClockDomain, 8, 8, 8, 8)
    val axi4mbusCC  = Axi4SharedCC(configBUS.getAxi4Config(), cpuClockDomain, ddrClockDomain, 8, 8, 8, 8)
    val axi4clintCC = Axi4SharedCC(configBUS.getAxi4Config(), cpuClockDomain, busClockDomain, 8, 8, 8, 8)
    val axi4plicCC  = Axi4SharedCC(configBUS.getAxi4Config(), cpuClockDomain, busClockDomain, 8, 8, 8, 8)
    
    val axiCPU = new ClockingArea(cpuClockDomain) {
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

      val axiCrossbar = Axi4CrossbarFactory()

//       rom.io.axi          -> (0x20010000L,   config.rom_size) //16 KiB
      axiCrossbar.addSlaves(
        axi4accCC.io.input        -> (0x70000000L,   256 MB),
        axi4perfCC.io.input       -> (0x40000000L,   256 MB),
        axi4mbusCC.io.input       -> (0x90000000L,   config.ddr_size), //0x80000000L
        axi4clintCC.io.input      -> (0x02000000L,    64 kB),
        axi4plicCC.io.input       -> (0x0C000000L,     4 MB),
        axi4ramCC.io.input        -> (0x80000000L,   config.ram_size) //0x01800000L PER E31 ITIM //0x08000000L PER E310?
      )

//       rom.io.axi
      axiCrossbar.addConnections(
        core.iBus -> List(axi4ramCC.io.input, axi4mbusCC.io.input),
        core.dBus -> List(axi4ramCC.io.input, axi4mbusCC.io.input, axi4clintCC.io.input, axi4plicCC.io.input, axi4accCC.io.input, axi4perfCC.io.input) //apbBridge.io.axi,
      )

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
    
    axiBUS.axi4clint.io.input <> axi4clintCC.io.output
    axiBUS.axi4plic.io.input  <> axi4plicCC.io.output
    
    axiBUS.clint.io.bus   <> axiBUS.axi4clint.io.output
    axiBUS.plic.io.bus    <> axiBUS.axi4plic.io.output
    axiBUS.ram.io.axi     <> axi4ramCC.io.output
    
    axiBUS.axi4acc.io.input  <> axi4accCC.io.output
    axiBUS.axi4perf.io.input <> axi4perfCC.io.output
    
    axiDDR.axi4mbus.io.input <> axi4mbusCC.io.output
    
    io.m_axi_acc      <> axiBUS.axi4acc.io.output
    io.m_axi_perf     <> axiBUS.axi4perf.io.output
    io.m_axi_mbus     <> axiDDR.axi4mbus.io.output
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

