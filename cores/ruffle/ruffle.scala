package vexriscv.afrl

import spinal.core._
import spinal.lib._
import spinal.lib.bus.amba4.axi.{Axi4ReadOnly, Axi4SpecRenamer}
import spinal.lib.bus.amba4.axilite.AxiLite4SpecRenamer
import spinal.lib.misc.AxiLite4Clint
import spinal.lib.misc.plic.AxiLite4Plic
import spinal.lib.com.jtag.{Jtag, JtagTapInstructionCtrl}
import spinal.lib.eda.altera.{InterruptReceiverTag, ResetEmitterTag}
import spinal.lib.misc.WishboneClint
import spinal.lib.misc.plic.WishbonePlic
import spinal.lib.bus.wishbone.{Wishbone, WishboneConfig}
import vexriscv.ip.{DataCacheConfig, InstructionCacheConfig}
import vexriscv.plugin._
import vexriscv.{Riscv, VexRiscv, VexRiscvConfig, plugin}

sealed trait jtag_type
object jtag_type {
  case object io extends jtag_type
  case object xilinx_bscane extends jtag_type
  case object none extends jtag_type
}

object RuffleBaseConfig{
  def gen(name : String,
          jtag_select : jtag_type) = {
      //CPU configuration
      VexRiscvConfig(
        plugins = List(
          new PcManagerSimplePlugin(0x80000000l, false),
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
          ),
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
          ),
          new MmuPlugin(
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
          ifGen(jtag_select != jtag_type.none)(new DebugPlugin(ClockDomain.current.clone(reset = Bool().setName("debugReset")))),
          new BranchPlugin(
            earlyBranch = false,
            catchAddressMisaligned = true
          ),
//           new CsrPlugin(CsrPluginConfig.linuxMinimal(0x80000020l).copy(ebreakGen = false)),
          new CsrPlugin(CsrPluginConfig.openSbi(mhartid = 0, misa = Riscv.misaToInt(s"ima")).copy(utimeAccess = CsrAccess.READ_ONLY)),
          new YamlPlugin(name + "_cpu0.yaml")
        )
      )
  }
}


object Ruffle{
  def wishbone( name : String,
                jtag_select : jtag_type) = {
    //CPU instanciation
    val cpu = new VexRiscv(RuffleBaseConfig.gen(name, jtag_select)){
        val clintCtrl = new WishboneClint(1)
        val plicCtrl  = new WishbonePlic(sourceCount = 31, targetCount = 2)

        val clint = clintCtrl.io.bus.toIo()
        val plic = plicCtrl.io.bus.toIo()
        val plicInterrupts = in Bits(32 bits)
        plicCtrl.io.sources := plicInterrupts >> 1
    }

    //CPU modifications to be an Avalon one
    cpu.setDefinitionName(name)
    cpu.rework {
      for (plugin <- cpu.plugins) plugin match {
        case plugin: IBusCachedPlugin => {
          plugin.iBus.setAsDirectionLess() //Unset IO properties of iBus
          master(plugin.iBus.toWishbone())
          .setName("wb_cpu_ibus")
          .addTag(ClockDomainTag(ClockDomain.current))
        }
        case plugin: DBusCachedPlugin => {
          plugin.dBus.setAsDirectionLess()
          master(plugin.dBus.toWishbone())
          .setName("wb_cpu_dbus")
          .addTag(ClockDomainTag(ClockDomain.current))
        }
        case plugin: DebugPlugin => plugin.debugClockDomain {
          plugin.io.bus.setAsDirectionLess()
          jtag_select match {
            case jtag_type.io => {
              val jtag = slave(new Jtag()).setName("jtag")
              jtag <> plugin.io.bus.fromJtag()
            }
            case jtag_type.xilinx_bscane => {
              val jtagCtrl = JtagTapInstructionCtrl()
              val tap = jtagCtrl.fromXilinxBscane2(userId = 2)
              jtagCtrl <> plugin.io.bus.fromJtagInstructionCtrl(ClockDomain(tap.TCK), 0)
            }
            case _ =>
          }
        }
        case plugin: CsrPlugin => {
          plugin.timerInterrupt     setAsDirectionLess() := cpu.clintCtrl.io.timerInterrupt(0)
          plugin.softwareInterrupt  setAsDirectionLess() := cpu.clintCtrl.io.softwareInterrupt(0)
          plugin.externalInterrupt  setAsDirectionLess() := cpu.plicCtrl.io.targets(0)
          plugin.externalInterruptS setAsDirectionLess() := cpu.plicCtrl.io.targets(1)
          plugin.utime              setAsDirectionLess() := cpu.clintCtrl.io.time
        }
        case _ =>
      }
    }
    cpu
  }
  def axi( name : String,
           jtag_select : jtag_type) = {
           //   //CPU instanciation
    val cpu = new VexRiscv(RuffleBaseConfig.gen(name, jtag_select)){
        val clintCtrl = new AxiLite4Clint(1, bufferTime = false)
        val plicCtrl  = new AxiLite4Plic(sourceCount = 31, targetCount = 2)

        val clint = clintCtrl.io.bus.toIo()
        val plic = plicCtrl.io.bus.toIo()
        val plicInterrupts = in Bits(32 bits)
        plicCtrl.io.sources := plicInterrupts >> 1

        AxiLite4SpecRenamer(clint .setName("s_axi_clint"))
        AxiLite4SpecRenamer(plic  .setName("s_axi_plic"))
    }

    //CPU modifications to be an Avalon one
    cpu.setDefinitionName(name)
    cpu.rework {
      for (plugin <- cpu.plugins) plugin match {
        case plugin: IBusCachedPlugin => {
          plugin.iBus.setAsDirectionLess() //Unset IO properties of iBus
          Axi4SpecRenamer(
          master(plugin.iBus.toAxi4ReadOnly().toFullConfig())
            .setName("m_axi_ibus")
            .addTag(ClockDomainTag(ClockDomain.current)))
        }
        case plugin: DBusCachedPlugin => {
          plugin.dBus.setAsDirectionLess()
          Axi4SpecRenamer(
          master(plugin.dBus.toAxi4Shared().toAxi4().toFullConfig())
            .setName("m_axi_dbus")
            .addTag(ClockDomainTag(ClockDomain.current)))
        }
        case plugin: DebugPlugin => plugin.debugClockDomain {
          plugin.io.bus.setAsDirectionLess()
          jtag_select match {
            case jtag_type.io => {
              val jtag = slave(new Jtag()).setName("jtag")
              jtag <> plugin.io.bus.fromJtag()
            }
            case jtag_type.xilinx_bscane => {
              val jtagCtrl = JtagTapInstructionCtrl()
              val tap = jtagCtrl.fromXilinxBscane2(userId = 2)
              jtagCtrl <> plugin.io.bus.fromJtagInstructionCtrl(ClockDomain(tap.TCK), 0)
            }
            case _ =>
          }
        }
        case plugin: CsrPlugin => {
          plugin.timerInterrupt     setAsDirectionLess() := cpu.clintCtrl.io.timerInterrupt(0)
          plugin.softwareInterrupt  setAsDirectionLess() := cpu.clintCtrl.io.softwareInterrupt(0)
          plugin.externalInterrupt  setAsDirectionLess() := cpu.plicCtrl.io.targets(0)
          plugin.externalInterruptS setAsDirectionLess() := cpu.plicCtrl.io.targets(1)
          plugin.utime              setAsDirectionLess() := cpu.clintCtrl.io.time
        }
        case _ =>
      }
    }
    cpu
  }
}

object Ruffle_Wishbone_JTAG_IO{
  def main(args: Array[String]) {
    SpinalVerilog(Ruffle.wishbone(name = "ruffle_wishbone_io", jtag_select = jtag_type.io))
  }
}

object Ruffle_Wishbone_Xilinx_Bscane{
  def main(args: Array[String]) {
    SpinalVerilog(Ruffle.wishbone(name = "ruffle_wishbone_bscane", jtag_select = jtag_type.xilinx_bscane))
  }
}

object Ruffle_Axi_Xilinx_Bscane{
  def main(args: Array[String]) {
    SpinalVerilog(Ruffle.axi(name = "ruffle_axi_bscane", jtag_select = jtag_type.xilinx_bscane))
  }
}

object Ruffle_Axi_JTAG_IO{
  def main(args: Array[String]) {
    SpinalVerilog(Ruffle.axi(name = "ruffle_axi_io", jtag_select = jtag_type.io))
  }
}
