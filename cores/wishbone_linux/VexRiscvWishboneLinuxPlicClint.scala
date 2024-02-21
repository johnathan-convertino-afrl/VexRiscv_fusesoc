package vexriscv.afrl

import spinal.core._
import spinal.lib._
import spinal.lib.com.jtag.{Jtag, JtagTapInstructionCtrl}
import spinal.lib.eda.altera.{InterruptReceiverTag, ResetEmitterTag}
import spinal.lib.misc.WishboneClint
import spinal.lib.misc.plic.WishbonePlic
import spinal.lib.bus.wishbone.{Wishbone, WishboneConfig}
import vexriscv.ip.{DataCacheConfig, InstructionCacheConfig}
import vexriscv.plugin._
import vexriscv.{Riscv, VexRiscv, VexRiscvConfig, plugin}

// object DBusSimpleBusWishbone extends DBusSimpleBus {
//   def getWishboneConfig() = WishboneConfig(
//     addressWidth = 32,
//     dataWidth = 32,
//     selWidth = 4,
//     useSTALL = false,
//     useLOCK = false,
//     useERR = true,
//     useRTY = false,
//     tgaWidth = 0,
//     tgcWidth = 0,
//     tgdWidth = 0,
//     useBTE = true,
//     useCTI = true
//   )
// }

object VexRiscvWishboneLinuxPlicClint{
  def main(args: Array[String]) {
    val report = SpinalVerilog{

      //CPU configuration
      val cpuConfig = VexRiscvConfig(
        plugins = List(
          new PcManagerSimplePlugin(0x00000000l, false),
          new IBusCachedPlugin(
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
              bytePerLine       = 32,
              wayCount          = 1,
              addressWidth      = 32,
              cpuDataWidth      = 32,
              memDataWidth      = 32,
              catchAccessError  = true,
              catchIllegal      = true,
              catchUnaligned    = true,
              withLrSc = true,
              withAmo = true
            ),
            dBusCmdMasterPipe = true, //required for wishbone
            memoryTranslatorPortConfig = MmuPortConfig(
              portTlbSize = 4,
              latency = 1,
              earlyRequireMmuLockup = true,
              earlyCacheHits = true
            )
          ),
//           new DBusSimplePlugin(
//             catchAddressMisaligned = false,
//             catchAccessFault = false,
//             earlyInjection = false,
//             withLrSc = true
//           ),
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
          new DebugPlugin(ClockDomain.current.clone(reset = Bool().setName("debugReset"))),
          new BranchPlugin(
            earlyBranch = false,
            catchAddressMisaligned = true
          ),
          new CsrPlugin(CsrPluginConfig.openSbi(mhartid = 0, misa = Riscv.misaToInt(s"ima")).copy(utimeAccess = CsrAccess.READ_ONLY)),
          new YamlPlugin("cpu0.yaml")
        )
      )

      //CPU instanciation
      val cpu = new VexRiscv(cpuConfig){
        val clintCtrl = new WishboneClint(1)
        val plicCtrl = new WishbonePlic(
          sourceCount = 31,
          targetCount = 2
        )

        val clint = clintCtrl.io.bus.toIo()
        val plic = plicCtrl.io.bus.toIo()
        val plicInterrupts = in Bits(32 bits)
        plicCtrl.io.sources := plicInterrupts >> 1
      }

      //CPU modifications to be an Avalon one
      cpu.setDefinitionName("VexRiscvWishboneLinuxPlicClint")
      cpu.rework {
        for (plugin <- cpuConfig.plugins) plugin match {
          case plugin: IBusCachedPlugin => {
            plugin.iBus.setAsDirectionLess() //Unset IO properties of iBus
            master(plugin.iBus.toWishbone()).setName("wb_cpu_ibus")
          }
          case plugin: DBusCachedPlugin => {
            plugin.dBus.setAsDirectionLess()
            master(plugin.dBus.toWishbone()).setName("wb_cpu_dbus")
          }
//           case plugin: DBusSimplePlugin => {
//             plugin.dBus.setAsDirectionLess()
//             slave(cmd)
//           }
          case plugin: DebugPlugin => plugin.debugClockDomain {
            plugin.io.bus.setAsDirectionLess()
            val jtagCtrl = JtagTapInstructionCtrl()
            val tap = jtagCtrl.fromXilinxBscane2(userId = 2)
            jtagCtrl <> plugin.io.bus.fromJtagInstructionCtrl(ClockDomain(tap.TCK),0)
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
}

