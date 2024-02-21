package vexriscv.afrl

import spinal.core._
import spinal.lib._
import spinal.lib.bus.amba4.axi.{Axi4ReadOnly, Axi4SpecRenamer}
import spinal.lib.bus.amba4.axilite.AxiLite4SpecRenamer
import spinal.lib.com.jtag.{Jtag, JtagTapInstructionCtrl}
import spinal.lib.eda.altera.{InterruptReceiverTag, ResetEmitterTag}
import spinal.lib.misc.AxiLite4Clint
import spinal.lib.misc.plic.AxiLite4Plic
import vexriscv.ip.{DataCacheConfig, InstructionCacheConfig}
import vexriscv.ip._
import vexriscv.plugin._
import vexriscv.{Riscv, VexRiscv, VexRiscvConfig, plugin}


//make clean run IBUS=CACHED DBUS=CACHED DEBUG_PLUGIN=STD DHRYSTONE=yes SUPERVISOR=yes MMU=no CSR=yes CSR_SKIP_TEST=yes  COMPRESSED=no MUL=yes DIV=yes LRSC=yes AMO=yes REDO=0 TRACE=no COREMARK=no LINUX_REGRESSION=yes
object VexRiscvAxi4LinuxPlicClint{
  def main(args: Array[String]) {
    val report = SpinalVerilog{

      //CPU configuration
      val cpuConfig = VexRiscvConfig(
        plugins = List(
//           new PcManagerSimplePlugin(0x00000000l, false),
          new IBusCachedPlugin(
            compressedGen = false,
            prediction = STATIC,
            resetVector = 0x80000000l,
            injectorStage = false,
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
              twoCycleRam = false,
              twoCycleCache = true
            ),
            memoryTranslatorPortConfig = MmuPortConfig(
              portTlbSize = 4,
              latency = 1,
              earlyRequireMmuLockup = true,
              earlyCacheHits = true
            )
          ),
//           new IBusSimplePlugin(
//             resetVector = 0x80000000l,
//             cmdForkOnSecondStage = false,
//             cmdForkPersistence = true,
//             prediction = DYNAMIC_TARGET,
//             historyRamSizeLog2 = 10,
//             catchAccessFault = true,
//             compressedGen = true,
//             busLatencyMin = 1,
//             injectorStage = true,
//             memoryTranslatorPortConfig = MmuPortConfig(
//               portTlbSize = 4
//             )
//           ),
          new DBusCachedPlugin(
            relaxedMemoryTranslationRegister = true,
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
              withExclusive     = false,
              withInvalidate    = false,
              withLrSc = true,
              withAmo = true
            ),
            memoryTranslatorPortConfig = MmuPortConfig(
              portTlbSize = 4,
              latency = 1,
              earlyRequireMmuLockup = true,
              earlyCacheHits = true
            )
          ),
//           new DBusSimplePlugin(
//             catchAddressMisaligned = true,
//             catchAccessFault = true,
//             earlyInjection = false,
//             withLrSc = true,
//             memoryTranslatorPortConfig = MmuPortConfig(
//               portTlbSize = 4
//             )
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
        val clintCtrl = new AxiLite4Clint(1, bufferTime = false)
        val plicCtrl = new AxiLite4Plic(
          sourceCount = 31,
          targetCount = 2
        )

        val clint = clintCtrl.io.bus.toIo()
        val plic = plicCtrl.io.bus.toIo()
        val plicInterrupts = in Bits(32 bits)
        plicCtrl.io.sources := plicInterrupts >> 1

        AxiLite4SpecRenamer(
          clint
          .setName("s_axi_clint")
         )
        AxiLite4SpecRenamer(
          plic
          .setName("s_axi_plic")
        )
      }

      //CPU modifications to be an Avalon one
      cpu.setDefinitionName("VexRiscvAxi4LinuxPlicClint")
      cpu.rework {
        for (plugin <- cpuConfig.plugins) plugin match {
          case plugin: IBusCachedPlugin => {
//           case plugin: IBusSimplePlugin => {
            plugin.iBus.setAsDirectionLess() //Unset IO properties of iBus
            Axi4SpecRenamer(
              master(plugin.iBus.toAxi4ReadOnly().toFullConfig())
                .setName("m_axi_ibus")
                .addTag(ClockDomainTag(ClockDomain.current)) //Specify a clock domain to the iBus (used by QSysify)
            )
          }
          case plugin: DBusCachedPlugin => {
//           case plugin: DBusSimplePlugin => {
            plugin.dBus.setAsDirectionLess()
            Axi4SpecRenamer(
              master(plugin.dBus.toAxi4Shared().toAxi4().toFullConfig())
                .setName("m_axi_dbus")
                .addTag(ClockDomainTag(ClockDomain.current))
            )
          }
          case plugin: DebugPlugin => plugin.debugClockDomain {
            plugin.io.bus.setAsDirectionLess()
            val jtagCtrl = JtagTapInstructionCtrl()
            val tap = jtagCtrl.fromXilinxBscane2(userId = 2)
            jtagCtrl <> plugin.io.bus.fromJtagInstructionCtrl(ClockDomain(tap.TCK),0)

//             plugin.io.bus.setAsDirectionLess()
//             val jtag = slave(new Jtag())
//               .setName("jtag")
//             jtag <> plugin.io.bus.fromJtag()
//             plugin.io.resetOut
//               .addTag(ResetEmitterTag(plugin.debugClockDomain))
//               .parent = null //Avoid the io bundle to be interpreted as a QSys conduit
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

