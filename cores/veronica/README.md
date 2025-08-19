# Veronica Axi Vexriscv Source
### Source files for Veronica AXI4 RISCV core
---

   author: Jay Convertino  
   
   details: Veronica Axi RISCV32IMAC
   
   license: MIT  
   
---

### Version
#### Current
  - V1.0.0 - initial release

#### Previous
  - none

### Dependencies
  - VexRiscv, github repo
  - SpinalHDL, github repo

### IP available
  - Veronica_Axi_JTAG_Xilinx_Bscane
  - Veronica_Axi_JTAG_IO
  - Veronica_Axi
  - Veronica_Axi_Secure_JTAG_Xilinx_Bscane
  - Veronica_Axi_Secure_JTAG_IO
  - Veronica_Axi_Secure
  - Veronica_Axi_Linux_JTAG_Xilinx_Bscane
  - Veronica_Axi_Linux_JTAG_IO
  - Veronica_Axi_Linux

### REG MAP
  - ACC PORT  -> 0x70000000L, 256 MB
  - PERF PORT -> 0x40000000L, 256 MB
  - MBUS PORT -> 0x90000000L, config.ddr_size
  - CLINT     -> 0x02000000L, 64 kB
  - PLICT     -> 0x0C000000L, 4 MB
  - RAM       -> 0x80000000L, config.ram_size
