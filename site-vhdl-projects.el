;
; Site-specific lisp for VHDL-mode.
;
; Unused FPGA projects are commented out to prevent the project list from getting too long
;
(custom-set-variables
 '(vhdl-project-alist (quote (
; Only scans correctly if you use a capital 'W' for "C:/Work'
 ("IBM Power A2I" "IBM Power A2I" "C:/Work/CadHut/Engineering/Projects/build_full_system/a2i/" (
 "-r rel/src/*.vhd"
 "-r ./*.vhd"
 ) "\\(work\\|syn\\|.git\\)/" nil "./" "work" "work/" "makefile" "")
 ("IBM Power A2O" "IBM Power A2O" "C:/Work/CadHut/Engineering/Projects/build_full_system/a2o/" (
 "-r rel/src/*.vhd"
 "-r ./*.vhd"
 ) "\\(work\\|syn\\)/" nil "./" "work" "work/" "makefile" "")
 ("Rudi RV32I Test" "Risc-V Rudi RV32I CPU Test" "C:/Work/CadHut/Engineering/Projects/build_full_system/Rudi-RV32I/" (
    "./src/program_memory/program_memory_test.vhd"
    "./src/program_memory/ram_memory_test.vhd"
    "./src/test_benches/tb_riscv_test.vhd"
    "-r ./*.vhd"
 ) "\\(work\\|syn\\|.git\\)/" nil "./" "work" "work/" "makefile" "")
 ("Xilinx CoreLib" "Xilinx Core Library" "C:/Apps/Xilinx/14.7/ISE_DS/ISE/vhdl/src/XilinxCoreLib/" ("-r ./*.vhd") "\\(work\\)/" nil "./" "xilinxcorelib" "xilinxcorelib/" "makefile" "")
 ("Xilinx Secureip" "Xilinx Secureip" "C:/Apps/Xilinx/14.7/ISE_DS/ISE/vhdl/src/unisims/secureip/" ("-r ./*.vhd") "\\(work\\)/" nil "./" "secureip" "secureip/" "makefile" "")
 ("Xilinx Unisim" "Xilinx Unisim" "C:/Apps/Xilinx/14.7/ISE_DS/ISE/vhdl/src/unisims/" ("./*.vhd" "./primitive/*.vhd") "\\(work\\)/" nil "./" "unisim" "unisim/" "makefile" "")
 )))
)