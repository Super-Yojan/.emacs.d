;
; Site-specific lisp for VHDL-mode.
;
(custom-set-variables
 '(vhdl-company-name "Change My Name")
 '(vhdl-copyright-string "Copyright (c) <year> Your Name, <company>. All rights reserved.
-- 
-- This code is a sample and does not come with any warranty or guarantee of correctness.
-- Redistribution and use in source and binary forms (with or without
-- modification) is permitted.")
 '(vhdl-file-header "-------------------------------------------------------------------------------
--
-- <copyright>
--
-------------------------------------------------------------------------------
-- Project Name  : <project>
-- Author(s)     : <login>
-- File Name     : <filename>
--
-- <cursor>
--
-------------------------------------------------------------------------------

")
 '(vhdl-testbench-declarations "  constant C_COUNT_MAX : natural := 127;
  signal   count       : natural range 0 to C_COUNT_MAX;
")
 '(vhdl-testbench-statements "  -------------------------------------------------------------------------------
  -- System clock generation
  clk_gen : process
  begin
    clk <= '0';
    wait for 5 ns;
    clk <= '1';
    wait for 5 ns;
  end process clk_gen;

  -----------------------------------------------------------------------------
  -- Reset generation
  rst_gen : process
  begin
    rst <= '1';
    wait for 100 ns;
    rst <= '0';
    wait;
  end process rst_gen;

  ----------------------------------------------------------------------
  -- Counter
  --  constant C_COUNT_MAX : natural := 127;
  --  signal   count       : natural range 0 to C_COUNT_MAX;
  process (clk)
  begin
    if (rising_edge(clk)) then
      if (rst = '0') then
        count <= 0;
      else
        if (count < C_COUNT_MAX) then
          count <= count + 1;
        else
          count <= 0;
        end if;
      end if;
    end if;
  end process;
")
 '(vhdl-model-alist (quote (("Synchronous Reset Flip-Flop" "----------------------------------------------------------------------
-- <description>
process (<clock>)
begin
  if (rising_edge(<clock>)) then
    if (<reset> = '1') then
      <output> <= (others => '0');
    else
      <output> <= <cursor>;
    end if;
  end if;
end process;

" "f" "") ("Synchronous Reset Flip-Flop with Enable" "----------------------------------------------------------------------
-- <description>
process (<clock>)
begin
  if (rising_edge(<clock>)) then
    if (<reset> = '1') then
      <output> <= (others => '0');
    elsif (<enable> = '1') then
      <output> <= <cursor>;
    else
      <output> <= ;
    end if;
  end if;
end process;

" "e" "") ("Re-synchronise to New Clock" "----------------------------------------------------------------------
-- Re-synchronise the asynchronous '<signal>' signal to '<new_clock>'
  process (<new_clock>)
  begin
    if (rising_edge(<new_clock>)) then
      ss_<signal> <= ns_<signal>;
      <signal>    <= ss_<signal>;
    end if;
  end process;

" "r" "Resync to new clock") ("State Machine with Synchronous Reset" "----------------------------------------------------------------------
-- <description>
  process (<clock>)
  begin
    if (rising_edge(<clock>)) then
      if (<reset> = '1') then
        <state> <= idle;
      else

        case <state> is

----------------------------------------------------------------------
          when state1 =>

----------------------------------------------------------------------
          when state2 =>

----------------------------------------------------------------------
          when others =>        -- idle state

        end case;

      end if;
    end if;
  end process;

" "s" "") ("Multiplexer" "    <result> <= <signal 1> when <control> = '1' else
         <signal 2>;

" "m" "") ("INOUT Signal Multiplexer" "-------------------------------------------------------------------------------
-- Multiplexer for 'inout' style signals
<inout signal> <= <out signal> when <output enable signal> = '1' else
         (others => 'z');

" "b" "") ("New VHDL 'entity' header file" "library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity <component name> is
  port(
    -- Clock and Reset signals
    <clock> : in std_logic;
    <reset> : in std_logic;

    -- Other signals
    <cursor>
    );
end <component name>;
" "ve" "") ("New VHDL 'architecture' file" "library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

architecture <component name>_rtl of <component name> is
  -- Internal signals

begin -- <component name>_rtl


end <component name>_rtl;
" "va" "") ("New VHDL 'structure' file" "library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

architecture <component name>_structure of <component name> is

-- Declare signals


begin -- <component name>_structure

-- Instantiate components


end <component name>_structure;
" "vs" "") ("New VHDL 'package' header file" "library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

package <package name> is
  -- Type declarations

  -- Function and procedure declarations

end <package name>;
" "vp" "") ("New VHDL 'package' body file" "library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

package body <package name> is

end <package name>;
" "vb" "") ("Clock and Reset generation (testbench only)" "-------------------------------------------------------------------------------
-- Clock generation
  <clock>_gen : process
  begin
    <clock> <= '0';
    wait for <half peroid> ns;
    <clock> <= '1';
    wait for <half peroid> ns;
  end process <clock>_gen;

-----------------------------------------------------------------------------
-- Reset generation
  <reset>_gen : process
  begin
    <reset> <= '1';
    wait for <reset length> ns;
    <reset> <= '0';
    wait;
  end process <reset>_gen;

" "c" "Clock and Reset generation (testbench only)"))))
 '(vhdl-testbench-create-files (quote single))
 '(vhdl-testbench-architecture-header "-------------------------------------------------------------------------------
--
-- <copyright>
--
-------------------------------------------------------------------------------
-- Project Name  : <project>
-- Author(s)     : <login>
-- File Name     : <filename>
--
-- <cursor>
--
-------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

<cursor>")
 '(vhdl-testbench-entity-header "-------------------------------------------------------------------------------
--
-- <copyright>
--
-------------------------------------------------------------------------------
-- Project Name  : <project>
-- Author(s)     : <login>
-- File Name     : <filename>
--
-- <cursor>
--
-------------------------------------------------------------------------------

<cursor>")
 )
