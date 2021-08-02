-- BBC Micro for Altera DE1
--
-- Copyright (c) 2011 Mike Stirling
--
-- All rights reserved
--
-- Redistribution and use in source and synthezised forms, with or without
-- modification, are permitted provided that the following conditions are met:
--
-- * Redistributions of source code must retain the above copyright notice,
--   this list of conditions and the following disclaimer.
--
-- * Redistributions in synthesized form must reproduce the above copyright
--   notice, this list of conditions and the following disclaimer in the
--   documentation and/or other materials provided with the distribution.
--
-- * Neither the name of the author nor the names of other contributors may
--   be used to endorse or promote products derived from this software without
--   specific prior written agreement from the author.
--
-- * License is granted for non-commercial use only.  A fee may not be charged
--   for redistributions as source code or in synthesized/hardware form without 
--   specific prior written agreement from the author.
--
-- THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
-- AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO,
-- THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
-- PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE AUTHOR OR CONTRIBUTORS BE
-- LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
-- CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
-- SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
-- INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
-- CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
-- ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
-- POSSIBILITY OF SUCH DAMAGE.
--
-- BBC Micro "VIDPROC" Video ULA
--
-- Synchronous implementation for FPGA
--
-- (C) 2011 Mike Stirling
--
-- Changes to create replacement Video Processor Chip using a Godil Module
-- June 2021 - Andy Burgess


-- Chip Pinouts
--
-- Pin 1	-	GND
-- Pin 2	-	A0
-- Pin 3	-	CS
-- Pin 4	-	1Mhz Clock out
-- Pin 5	-	2Mhz Clock out
-- Pin 6	-	4Mhz Clock out
-- Pin 7	-	8Mhz Clock out
-- Pin 8	-	16Mhz Clock in
-- Pin 9	-	Blue IN
-- Pin 10	-	Blue OUT
-- Pin 11	-	Green IN
-- Pin 12	-	Green OUT
-- Pin 13	-	Red IN
-- Pin 14	-	Red OUT
-- Pin 15	-	Voltage 2 - 2.2V
-- Pin 16	-	Voltage 1 - 5V
-- Pin 17	-	D0
-- Pin 18	-	D1
-- Pin 19	-	D2
-- Pin 20	-	D3
-- Pin 21	-	D4
-- Pin 22	-	D5
-- Pin 23	-	D6
-- Pin 24	-	D7
-- Pin 25	-	CURSOR
-- Pin 26	-	DISEN
-- Pin 27	-	INVERT
-- Pin 28	-	CRTC CLK


library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;

entity vidproc is
port (
	CLK16		:	in		std_logic;
	CLK8		:	out	std_logic;
	CLK4		:	out	std_logic;
	CLK2		:	out	std_logic;
	CLK1		:	out	std_logic;
	
	-- Clock output to CRTC
	CRTC_CLK	:	out	std_logic;
	
	-- Bus interface
	nCS		:	in		std_logic;
	A0			:	in		std_logic;
	-- CPU data bus
	DI_CPU	:	in		std_logic_vector(7 downto 0);
	
	-- Control interface
	--nINVERT	:	in		std_logic := '1';
	DISEN		:	in		std_logic;
	CURSOR	:	in		std_logic;
	
	-- Video in (teletext mode)
	R_IN		:	in		std_logic;
	G_IN		:	in		std_logic;
	B_IN		:	in		std_logic;
	
	-- Video out
	R			:	out	std_logic;
	G			:	out	std_logic;
	B			:	out	std_logic
	
	);
end entity;

architecture rtl of vidproc is
-- Write-only registers
signal r0_cursor0			:	std_logic := '0';
signal r0_cursor1			:	std_logic := '0';
signal r0_cursor2 		: 	std_logic := '0';
signal r0_crtc_2mhz		:	std_logic := '0';
signal r0_pixel_rate		:	std_logic_vector(1 downto 0) := (others => '0');
signal r0_teletext		:	std_logic := '0';
signal r0_flash			:	std_logic := '0';

type palette_t is array(0 to 15) of std_logic_vector(3 downto 0);
signal palette 			:	palette_t;

-- Pixel shift register
signal shiftreg			:	std_logic_vector(7 downto 0) := (others => '0');
-- Delayed display enable
signal delayed_disen		:	std_logic := '0';

-- Internal clock enable generation
signal clken_pixel		:	std_logic := '0';
signal clken_fetch		:	std_logic := '0';
signal clken_counter		:	unsigned(3 downto 0) := (others => '0');

-- Cursor generation - can span up to 32 pixels
-- Segments 0 and 1 are 8 pixels wide
-- Segment 2 is 16 pixels wide
signal cursor_invert		:	std_logic := '0' ;
signal cursor_active		:	std_logic := '0' ;
signal cursor_counter	:	unsigned(1 downto 0) := (others => '0');

begin

	-- Generate clocks for the rest of the system
	CLK8 <= clken_counter(0);
	CLK4 <= clken_counter(1);
	CLK2 <= clken_counter(2);
	CLK1 <= clken_counter(3);
	
	-- Main clock counter
	process(CLK16)
	begin
		if rising_edge(CLK16) then
			-- Increment internal cycle counter during each video clock
			clken_counter <= clken_counter + 1;
		end if;
	end process;

	-- Read Control Register or Palette Register after falling edge of nCS and CLK2 signal
	process(CLK16)
	begin
		if rising_edge(CLK16) then	
			if nCS = '0' and clken_counter(2 downto 0) = "100" then -- Note "011" appears to avoid glitches
				if A0 = '0' then				-- Control Register FE20
					-- Access control register
					r0_cursor0 <= DI_CPU(7);
					r0_cursor1 <= DI_CPU(6);
					r0_cursor2 <= DI_CPU(5);
					r0_crtc_2mhz <= DI_CPU(4);
					r0_pixel_rate <= DI_CPU(3 downto 2);
					r0_teletext <= DI_CPU(1);
					r0_flash <= DI_CPU(0);
				else
					-- Palette Register FE21
					palette(to_integer(unsigned(DI_CPU(7 downto 4)))) <= DI_CPU(3 downto 0);
				end if;
			end if;
		end if;
	end process;
	
	-- Pixel clock can be divided by 1,2,4 or 8 depending on the value
	-- programmed at r0_pixel_rate
	-- 00 = /8, 01 = /4, 10 = /2, 11 = /1
	clken_pixel <= 
		'1'													when r0_pixel_rate = "11" else		-- 16Mhz Pixel Rate
		clken_counter(0)									when r0_pixel_rate = "10" else		-- 8Mhz Pixel Rate
		(clken_counter(0) and clken_counter(1))	when r0_pixel_rate = "01" else		-- 4Mhz Pixel Rate
		(clken_counter(0) and clken_counter(1) and clken_counter(2));						-- 2Mhz Pixel Rate
	
	-- Generate clock for the CRTC controller chip
	CRTC_CLK <= clken_counter(2) when r0_crtc_2mhz = '1' else clken_counter(3);
	
	-- Fetch screen data from memory on the falling edge of the CRTC_CLK clock
	-- This is used for reloading the shift register as well as counting cursor pixels
	clken_fetch <= (clken_counter(0) and clken_counter(1) and clken_counter(2) and (clken_counter(3) or r0_crtc_2mhz));
		
	-- Fetch control
	process(CLK16)
	begin
		if falling_edge(CLK16) then
		  if clken_pixel = '1' then
			if clken_fetch = '1' then
				-- Fetch next byte from memory into shift register
				-- This occurs on the falling edge of CRTC_CLK
				shiftreg <= DI_CPU;
				delayed_disen <= DISEN;
			else
				-- Clock shift register and input '1' at LSB
				shiftreg <= shiftreg(6 downto 0) & "1";
			end if;
		 end if;	
		end if;
	end process;

	-- Cursor generation
	cursor_invert <= cursor_active and
		((r0_cursor0 and not (cursor_counter(0) or cursor_counter(1))) or
		(r0_cursor1 and cursor_counter(0) and not cursor_counter(1)) or
		(r0_cursor2 and cursor_counter(1)));
	
	process(CLK16)
	begin
		if rising_edge(CLK16) and clken_fetch = '1' then
			if CURSOR = '1' or cursor_active = '1' then
				-- Latch cursor
				cursor_active <= '1';
		
				-- Reset on counter wrap
				if cursor_counter = "11" then
					cursor_active <= '0';
				end if;
				
				-- Increment counter
				if cursor_active = '0' then
					-- Reset
					cursor_counter <= (others => '0');
				else
					-- Increment
					cursor_counter <= cursor_counter + 1;
				end if;
			end if;
		end if;
	end process;
	
	-- Pixel generation
	-- By running this process on every single video tick instead of at
	-- the pixel rate we ensure that the resulting delay is minimal and
	-- constant (running this at the pixel rate would cause
	-- the display to move slightly depending on which mode was selected). 
	process(CLK16)
	variable palette_a : std_logic_vector(3 downto 0);
	variable dot_val : std_logic_vector(3 downto 0);
	variable red_val : std_logic;
	variable green_val : std_logic;
	variable blue_val : std_logic;
	begin
		if rising_edge(CLK16) then
			-- Look up dot value in the palette.  Bits are as follows:
			-- bit 3 - FLASH
			-- bit 2 - Not BLUE
			-- bit 1 - Not GREEN
			-- bit 0 - Not RED
			palette_a := shiftreg(7) & shiftreg(5) & shiftreg(3) & shiftreg(1);
			dot_val := palette(to_integer(unsigned(palette_a)));
		
			-- Apply flash inversion if required
			red_val := (dot_val(3) and r0_flash) xor not dot_val(0);
			green_val := (dot_val(3) and r0_flash) xor not dot_val(1);
			blue_val := (dot_val(3) and r0_flash) xor not dot_val(2);
		
			-- To output
			-- FIXME: INVERT option
			if r0_teletext = '0' then
				-- Cursor can extend outside the bounds of the screen, so
				-- it is not affected by DISEN
				R <= (red_val and delayed_disen) xor cursor_invert;
				G <= (green_val and delayed_disen) xor cursor_invert;
				B <= (blue_val and delayed_disen) xor cursor_invert;
			else
				R <= R_IN xor cursor_invert;
				G <= G_IN xor cursor_invert;
				B <= B_IN xor cursor_invert;
			end if;
		end if;
	end process;
end architecture;

