--
-- HT 1080Z (TSR-80 clone) top level
--
--
-- Copyright (c) 2016-2017 Jozsef Laszlo (rbendr@gmail.com)
--
-- All rights reserved
--
-- Redistribution and use in source and synthezised forms, with or without
-- modification, are permitted provided that the following conditions are met:
--
-- Redistributions of source code must retain the above copyright notice,
-- this list of conditions and the following disclaimer.
--
-- Redistributions in synthesized form must reproduce the above copyright
-- notice, this list of conditions and the following disclaimer in the
-- documentation and/or other materials provided with the distribution.
--
-- Neither the name of the author nor the names of other contributors may
-- be used to endorse or promote products derived from this software without
-- specific prior written permission.
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
-- Please report bugs to the author, but before you do so, please
-- make sure that this is not a derivative work and that
-- you have the latest version of this file.
--


library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.STD_LOGIC_ARITH.ALL;
use IEEE.STD_LOGIC_UNSIGNED.ALL;
use IEEE.NUMERIC_STD.ALL;

entity ht1080z is
    Port ( clk42m : in  STD_LOGIC;
        pllLocked : in  STD_LOGIC;
            reset : in  STD_LOGIC;
              led : out STD_LOGIC;
             ntsc : in  STD_LOGIC;
            turbo : in  STD_LOGIC;

           ps2clk : in  STD_LOGIC;
           ps2dat : in  STD_LOGIC;			

           ht_rgb : out std_logic_vector(17 downto 0);
               hs : out STD_LOGIC;
               vs : out STD_LOGIC;
               hb : out STD_LOGIC;
               vb : out STD_LOGIC;
         audiomix : out std_logic_vector(8 downto 0); 

            dn_go : in  std_logic;
            dn_wr : in  std_logic;
          dn_addr : in  std_logic_vector(24 downto 0);
          dn_data : in  std_logic_vector(7 downto 0);
           dn_idx : in  std_logic_vector(4 downto 0);

		 -- SDRAM
		  SDRAM_nCS : out std_logic;                     -- Chip Select
		   SDRAM_DQ : inout std_logic_vector(15 downto 0); -- SDRAM Data bus 16 Bits
		    SDRAM_A : out std_logic_vector(12 downto 0);  -- SDRAM Address bus 13 Bits
		 SDRAM_DQMH : out std_logic; -- SDRAM High Data Mask
		 SDRAM_DQML : out std_logic; -- SDRAM Low-byte Data Mask
		  SDRAM_nWE : out std_logic;  -- SDRAM Write Enable
		 SDRAM_nCAS : out std_logic; -- SDRAM Column Address Strobe
		 SDRAM_nRAS : out std_logic; -- SDRAM Row Address Strobe
		   SDRAM_BA : out std_logic_vector(1 downto 0); -- SDRAM Bank Address
		  SDRAM_CKE : out std_logic  -- SDRAM Clock Enable			 
  			  			  
    ); 
end ht1080z;

architecture Behavioral of ht1080z is

component sdram is
      port( sd_data : inout std_logic_vector(15 downto 0);
            sd_addr : out std_logic_vector(12 downto 0);
             sd_dqm : out std_logic_vector(1 downto 0);
              sd_ba : out std_logic_vector(1 downto 0);
              sd_cs : out std_logic;
              sd_we : out std_logic;
             sd_ras : out std_logic;
             sd_cas : out std_logic;
               init : in std_logic;
                clk : in std_logic;
                din : in std_logic_vector(7 downto 0);
               dout : out std_logic_vector(7 downto 0);
               addr : in std_logic_vector(24 downto 0);
                 oe : in std_logic;
                 we : in std_logic
      );
end component;

signal sdram_dqm  : std_logic_vector(1 downto 0);
signal ram_addr : std_logic_vector(24 downto 0);
signal  ram_din : STD_LOGIC_VECTOR(7 downto 0);
signal ram_dout : STD_LOGIC_VECTOR(7 downto 0);
signal ram_we: std_logic;
signal ram_oe: std_logic; 

signal   dn_wr_r : std_logic;
signal dn_wr_new : std_logic;
signal dn_addr_r : std_logic_vector(24 downto 0);
signal dn_data_r : std_logic_vector(7 downto 0);

signal res_cnt : std_logic_vector(5 downto 0) := "111111";
signal autores : std_logic; 

signal cpua     : std_logic_vector(15 downto 0); 
signal cpudo    : std_logic_vector(7 downto 0);
signal cpudi    : std_logic_vector(7 downto 0);
signal cpuwr,cpurd,cpumreq,cpuiorq,cpunmi,cpuint,cpum1,cpuClkEn,clkref : std_logic;

signal rgbi : std_logic_vector(3 downto 0);
signal romdo,vramdo,ramdo,ramHdo,kbdout : std_logic_vector(7 downto 0);
signal vramcs : std_logic;

signal page,vcut,swres : std_logic;

signal romrd,ramrd,ramwr,vramsel,kbdsel : std_logic;
signal ior,iow,memr,memw : std_logic;
signal vdata : std_logic_vector(7 downto 0);

signal clk42div : std_logic_vector(4 downto 0);

signal dacout : std_logic;
signal sndBC1,sndBDIR,sndCLK : std_logic;
signal oaudio,snddo : std_logic_vector(7 downto 0); 

signal p_hs,p_vs,vgahs,vgavs : std_logic; 
signal pclk : std_logic; 

signal io_ram_addr : std_logic_vector(23 downto 0);
signal iorrd,iorrd_r : std_logic;

signal tapebits : std_logic_vector(2 downto 0); 
signal  speaker : std_logic_vector(7 downto 0); 
signal vga : std_logic := '0';
signal oddline : std_logic;

signal inkpulse, paperpulse, borderpulse : std_logic;

begin

  led <= not dn_go;--swres;
  
  process(clk42m)
  begin
    if rising_edge(clk42m) then
	    clk42div <= clk42div + 1;
		 -- 42 MHz/24 = 1.75 MHz
		 if clk42div = 23 then clk42div <= (others => '0'); end if;
	 end if;
  end process;
  
  ior <= cpurd or cpuiorq or (not cpum1);
  iow <= cpuwr or cpuiorq;
  memr <= cpurd or cpumreq;
  memw <= cpuwr or cpumreq;
  
  romrd <= '1' when memr='0' and cpua<x"3780" else '0';
  ramrd <= '1' when cpua(15 downto 14)/="00" and memr='0' else '0';
  ramwr <= '1' when cpua(15 downto 14)/="00" and memw='0' else '0';
  vramsel <= '1' when cpua(15 downto 10)="001111" and cpumreq='0' else '0';
  kbdsel  <= '1' when cpua(15 downto 10)="001110" and memr='0' else '0';
  iorrd <= '1' when ior='0' and cpua(7 downto 0)=x"04" else '0'; -- in 04
    
  cpu : entity work.T80s
   port map (  
	   RESET_n => autores, --swres,
		CLK     => clk42m, 
		CEN     => cpuClkEn and not dn_go, -- 1.75 MHz
		WAIT_n  => '1',
		INT_n   => '1',
		NMI_n   => '1',
		BUSRQ_n => '1',
		M1_n    => cpum1,
		MREQ_n  => cpumreq,
		IORQ_n  => cpuiorq,
		RD_n    => cpurd,
		WR_n    => cpuwr,
		RFSH_n  => open,
		HALT_n  => open,
		BUSAK_n => open,
		A       => cpua, 
		DI      => cpudi,
		DO      => cpudo
  );

  cpudi <= --romdo when romrd='1' else
	        --ramdo when ramrd='1' else
			  ram_dout when romrd='1' else
			  ram_dout when ramrd='1' else
			  vramdo when vramsel='1' else
			  kbdout when kbdsel='1' else
			  x"30" when ior='0' and cpua(7 downto 0)=x"fd" else -- printer io read
			  ram_dout when iorrd='1' else
			  x"ff";
			  --ram_dout;


  vdata <= cpudo when cpudo>x"1f" else cpudo or x"40";
  -- video ram at 0x3C00
  video : entity work.videoctrl 
    port map (   
	  reset => autores, --swres and pllLocked,
	  clk42 => clk42m,
			a => cpua(13 downto 0),
		 din => vdata,--cpudo,
		dout => vramdo,
		mreq => cpumreq,
		iorq => cpuiorq,
		  wr => cpuwr,
		  cs => not vramsel,
		hz60 => ntsc,
		vcut => vcut,
		vvga => '0',
		page => page,
		rgbi => rgbi, 
	   pclk => pclk,
		inkp => inkpulse,
	 paperp => paperpulse,
	borderp => borderpulse,
	oddline => oddline,
	  hsync => hs,
	  vsync => vs,
	  hblank=> hb,
	  vblank=> vb
   );  

  kbd : entity work.ps2kbd 
    port  map ( 
	   RESET => not pllLocked,
 	   KBCLK => ps2clk,
	   KBDAT => ps2dat,
		SWRES => swres,
	     CLK => clk42m,
		    A => cpua(7 downto 0),
		 DOUT => kbdout,
		 PAGE => page,
		 VCUT => vcut,
		 INKP => inkpulse,
	  PAPERP => paperpulse,
	 BORDERP => borderpulse
		  );
	
	-- PSG 
	-- out 1e = data port
	-- out 1f = register index

 soundchip : entity work.YM2149 
   port map (
  -- data bus
  I_DA      => cpudo,     
  O_DA      => open,     
  O_DA_OE_L => open,     
  -- control
  I_A9_L   	=> '0',      
  I_A8      => '1',     
  I_BDIR    => sndBDIR,     
  I_BC2     => '1',     
  I_BC1     => sndBC1,     
  I_SEL_L   => '1',     

  O_AUDIO   => oaudio,     
  -- port a
  I_IOA      => "ZZZZZZZZ",     
  O_IOA      => open,     
  O_IOA_OE_L => open,         
  -- port b
  I_IOB      => "ZZZZZZZZ",         
  O_IOB      => open,         
  O_IOB_OE_L => open,         
  --
  ENA        => cpuClkEn,
  RESET_L    => autores,--swres and pllLocked,    
  CLK        => clk42m
  );
  sndBDIR <= '1' when cpua(7 downto 1)="0001111" and iow='0' else '0';
  sndBC1  <= cpua(0);
	  
  with tapebits select speaker <=
   "00100000" when "001",
	"00010000" when "000"|"011",
   "00000000" when others;
 
  audiomix <= ('0' & oaudio) + ('0' & speaker); 
 	 
	-- fix palette for now
	--with rgbi select rgb <=
	with rgbi select ht_rgb <=
	 "000000000000000000" when "0000",
	 "000000000000100000" when "0001",
	 "000000100000000000" when "0010",
	 "000000100000100000" when "0011",
	 "100000000000000000" when "0100",
	 "100000000000100000" when "0101",
	 "110000011000000000" when "0110",
	 "100000100000100000" when "0111",
	 "110000110000110000" when "1000",
	 "000000000000111100" when "1001",
	 "000000111100000000" when "1010",
	 "000000111100111100" when "1011",
	 "111110000000000000" when "1100",
	 "111100000000111100" when "1101",
	 "111110111110000000" when "1110",
	 "111110111110111110" when others;

  sdram_inst : sdram
    port map( sd_data => SDRAM_DQ,
              sd_addr => SDRAM_A,
               sd_dqm => sdram_dqm,
                sd_cs => SDRAM_nCS,
                sd_ba => SDRAM_BA,
                sd_we => SDRAM_nWE,
               sd_ras => SDRAM_nRAS,
               sd_cas => SDRAM_nCAS,
                  clk => clk42m,
                 init => not pllLocked,
                  din => ram_din,
                 addr => ram_addr,
                   we => ram_we,
                   oe => ram_oe,
                 dout => ram_dout
    ); 	
  --ram_addr <= "000000000" & cpua when dn_go='0' else dn_addr_r; 
  ram_din <= cpudo when dn_go='0' else dn_data_r;
  ram_we <= ((not memw) and (cpua(15) or cpua(14))) when dn_go='0' else dn_wr_r; 
  --ram_oe <= not memr when dn_go='0' else '0';
  
  ram_addr <= "0" & io_ram_addr when iorrd='1' else "000000000" & cpua when dn_go='0' else dn_addr_r; 
  ram_oe <= '1' when iorrd='1' else not memr when dn_go='0' else '0';
	 
  -- sdram interface
  SDRAM_CKE <= '1';
  SDRAM_DQMH <= sdram_dqm(1);
  SDRAM_DQML <= sdram_dqm(0); 	 

  process(clk42m)
  -- sync wr from data io to sdram clkref
  begin
    if rising_edge(clk42m) then
	   if dn_wr='1' then
		  dn_wr_new <= '1';
		  dn_data_r <= dn_data;
			if dn_idx = x"00" then
				dn_addr_r <= dn_addr;
			else
				dn_addr_r <= dn_addr + x"10000";
			end if;
		end if;
		if clkref = '1' then
			if dn_wr_new = '1' then
				dn_wr_r <= '1';
				dn_wr_new <= '0';
			else
				dn_wr_r <= '0';
			end if;
		end if;
	 end if;
  end process; 
  
 process (clk42m, pllLocked)
 begin
   if pllLocked = '0' then
		  res_cnt <= "000000";
   elsif rising_edge(clk42m) then
		if reset = '1' then
		  res_cnt <= "000000";
		else
		  if (res_cnt/="111111") then
			 res_cnt <= res_cnt+1;
		  end if;
		end if;
   end if;
 end process;
 
 cpuClkEn <= '1' when clk42div = 0 or (turbo = '1' and clk42div = 12) else '0'; -- 42/24 = 1.75 MHz
 clkref <= '1' when clk42div = 0 or clk42div = 12 else '0'; -- 42/24*2 = 3.5 MHz
 autores <= '1' when res_cnt="111111" else '0';   


 process (clk42m)
 begin
   if rising_edge(clk42m) then  
      if dn_go='1' or autores='0' then
	     io_ram_addr <= x"010000"; -- above 64k
	     iorrd_r<='0';
		elsif cpuClkEn = '1' then
		  if iow='0' and cpua(7 downto 0)=x"ff" then 
		    tapebits <= cpudo(2 downto 0);
		  end if;
		  if iow='0' and cpua(7 downto 2)="000001" then -- out 4 5 6
			 case cpua(1 downto 0) is
				when "00"=> io_ram_addr(7 downto 0) <= cpudo;
				when "01"=> io_ram_addr(15 downto 8) <= cpudo;
				when "10"=> io_ram_addr(23 downto 16) <= cpudo;
				when others => null;
			 end case;
		  end if;
		  iorrd_r<=iorrd;
		  if iorrd='0' and iorrd_r='1' then
			 io_ram_addr <= io_ram_addr + 1;
		  end if;
		end if;
	end if;
 end process;

 
end Behavioral;
