library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;



entity lcd_test is 
	generic(deb_cycles : integer := 200;
			idle_cycles : integer := 3000);
			
	port(clk, ps2clk, din, reset : in bit;
		 is_lcd_on : in bit;
		 E, LCD_ON : buffer bit;
		 db : out bit_vector(9 downto 0);
		 dout : out bit_vector(7 downto 0));
end;



architecture arc of lcd_test is 
	signal data_reg, data : bit_vector(10 downto 0);
	signal deb_ps2clk, deb_ps2data, idle : bit;
	signal data_tmp, lcd_encode : bit_vector(7 downto 0);
	type state is (init, A, B, D, O);
	TYPE STATE_W IS (STANDBY, GETKEY, HALT);
	type state_lcd is (function_set, clear, display_ctrl, entry_mode, write_data, incr, decr);
	SIGNAL PR_STATE_W, NEXT_STATE_W : STATE_W;
	signal pr_state_lcd, next_state_lcd : state_lcd;
	signal lcd_pos : unsigned(9 downto 0);
	
	SIGNAL ENA : STD_LOGIC;
	
	begin
		process(clk) 
			variable counter : integer range 0 to deb_cycles;
			begin 
				if(clk'event and clk = '1') then 
					if(deb_ps2clk = ps2clk) then 
						counter := 0;
					else 
						counter := counter + 1;
						if(counter = deb_cycles) then
							deb_ps2clk <= ps2clk;
							counter := 0;
						end if;
					end if;
				end if;
		end process;
		

		process(clk)
			variable counter : integer range 0 to deb_cycles;
			begin 
				if(clk'event and clk = '1') then 
					if(deb_ps2data = din) then 
						counter := 0;
					else 
						counter := counter + 1;
						if(counter = deb_cycles) then
							deb_ps2data <= din;
							counter := 0;
						end if;
					end if;
				end if;
		end process;
				
				
	PROCESS (clk)
		VARIABLE count: INTEGER RANGE 0 TO idle_cycles;
		BEGIN
			IF (clk'EVENT AND clk='0') THEN
				IF (deb_ps2data='0') THEN
					 idle <= '0';
					 count := 0;
				ELSIF (deb_ps2clk='1') THEN
				count := count + 1;
					IF (count=idle_cycles) THEN
						idle <= '1';
					END IF;
				ELSE
					count := 0;
				END IF;
			END IF;
	END PROCESS;
						
						
		process(deb_ps2clk)
			variable i : integer range 0 to 15;
			begin 
				if(deb_ps2clk'event and deb_ps2clk = '0') then 
					if(idle = '1') then 
						i := 0;
					else
						data_reg(i) <= deb_ps2data;
						i := i + 1;
						if(i = 11) then
							data <= data_reg;
							i := 0;
						end if;
					end if;
				end if;
		end process;
				
				
		------CONTROLLER FINITE STATE MACHINE (PS/2)
		process(E, reset)
			begin 
				if(reset = '1') then 
					PR_STATE_W <= HALT;
				elsif(E'event and E = '1') then 
					PR_STATE_W <= NEXT_STATE_W;
				end if;
		end process;
		
						
		
		PROCESS(DATA, PR_STATE_W, ENA) 
			BEGIN 
				CASE PR_STATE_W IS 
					WHEN HALT =>
						IF(data(8 downto 1) = "11110000") THEN	
							ENA <= '0'; 
							NEXT_STATE_W <= STANDBY;
						ELSE 
							ENA <= '0';
							NEXT_STATE_W <= HALT;
						END IF;
					WHEN STANDBY =>
						IF(data(8 downto 1) = "11110000") THEN 
							ENA <= '0';
							NEXT_STATE_W <= STANDBY;
						ELSE 
							ENA <= '0';
							NEXT_STATE_W <= GETKEY;
						END IF;
					WHEN GETKEY =>
						ENA <= '1';
						NEXT_STATE_W <= HALT;
				END CASE;
		END PROCESS;
		
		
		data_tmp <= data(8 downto 1) when(ena = '1') else (others => '1');
		dout <= data_tmp;
		
		
		-----EXTENDABLE PS/2 TO LCD CGROM LUT
		process(data_tmp) 
			begin 
				case data_tmp is
					when "01000101" => lcd_encode <= "00110000"; --0
					when "00010110" => lcd_encode <= "00110001"; --1
					when "00011110" => lcd_encode <= "00110010"; --2
					when "00100110" => lcd_encode <= "00110011"; --3
					when "00100101" => lcd_encode <= "00110100"; --4
					when "00101110" => lcd_encode <= "00110101"; --5
					when "00110110" => lcd_encode <= "00110110"; --6
					when "00111101" => lcd_encode <= "00110111"; --7
					when "00111110" => lcd_encode <= "00111000"; --8
					when "01000110" => lcd_encode <= "00111001"; --9
					when "00010101" => lcd_encode <= "01010001"; --A
					when "00110010" => lcd_encode <= "01000010"; --B
					when "00100001" => lcd_encode <= "01000011"; --C
					when "00100011" => lcd_encode <= "01000100"; --D
					when "00100100" => lcd_encode <= "01000101"; --E
					when "00101011" => lcd_encode <= "01000110"; --F
					when "01001101" => lcd_encode <= "01010000"; --P
					when "01000100" => lcd_encode <= "11101111"; --OOH
					when "00110011" => lcd_encode <= "01001000"; --H
					when "01000011" => lcd_encode <= "01001001"; --I
					when "00011100" => lcd_encode <= "01000001"; --Q
					when "00011101" => lcd_encode <= "01010111"; --W
					when "00101101" => lcd_encode <= "01010010"; --R
					when "00101100" => lcd_encode <= "01010100"; --T
					when "00110101" => lcd_encode <= "01011001"; --Y
					when "00011010" => lcd_encode <= "01011010"; --Z
					when "00100010" => lcd_encode <= "01011000"; --X
					when "01001011" => lcd_encode <= "01001100"; --L
					when "00101010" => lcd_encode <= "01010110"; --V
					when "00111100" => lcd_encode <= "01010101"; --U
					when "00011011" => lcd_encode <= "01010011"; --S
					when "00110100" => lcd_encode <= "01000111"; --G
					when "00111011" => lcd_encode <= "01001010"; --J
					when "01000010" => lcd_encode <= "01001011"; --K
					when "00111010" => lcd_encode <= "01001101"; --M
					when "00110001" => lcd_encode <= "01001110"; --N
					when "01001001" => lcd_encode <= "00101110"; --.
					when "01000001" => lcd_encode <= "00101100"; --,
					when "01011010" => lcd_encode <= "11110111"; --Enter
					when "00101001" => lcd_encode <= "10000000"; --space--
					when "01110110" => lcd_encode <= "11111111"; --ESC--
					when "01100110" => lcd_encode <= "10000010"; --backspace--
					when others => lcd_encode <= "00010000"; --null
				end case;
		end process;
		
		
		-------------------------------------------------
		LCD_ON <= is_LCD_ON;
		

		process(clk)
			variable counter : integer range 0 to 50_000_000;
			begin
				if(clk'event and clk = '1') then
					if(counter = 50_000_000 / 1000) then
						E <= not E;
						counter := 0;
					else 
						counter := counter + 1;
					end if;
				end if;
		end process;	
				
				
		------CONTROLLER FINITE STATE MACHINE (LCD)			
		process(E, reset)
			begin
				if(reset = '1') then 
					pr_state_lcd <= function_set;
				elsif(E'event and E = '1') then 
					pr_state_lcd <= next_state_lcd;
				end if;
		end process;	
		
		
		process(pr_state_lcd, lcd_encode, lcd_pos)
			begin
				case pr_state_lcd is 
					when function_set => 
						DB <= "0000111000";
						next_state_lcd <= display_ctrl;
					
					when display_ctrl =>
						DB <= "0000001110";
						next_state_lcd <= entry_mode;
					
					when entry_mode =>
						DB <= "0000000110";
						next_state_lcd <= clear;
					
					when clear =>
						DB <= "0000000001";
						next_state_lcd <= write_data;
					
					when write_data =>
						if(lcd_encode = "00010000") then 
							db <= to_bitvector(std_logic_vector(lcd_pos));
							next_state_lcd <= write_data;
						elsif(lcd_encode = "11111111") then
							db <= "10" & lcd_encode;
							next_state_lcd <= clear;
						elsif(lcd_encode = "10000010") then 
							db <= "10" & lcd_encode;
							next_state_lcd <= decr;
						else 
							db <= "10" & lcd_encode;
							next_state_lcd <= incr;
						end if;
						
					when incr => ----DECREMENT CURSOR POSITION
						db <= to_bitvector(std_logic_vector(lcd_pos));
						next_state_lcd <= write_data;
						
					when decr => ----DECREMENT CURSOR POSITION (MAINLY TO BACKSPACE ACTIVATION)
						db <= to_bitvector(std_logic_vector(lcd_pos));
						next_state_lcd <= write_data;
				end case;
		end process;	
		
		
		------CURSOR CONTROL FSM LOWER PART
		process(E, pr_state_lcd)
			begin
				if(pr_state_lcd = clear) then 
					lcd_pos <= "0010000000";
				elsif(E'event and E = '1') then 
					if(pr_state_lcd = incr) then 
						lcd_pos <= lcd_pos + 1;
					elsif(pr_state_lcd = decr) then
						if(lcd_pos = "0010000000") then 
							lcd_pos <= "0010000000";
						else lcd_pos <= lcd_pos - 1;
						end if;
					end if;
				end if;
		end process;
end;

--------------------------------------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------------------------------------

