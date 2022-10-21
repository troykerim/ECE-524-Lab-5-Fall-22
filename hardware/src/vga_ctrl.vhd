library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.std_logic_unsigned.all;

entity vga_ctrl is
    Port ( CLK_I : in  STD_LOGIC;
           VGA_HS_O : out  STD_LOGIC;
           VGA_VS_O : out  STD_LOGIC;
           VGA_R : out  STD_LOGIC_VECTOR (3 downto 0);
           VGA_B : out  STD_LOGIC_VECTOR (3 downto 0);
           VGA_G : out  STD_LOGIC_VECTOR (3 downto 0);
           sw    : in   std_logic_vector (3 downto 0);
           btn   : in   std_logic_vector (3 downto 0)
           );
end vga_ctrl;

architecture Behavioral of vga_ctrl is

component clk_wiz_0
port
 (-- Clock in ports
  CLK_IN1           : in     std_logic;
  -- Clock out ports
  CLK_OUT1          : out    std_logic
 );
end component;

--Sync Generation constants

----***640x480@60Hz***--  Requires 25 MHz clock
--constant FRAME_WIDTH : natural := 640;
--constant FRAME_HEIGHT : natural := 480;

--constant H_FP : natural := 16; --H front porch width (pixels)
--constant H_PW : natural := 96; --H sync pulse width (pixels)
--constant H_MAX : natural := 800; --H total period (pixels)

--constant V_FP : natural := 10; --V front porch width (lines)
--constant V_PW : natural := 2; --V sync pulse width (lines)
--constant V_MAX : natural := 525; --V total period (lines)

--constant H_POL : std_logic := '0';
--constant V_POL : std_logic := '0';

----***800x600@60Hz***--  Requires 40 MHz clock
--constant FRAME_WIDTH : natural := 800;
--constant FRAME_HEIGHT : natural := 600;
--
--constant H_FP : natural := 40; --H front porch width (pixels)
--constant H_PW : natural := 128; --H sync pulse width (pixels)
--constant H_MAX : natural := 1056; --H total period (pixels)
--
--constant V_FP : natural := 1; --V front porch width (lines)
--constant V_PW : natural := 4; --V sync pulse width (lines)
--constant V_MAX : natural := 628; --V total period (lines)
--
--constant H_POL : std_logic := '1';
--constant V_POL : std_logic := '1';


----***1280x720@60Hz***-- Requires 74.25 MHz clock
--constant FRAME_WIDTH : natural := 1280;
--constant FRAME_HEIGHT : natural := 720;
--
--constant H_FP : natural := 110; --H front porch width (pixels)
--constant H_PW : natural := 40; --H sync pulse width (pixels)
--constant H_MAX : natural := 1650; --H total period (pixels)
--
--constant V_FP : natural := 5; --V front porch width (lines)
--constant V_PW : natural := 5; --V sync pulse width (lines)
--constant V_MAX : natural := 750; --V total period (lines)
--
--constant H_POL : std_logic := '1';
--constant V_POL : std_logic := '1';

----***1280x1024@60Hz***-- Requires 108 MHz clock
--constant FRAME_WIDTH : natural := 1280;
--constant FRAME_HEIGHT : natural := 1024;

--constant H_FP : natural := 48; --H front porch width (pixels)
--constant H_PW : natural := 112; --H sync pulse width (pixels)
--constant H_MAX : natural := 1688; --H total period (pixels)

--constant V_FP : natural := 1; --V front porch width (lines)
--constant V_PW : natural := 3; --V sync pulse width (lines)
--constant V_MAX : natural := 1066; --V total period (lines)

--constant H_POL : std_logic := '1';
--constant V_POL : std_logic := '1';

--***1920x1080@60Hz***-- Requires 148.5 MHz pxl_clk
constant FRAME_WIDTH : natural := 1920;
constant FRAME_HEIGHT : natural := 1080;

constant ONE_THIRD_H : natural := FRAME_WIDTH/3;
constant ONE_EIGTH_H : natural := FRAME_WIDTH/8;

constant H_FP : natural := 88; --H front porch width (pixels)
constant H_PW : natural := 44; --H sync pulse width (pixels)
constant H_MAX : natural := 2200; --H total period (pixels)

constant V_FP : natural := 4; --V front porch width (lines)
constant V_PW : natural := 5; --V sync pulse width (lines)
constant V_MAX : natural := 1125; --V total period (lines)

constant H_POL : std_logic := '1';
constant V_POL : std_logic := '1';

--Moving Box constants
constant BOX_WIDTH : natural := 20;
constant BOX_CLK_DIV : natural := 1000000; --MAX=(2^25 - 1)

constant BOX_X_MAX : natural := (FRAME_WIDTH - BOX_WIDTH);
constant BOX_Y_MAX : natural := (FRAME_HEIGHT - BOX_WIDTH);

constant BOX_X_MIN : natural := 0;
constant BOX_Y_MIN : natural := 0;

constant BOX_X_INIT : std_logic_vector(11 downto 0) := x"000";
constant BOX_Y_INIT : std_logic_vector(11 downto 0) := x"190"; --400

signal pxl_clk : std_logic;
signal active : std_logic;

signal h_cntr_reg : std_logic_vector(11 downto 0) := (others =>'0');
signal v_cntr_reg : std_logic_vector(11 downto 0) := (others =>'0');

signal h_sync_reg : std_logic := not(H_POL);
signal v_sync_reg : std_logic := not(V_POL);

signal h_sync_dly_reg : std_logic := not(H_POL);
signal v_sync_dly_reg : std_logic :=  not(V_POL);

signal vga_red_reg : std_logic_vector(3 downto 0) := (others =>'0');
signal vga_green_reg : std_logic_vector(3 downto 0) := (others =>'0');
signal vga_blue_reg : std_logic_vector(3 downto 0) := (others =>'0');

signal vga_red : std_logic_vector(3 downto 0);
signal vga_green : std_logic_vector(3 downto 0);
signal vga_blue : std_logic_vector(3 downto 0);

signal box_x_reg : std_logic_vector(11 downto 0) := BOX_X_INIT;
signal box_x_dir : std_logic := '1';
signal box_y_reg : std_logic_vector(11 downto 0) := BOX_Y_INIT;
signal box_y_dir : std_logic := '1';
signal box_cntr_reg : std_logic_vector(24 downto 0) := (others =>'0');

signal update_box : std_logic;
signal pixel_in_box : std_logic;

type rom_type is array (0 to 19) of std_logic_vector(19 downto 0);
-- ROM definition
constant BALL_ROM: rom_type :=
(
"00000111111111100000",
"00001111111111110000",
"00111111111111111100",
"00111111111111111100",
"01111111111111111110",
"11111111111111111111",
"11111111111111111111",
"11111111111111111111",
"11111111111111111111",
"11111111111111111111",
"11111111111111111111",
"11111111111111111111",
"11111111111111111111",
"11111111111111111111",
"11111111111111111111",
"01111111111111111110",
"00111111111111111100",
"00111111111111111100",
"00001111111111110000",
"00000111111111100000"
);

signal ball_point : std_logic;

type rom_type_2 is array (0 to 27) of std_logic_vector(27 downto 0);
-- ROM definition
constant img: rom_type_2 :=
(
"0000000000000000000000000000",
"0000000000000000000000000000",
"0000000000000000000000000000",
"0000000000000000000000000000",
"0000000000000000000000000000",
"0000000000001111111111110000",
"0000000011111111111111110000",
"0000000111111111111111100000",
"0000000111111111110000000000",
"0000000011111110110000000000",
"0000000001111100000000000000",
"0000000000011110000000000000",
"0000000000011110000000000000",
"0000000000001111110000000000",
"0000000000000111111000000000",
"0000000000000011111100000000",
"0000000000000001111100000000",
"0000000000000000011110000000",
"0000000000000011111110000000",
"0000000000001111111100000000",
"0000000000111111111000000000",
"0000000011111111110000000000",
"0000001111111111000000000000",
"0000111111111100000000000000",
"0000111111110000000000000000",
"0000000000000000000000000000",
"0000000000000000000000000000",
"0000000000000000000000000000");
signal img_pxl : std_logic;

begin
  
   
clk_div_inst : clk_wiz_0
  port map
   (-- Clock in ports
    CLK_IN1 => CLK_I,
    -- Clock out ports
    CLK_OUT1 => pxl_clk);

  
  ----------------------------------------------------
  -------         TEST PATTERN LOGIC           -------
  ----------------------------------------------------
process
begin
    if active = '1' then
        case sw is  
            when "0000" =>  
                vga_red   <= (others => '0');
                vga_green <= (others => '0');
                vga_blue  <= (others => '0');
            when "0001" =>  --Show solid Red
                vga_red   <= (others => '1');
                vga_green <= (others => '0');
                vga_blue  <= (others => '0');
            when "0010" =>   --Show solid Green
                vga_red   <= (others => '0');
                vga_green <= (others => '1');
                vga_blue  <= (others => '0');
            when "0011" => --Show solid Blue
                vga_red   <= (others => '0');
                vga_green <= (others => '0');
                vga_blue  <= (others => '1');
            when "0100" => --Show red green and blue horizontally
                if ( h_cntr_reg >= 0 and h_cntr_reg < ONE_THIRD_H) then
                    vga_red   <= (others => '1');
                    vga_green <= (others => '0');
                    vga_blue  <= (others => '0');
                elsif(h_cntr_reg >= ONE_THIRD_H and h_cntr_reg < 2*ONE_THIRD_H) then
                    vga_red   <= (others => '0');
                    vga_green <= (others => '1');
                    vga_blue  <= (others => '0');
                elsif(h_cntr_reg >= 2*ONE_THIRD_H and h_cntr_reg < 3*ONE_THIRD_H) then
                    vga_red   <= (others => '0');
                    vga_green <= (others => '0');
                    vga_blue  <= (others => '1');
                else
                    vga_red   <= (others => '0');
                    vga_green <= (others => '0');
                    vga_blue  <= (others => '0');
                end if;
            when "0101" => --White, yellow, cyan, green, magenta, red, blue, black 
                if ( h_cntr_reg >= 0 and h_cntr_reg < ONE_EIGTH_H) then
                    vga_red   <= (others => '1');
                    vga_green <= (others => '1');   --white
                    vga_blue  <= (others => '1');
                elsif(h_cntr_reg >= ONE_EIGTH_H and h_cntr_reg < 2*ONE_EIGTH_H) then
                    vga_red   <= (others => '1');
                    vga_green <= (others => '1');   --yellow
                    vga_blue  <= (others => '0'); 
                elsif(h_cntr_reg >= 2*ONE_EIGTH_H and h_cntr_reg < 3*ONE_EIGTH_H) then
                    vga_red   <= (others => '0');
                    vga_green <= (others => '1');   --cyan
                    vga_blue  <= (others => '1'); 
                elsif(h_cntr_reg >= 3*ONE_EIGTH_H and h_cntr_reg < 4*ONE_EIGTH_H) then
                    vga_red   <= (others => '0');
                    vga_green <= (others => '1');   --green
                    vga_blue  <= (others => '0'); 
                elsif(h_cntr_reg >= 4*ONE_EIGTH_H and h_cntr_reg < 5*ONE_EIGTH_H) then
                    vga_red   <= (others => '1');
                    vga_green <= (others => '0');   ---magenta 
                    vga_blue  <= (others => '1');
                elsif(h_cntr_reg >= 4*ONE_EIGTH_H and h_cntr_reg < 5*ONE_EIGTH_H) then
                    vga_red   <= (others => '1');
                    vga_green <= (others => '0');   --red
                    vga_blue  <= (others => '0');                   
                elsif(h_cntr_reg >= 6*ONE_EIGTH_H and h_cntr_reg < 7*ONE_EIGTH_H) then
                    vga_red   <= (others => '0');
                    vga_green <= (others => '0');   --blue
                    vga_blue  <= (others => '1'); 
                elsif(h_cntr_reg >= 7*ONE_EIGTH_H and h_cntr_reg < 8*ONE_EIGTH_H) then
                    vga_red   <= (others => '0');
                    vga_green <= (others => '0');   --black
                    vga_blue  <= (others => '0');
                else 
                    vga_red   <= (others => '0');
                    vga_green <= (others => '0');   --black
                    vga_blue  <= (others => '0');
                end if;  
            when "0110" => --8 shades of grey
                if ( h_cntr_reg >= 0 and h_cntr_reg < ONE_EIGTH_H) then
                    vga_red   <= (others => '1');
                    vga_green <= (others => '1');   
                    vga_blue  <= (others => '1');
                elsif(h_cntr_reg >= ONE_EIGTH_H and h_cntr_reg < 2*ONE_EIGTH_H) then
                    vga_red   <= "1110";
                    vga_green <= "1110";
                    vga_blue  <= "1110";
                elsif(h_cntr_reg >= 2*ONE_EIGTH_H and h_cntr_reg < 3*ONE_EIGTH_H) then
                    vga_red   <= "1100";
                    vga_green <= "1100";
                    vga_blue  <= "1100";
                elsif(h_cntr_reg >= 3*ONE_EIGTH_H and h_cntr_reg < 4*ONE_EIGTH_H) then
                    vga_red   <= "1010";
                    vga_green <= "1010";
                    vga_blue  <= "1010"; 
                elsif(h_cntr_reg >= 4*ONE_EIGTH_H and h_cntr_reg < 5*ONE_EIGTH_H) then
                    vga_red   <= "1000";
                    vga_green <= "1000"; 
                    vga_blue  <= "1000";
                elsif(h_cntr_reg >= 4*ONE_EIGTH_H and h_cntr_reg < 5*ONE_EIGTH_H) then
                    vga_red   <= "0110";
                    vga_green <= "0110";
                    vga_blue  <= "0110";              
                elsif(h_cntr_reg >= 6*ONE_EIGTH_H and h_cntr_reg < 7*ONE_EIGTH_H) then
                    vga_red   <= "0100";
                    vga_green <= "0100";
                    vga_blue  <= "0100"; 
                elsif(h_cntr_reg >= 7*ONE_EIGTH_H and h_cntr_reg < 8*ONE_EIGTH_H) then
                    vga_red   <= "0010";
                    vga_green <= "0010";
                    vga_blue  <= "0010";
                else 
                    vga_red   <= (others => '0');
                    vga_green <= (others => '0');   
                    vga_blue  <= (others => '0');
                end if;  
                
            when "0111" => --Press buttons Vertically
                case btn is 
                    when "0000" => --NO buttons pressed
                        vga_red <= h_cntr_reg(3 downto 0);
                        vga_green <= h_cntr_reg(3 downto 0);
                        vga_blue <= h_cntr_reg(3 downto 0);                   
                    when "0001" => --1st button
                        vga_red <= h_cntr_reg(4 downto 1);
                        vga_green <= h_cntr_reg(4 downto 1);
                        vga_blue <= h_cntr_reg(4 downto 1);
                    when "0010" => --2nd button
                        vga_red <= h_cntr_reg(5 downto 2);
                        vga_green <= h_cntr_reg(5 downto 2);
                        vga_blue <= h_cntr_reg(5 downto 2);
                    when "0100" => --3rd
                        vga_red <= h_cntr_reg(6 downto 3);
                        vga_green <= h_cntr_reg(6 downto 3);
                        vga_blue <= h_cntr_reg(6 downto 3);
                    when "1000" => --4th
                        vga_red <= h_cntr_reg(11 downto 8);
                        vga_green <= h_cntr_reg(11 downto 8);
                        vga_blue <= h_cntr_reg(11 downto 8);
                    when others =>
                        vga_red   <= (others => '0');
                        vga_green <= (others => '0');
                        vga_blue  <= (others => '0');
                    end case;
            when "1000" => --Press buttons Horizontally
                case btn is 
                    when "0000" => --NO buttons pressed
                        vga_red <= v_cntr_reg(3 downto 0);
                        vga_green <= v_cntr_reg(3 downto 0);
                        vga_blue <= v_cntr_reg(3 downto 0);                   
                    when "0001" => --1st button
                        vga_red <= v_cntr_reg(4 downto 1);
                        vga_green <= v_cntr_reg(4 downto 1);
                        vga_blue <= v_cntr_reg(4 downto 1);
                    when "0010" => --2nd button
                        vga_red <= v_cntr_reg(5 downto 2);
                        vga_green <= v_cntr_reg(5 downto 2);
                        vga_blue <= v_cntr_reg(5 downto 2);
                    when "0100" => --3rd
                        vga_red <= v_cntr_reg(6 downto 3);
                        vga_green <= v_cntr_reg(6 downto 3);
                        vga_blue <= v_cntr_reg(6 downto 3);
                    when "1000" => --4th
                        vga_red <= v_cntr_reg(11 downto 8);
                        vga_green <= v_cntr_reg(11 downto 8);
                        vga_blue <= v_cntr_reg(11 downto 8);
                    when others =>
                        vga_red   <= (others => '0');
                        vga_green <= (others => '0');
                        vga_blue  <= (others => '0');
                    end case;
                    --create checkard board pattern
            when "1001" =>
                case btn is 
                    when "0000" => --NO buttons pressed
                        vga_red <= (others => (v_cntr_reg(5) xor h_cntr_reg(5)));
                        vga_green <= (others => (v_cntr_reg(5) xor h_cntr_reg(5)));
                        vga_blue <= (others => (v_cntr_reg(5) xor h_cntr_reg(5)));                  
                    when "0001" => --1st button
                        vga_red <= (others => (v_cntr_reg(6) xor h_cntr_reg(6)));
                        vga_green <= (others => (v_cntr_reg(6) xor h_cntr_reg(6)));
                        vga_blue <= (others => (v_cntr_reg(6) xor h_cntr_reg(6)));
                    when "0010" => --2nd button
                        vga_red <= (others => (v_cntr_reg(7) xor h_cntr_reg(7)));
                        vga_green <= (others => (v_cntr_reg(7) xor h_cntr_reg(7)));
                        vga_blue <= (others => (v_cntr_reg(7) xor h_cntr_reg(7)));
                    when "0100" => --3rd
                        vga_red <= (others => (v_cntr_reg(8) xor h_cntr_reg(8)));
                        vga_green <= (others => (v_cntr_reg(8) xor h_cntr_reg(8)));
                        vga_blue <= (others => (v_cntr_reg(8) xor h_cntr_reg(8)));
                    when "1000" => --4th
                        vga_red <= (others => (v_cntr_reg(9) xor h_cntr_reg(9)));
                        vga_green <= (others => (v_cntr_reg(9) xor h_cntr_reg(9)));
                        vga_blue <= (others => (v_cntr_reg(9) xor h_cntr_reg(9)));
                    when others =>
                        vga_red   <= (others => '0');
                        vga_green <= (others => '0');
                        vga_blue  <= (others => '0');
                    end case;
             when "1010" =>
                case btn is 
                    when "0000" => --NO buttons pressed
                        vga_red <= v_cntr_reg(6 downto 3) and h_cntr_reg(6 downto 3);
                        vga_green <= v_cntr_reg(6 downto 3) and h_cntr_reg(6 downto 3);
                        vga_blue <= v_cntr_reg(6 downto 3) and h_cntr_reg(6 downto 3);              
                    when "0001" => --1st button
                        vga_red <= v_cntr_reg(6 downto 3) or h_cntr_reg(6 downto 3);
                        vga_green <= v_cntr_reg(6 downto 3) or h_cntr_reg(6 downto 3);
                        vga_blue <= v_cntr_reg(6 downto 3) or h_cntr_reg(6 downto 3);
                    when "0010" => --2nd button
                        vga_red <= v_cntr_reg(6 downto 3) xor h_cntr_reg(6 downto 3);
                        vga_green <= v_cntr_reg(6 downto 3) xor h_cntr_reg(6 downto 3);
                        vga_blue <= v_cntr_reg(6 downto 3) xor h_cntr_reg(6 downto 3);
                    when "0100" => --3rd
                        vga_red <= v_cntr_reg(7 downto 4) xor h_cntr_reg(7 downto 4);
                        vga_green <= v_cntr_reg(7 downto 4) xor h_cntr_reg(7 downto 4);
                        vga_blue <= v_cntr_reg(7 downto 4) xor h_cntr_reg(7 downto 4);
                    when "1000" => --4th
                        vga_red <= v_cntr_reg(8 downto 5) xor h_cntr_reg(8 downto 5);
                        vga_green <= v_cntr_reg(8 downto 5) xor h_cntr_reg(8 downto 5);
                        vga_blue <= v_cntr_reg(8 downto 5) xor h_cntr_reg(8 downto 5);
                    when others =>
                        vga_red   <= (others => '0');
                        vga_green <= (others => '0');
                        vga_blue  <= (others => '0');
                    end case; 
            when "1011" =>
                if pixel_in_box = '1' then
                    vga_red <= (others => ball_point);
                else
                    vga_red <= (others => '0');
                end if; 
            when "1100" =>
                vga_red   <= (others => img_pxl);
                vga_green <= (others => img_pxl);
                vga_blue  <= (others => img_pxl);                
            when others =>
                vga_red   <= (others => '0');
                vga_green <= (others => '0');
                vga_blue  <= (others => '0');
            end case;
    else 
        vga_red   <= (others => '0');
        vga_green <= (others => '0');
        vga_blue  <= (others => '0');
    end if;
end process;  
  
  
--  vga_red <= h_cntr_reg(5 downto 2) when (active = '1' and ((h_cntr_reg < 512 and v_cntr_reg < 256) and h_cntr_reg(8) = '1')) else
--              (others=>'1')         when (active = '1' and ((h_cntr_reg < 512 and not(v_cntr_reg < 256)) and not(pixel_in_box = '1'))) else
--              (others=>'1')         when (active = '1' and ((not(h_cntr_reg < 512) and (v_cntr_reg(8) = '1' and h_cntr_reg(3) = '1')) or
--                                          (not(h_cntr_reg < 512) and (v_cntr_reg(8) = '0' and v_cntr_reg(3) = '1')))) else
--              (others=>'0');
                
--  vga_blue <= h_cntr_reg(5 downto 2) when (active = '1' and ((h_cntr_reg < 512 and v_cntr_reg < 256) and  h_cntr_reg(6) = '1')) else
--              (others=>'1')          when (active = '1' and ((h_cntr_reg < 512 and not(v_cntr_reg < 256)) and not(pixel_in_box = '1'))) else 
--              (others=>'1')          when (active = '1' and ((not(h_cntr_reg < 512) and (v_cntr_reg(8) = '1' and h_cntr_reg(3) = '1')) or
--                                           (not(h_cntr_reg < 512) and (v_cntr_reg(8) = '0' and v_cntr_reg(3) = '1')))) else
--              (others=>'0');  
              
--  vga_green <= h_cntr_reg(5 downto 2) when (active = '1' and ((h_cntr_reg < 512 and v_cntr_reg < 256) and h_cntr_reg(7) = '1')) else
--              (others=>'1')           when (active = '1' and ((h_cntr_reg < 512 and not(v_cntr_reg < 256)) and not(pixel_in_box = '1'))) else 
--              (others=>'1')           when (active = '1' and ((not(h_cntr_reg < 512) and (v_cntr_reg(8) = '1' and h_cntr_reg(3) = '1')) or
--                                            (not(h_cntr_reg < 512) and (v_cntr_reg(8) = '0' and v_cntr_reg(3) = '1')))) else
--              (others=>'0');
              
 
 ------------------------------------------------------
 -------         MOVING BOX LOGIC                ------
 ------------------------------------------------------
  process (pxl_clk)
  begin
    if (rising_edge(pxl_clk)) then
      if (update_box = '1') then
        if (box_x_dir = '1') then
          box_x_reg <= box_x_reg + 1;
        else
          box_x_reg <= box_x_reg - 1;
        end if;
        if (box_y_dir = '1') then
          box_y_reg <= box_y_reg + 1;
        else
          box_y_reg <= box_y_reg - 1;
        end if;
      end if;
    end if;
  end process;
      
  process (pxl_clk)
  begin
    if (rising_edge(pxl_clk)) then
      if (update_box = '1') then
        if ((box_x_dir = '1' and (box_x_reg = BOX_X_MAX - 1)) or (box_x_dir = '0' and (box_x_reg = BOX_X_MIN + 1))) then
          box_x_dir <= not(box_x_dir);
        end if;
        if ((box_y_dir = '1' and (box_y_reg = BOX_Y_MAX - 1)) or (box_y_dir = '0' and (box_y_reg = BOX_Y_MIN + 1))) then
          box_y_dir <= not(box_y_dir);
        end if;
      end if;
    end if;
  end process;
  
  process (pxl_clk)
  begin
    if (rising_edge(pxl_clk)) then
      if (box_cntr_reg = (BOX_CLK_DIV - 1)) then
        box_cntr_reg <= (others=>'0');
      else
        box_cntr_reg <= box_cntr_reg + 1;     
      end if;
    end if;
  end process;
  
  update_box <= '1' when box_cntr_reg = (BOX_CLK_DIV - 1) else
                '0';
                
--  pixel_in_box <= '1' when (((h_cntr_reg >= box_x_reg) and (h_cntr_reg < (box_x_reg + BOX_WIDTH))) and
--                            ((v_cntr_reg >= box_y_reg) and (v_cntr_reg < (box_y_reg + BOX_WIDTH)))) else
--                  '0';
                  
ball_point <= BALL_ROM(conv_integer(v_cntr_reg(4 downto 0) - box_y_reg))(conv_integer(h_cntr_reg(4 downto 0) - box_x_reg)) when 
            (((h_cntr_reg >= box_x_reg) and (h_cntr_reg < (box_x_reg + BOX_WIDTH))) and
            ((v_cntr_reg >= box_y_reg) and (v_cntr_reg < (box_y_reg + BOX_WIDTH)))) else
            '0'; 
            
pixel_in_box <= ball_point when (((h_cntr_reg >= box_x_reg) and (h_cntr_reg < (box_x_reg + BOX_WIDTH))) and
                    ((v_cntr_reg >= box_y_reg) and (v_cntr_reg < (box_y_reg + BOX_WIDTH)))) else
             '0';
--img pxl change 4 downto 0 to 5 downto 1 ?
img_pxl <= img(conv_integer(v_cntr_reg(5 downto 1)))((conv_integer(h_cntr_reg(5 downto 1)))) when
           (((h_cntr_reg >= 0) and (h_cntr_reg < 56)) and ((v_cntr_reg >= 0) and (v_cntr_reg < 56))) else '0';                   
 ------------------------------------------------------
 -------         SYNC GENERATION                 ------
 ------------------------------------------------------
 
  process (pxl_clk)
  begin
    if (rising_edge(pxl_clk)) then
      if (h_cntr_reg = (H_MAX - 1)) then
        h_cntr_reg <= (others =>'0');
      else
        h_cntr_reg <= h_cntr_reg + 1;
      end if;
    end if;
  end process;
  
  process (pxl_clk)
  begin
    if (rising_edge(pxl_clk)) then
      if ((h_cntr_reg = (H_MAX - 1)) and (v_cntr_reg = (V_MAX - 1))) then
        v_cntr_reg <= (others =>'0');
      elsif (h_cntr_reg = (H_MAX - 1)) then
        v_cntr_reg <= v_cntr_reg + 1;
      end if;
    end if;
  end process;
  
  process (pxl_clk)
  begin
    if (rising_edge(pxl_clk)) then
      if (h_cntr_reg >= (H_FP + FRAME_WIDTH - 1)) and (h_cntr_reg < (H_FP + FRAME_WIDTH + H_PW - 1)) then
        h_sync_reg <= H_POL;
      else
        h_sync_reg <= not(H_POL);
      end if;
    end if;
  end process;
  
  
  process (pxl_clk)
  begin
    if (rising_edge(pxl_clk)) then
      if (v_cntr_reg >= (V_FP + FRAME_HEIGHT - 1)) and (v_cntr_reg < (V_FP + FRAME_HEIGHT + V_PW - 1)) then
        v_sync_reg <= V_POL;
      else
        v_sync_reg <= not(V_POL);
      end if;
    end if;
  end process;
  
  
  active <= '1' when ((h_cntr_reg < FRAME_WIDTH) and (v_cntr_reg < FRAME_HEIGHT))else
            '0';

  process (pxl_clk)
  begin
    if (rising_edge(pxl_clk)) then
      v_sync_dly_reg <= v_sync_reg;
      h_sync_dly_reg <= h_sync_reg;
      vga_red_reg <= vga_red;
      vga_green_reg <= vga_green;
      vga_blue_reg <= vga_blue;
    end if;
  end process;

  VGA_HS_O <= h_sync_dly_reg;
  VGA_VS_O <= v_sync_dly_reg;
  VGA_R <= vga_red_reg;
  VGA_G <= vga_green_reg;
  VGA_B <= vga_blue_reg;

end Behavioral;
