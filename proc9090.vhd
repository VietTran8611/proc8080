
LIBRARY ieee; USE ieee.std_logic_1164.ALL; USE ieee.numeric_std.ALL;



entity proc9090 is
--  Port ( );
end proc9090;


ARCHITECTURE Behavioral OF proc9090 IS 
type myArray is array (0 to 4) of unsigned(7 downto 0); 
signal Mem:myArray:=(
"00011110", "10001010",  -- MVI E,8a
"00111110", "01111011",  -- MVI A,7b
"11011101"-- MUL     (Opcode DD is unused on 8080, so use it for MUL on the 9090)
             
 -- later, you can test your DIV instruction here...  (Let's use opcode ED for DIV)
 --
);
type myRegs is array (0 to 7) of unsigned(7 downto 0); 
signal Reg:myRegs:=(x"00",x"00",x"00",x"00",x"00",x"00",x"00",x"00");
signal clk : bit := '1'; -- start high, so clock rise will happen after simulation initializes
signal PC,IR,Tc,A: unsigned(7 downto 0) :=x"00";
signal HL,DE: unsigned(15 downto 0);
signal Cy, Zf, Sf : unsigned(0 downto 0) := "0";

-- add any signals needed for your MUL and DIV here....

begin 
clk <= not clk after 1 ps;
process(clk)
variable src,dst, dsrc,ddst : integer;
variable tmp8: unsigned(7 downto 0);
variable tmp17: unsigned(16 downto 0);
begin
if (clk'event and clk='1') then 
   Tc <= Tc+1; -- cycle counter will increment on clock rise.  Important Note: LHS (after clock) <= RHS (before clock)
   CASE Tc is
     when x"00" => IR <= Mem(to_integer(PC));   -- Fetch opcode in Tc "00" cycle
                   PC <= PC+1;
     when x"01" => 
        src := to_integer(IR(2 downto 0));
        dst := to_integer(IR(5 downto 3));
        CASE IR(7 downto 6) is
          when "00" => 
            if IR(2 downto 0)="100" then Reg(dst)<=Reg(dst)+1; Zf<="0"; if Reg(dst)+1=x"00" then Zf<="1"; end if; end if;  -- INR dst
            if IR(2 downto 0)="101" then Reg(dst)<=Reg(dst)-1; Zf<="0"; if Reg(dst)-1=x"00" then Zf<="1"; end if; end if;  -- DCR dst
            if IR(5 downto 0)="010111" then 
                           Reg(7) <= Reg(7)(6 downto 0) & Cy;  --  RAL: Rotate Accumulator left through carry bit Cy
                           Cy<=Reg(7)(7 downto 7); -- set after clock
                           end if;
            if IR(5 downto 0)="011111" then 
                           Reg(7) <= Cy & Reg(7)(7 downto 1); --  RAR: Rotate Accumulator right through carry bit Cy
                           Cy<=Reg(7)(0 downto 0); 
                           end if;
            if IR(3 downto 0)="1001" then                             -- DAD yy
                           dsrc := to_integer(IR(5 downto 4))*2;
                           tmp17 := ("0" & Reg(4) & Reg(5)) + ("0" & Reg(dsrc) & Reg(dsrc+1));
                           Reg(5) <= tmp17(7 downto 0);
                           Reg(4) <= tmp17(15 downto 8);
                           Cy <= tmp17(16 downto 16);
                           end if;
            if IR(2 downto 0)="110" then Reg(dst) <= Mem(to_integer(PC));  --  MVI  dst,data8
                                                PC <= PC+1;  end if;
            if IR(3 downto 0)="0001" then 
                           ddst := to_integer(IR(5 downto 4))*2;
                           Reg(ddst+1)<=Mem(to_integer(PC));       -- LXI
                           PC <= PC+1; -- LXI doesn't clear Tc yet, since more cycles follow
            else Tc <= x"00"; end if;
          when "01" => Reg(dst) <= Reg(src);
             Tc<=x"00"; 
          when "10" =>  
             if(IR(5 downto 3)="000") then Reg(7)<=Reg(7)+Reg(src); end if;  -- ADD src  -- add register "src" to accumulator
             -- all of the other vanilla ALU operators go here...  But I found I didn't need them for this assignment
             -- the following line implements the CMP instruction:
             if(IR(5 downto 3)="111") then tmp8:=Reg(7)-Reg(src); Sf<=tmp8(7 downto 7); Zf<="0"; if(tmp8=x"00") then Zf<="1"; end if; end if;
             Tc<=x"00"; 
          when "11" => 
           if(IR(5 downto 0))="011101" then  --  multiply stuff goes here (does not reset Tc)
                
                              -- first cycle of multiply goes here
                            HL <= x"0000";
                            DE(15 downto 8) <= x"00";
                            DE(7 downto 0) <= Reg(3);
                            A <= Reg(7);                          
           elsif(IR(5 downto 0))="101101" then -- DIV stuff goes here (does not reset Tc)

                              -- first cycle of your divider goes here

           else
             if IR(5 downto 0)="111110" then tmp8:=Reg(7)-Mem(to_integer(PC)); PC<=PC+1; Sf<=tmp8(7 downto 7); Zf<="0"; if(tmp8=x"00") then Zf<="1"; end if; end if; --CPI
             if IR(5 downto 0)="101011" then Reg(4)<=Reg(2); Reg(2)<=Reg(4); Reg(5)<=Reg(3); Reg(3)<=Reg(5); end if; -- XCHG
             if IR(5 downto 0)="000011" then  PC<=Mem(to_integer(PC));  end if;                                      -- JMP addr8
             if IR(5 downto 0)="010010" then if Cy="0" then PC<=Mem(to_integer(PC)); else PC<=PC+1; end if; end if;  -- JNC addr8
             if IR(5 downto 0)="011010" then if Cy="1" then PC<=Mem(to_integer(PC)); else PC<=PC+1; end if; end if;  -- JC addr8
             if IR(5 downto 0)="000010" then if Zf="0" then PC<=Mem(to_integer(PC)); else PC<=PC+1; end if; end if;  -- JNZ addr8
             if IR(5 downto 0)="001010" then if Zf="1" then PC<=Mem(to_integer(PC)); else PC<=PC+1; end if; end if;  -- JZ addr8
             if IR(5 downto 0)="110010" then if Sf="0" then PC<=Mem(to_integer(PC)); else PC<=PC+1; end if; end if;  -- JP addr8
             if IR(5 downto 0)="111010" then if Sf="1" then PC<=Mem(to_integer(PC)); else PC<=PC+1; end if; end if;  -- JM addr8
             Tc<=x"00";     -- the Jump addresses on Proc9090 are 1-byte (not 2-bytes like on the 8080)
             end if;
          when others => 
          end case;
     when x"02" => 
        CASE IR(7 downto 6) is
            when "11" =>
                if(IR(5 downto 0))="011101" then  --  multiply stuff goes here (does not reset Tc));
                    if A(0) = '1' then
                        HL <=HL+DE;
                    end if;
                    DE <= DE sll 1;
                    A <= A srl 1; 
                end if; 
            when others =>    
        end case;
      when x"03" =>
        CASE IR(7 downto 6) is
            when "11" =>
                if(IR(5 downto 0))="011101" then 
                    if A(0) = '1' then
                        HL <=HL+DE;
                    end if;
                    DE <= DE sll 1;
                    A <= A srl 1; 
                end if;  
            when others =>   
        end case;
      when x"04" => 
        CASE IR(7 downto 6) is
            when "11" =>
                if(IR(5 downto 0))="011101" then  --  multiply stuff goes here (does not reset Tc));
                    if A(0) = '1' then
                        HL <=HL+DE;
                    end if;
                    DE <= DE sll 1;
                    A <= A srl 1; 
                end if;
             when others =>     
        end case;
      when x"05" => 
        CASE IR(7 downto 6) is
            when "11" =>
                if(IR(5 downto 0))="011101" then  --  multiply stuff goes here (does not reset Tc));
                    if A(0) = '1' then
                        HL <=HL+DE;
                    end if;
                    DE <= DE sll 1;
                    A <= A srl 1; 
                end if; 
            when others =>   
        end case;
      when x"06" => 
        CASE IR(7 downto 6) is
            when "11" =>
                if(IR(5 downto 0))="011101" then  --  multiply stuff goes here (does not reset Tc));
                    if A(0) = '1' then
                        HL <=HL+DE;
                    end if;
                    DE <= DE sll 1;
                    A <= A srl 1; 
                end if; 
            when others =>    
        end case;
       when x"07" => 
        CASE IR(7 downto 6) is
            when "11" =>
                if(IR(5 downto 0))="011101" then  --  multiply stuff goes here (does not reset Tc));
                    if A(0) = '1' then
                        HL <=HL+DE;
                    end if;
                    DE <= DE sll 1;
                    A <= A srl 1; 
                end if; 
            when others =>    
        end case;
      when x"08" => 
        CASE IR(7 downto 6) is
            when "11" =>
                if(IR(5 downto 0))="011101" then  --  multiply stuff goes here (does not reset Tc));
                    if A(0) = '1' then
                        HL <=HL+DE;
                    end if;
                    DE <= DE sll 1;
                    A <= A srl 1; 
                end if; 
            when others =>    
        end case;
      when x"09" => 
        CASE IR(7 downto 6) is
            when "11" =>
                if(IR(5 downto 0))="011101" then  --  multiply stuff goes here (does not reset Tc));
                    if A(0) = '1' then
                        HL <=HL+DE;
                    end if;
                    DE <= DE sll 1;
                    A <= A srl 1; 
                end if; 
            when others =>    
        end case;
     when others => ---  MUL, DIV, and LXI  will flow here when Tc="02" or higher
        CASE IR(7 downto 6) is
          when "00" =>
             if IR(3 downto 0)="0001" then 
                           Reg(ddst)<=Mem(to_integer(PC)); -- second byte of LXI (little Endian)
                           PC <= PC+1;  end if;
             Tc<=x"00"; -- now after second byte, can reset clock
          when "11" => 
           if(IR(5 downto 0))="011101" then  --  multiply stuff goes here (does not reset Tc)
                Reg(5) <= HL(7 downto 0);
                Reg(4) <= HL(15 downto 8);

           elsif(IR(5 downto 0))="101101" then -- DIV stuff goes here (does not reset Tc)

               -- put the second cycle of your DIV implementation here

         --  Zf<="0"; if (remainder=x"00") then Zf<="1"; end if;    end if;   -- This line will set Zf to 1 if the remainder is zero for DIV
           end if;
           Tc<=x"00"; -- now can reset clock
          when others =>
        end case;  
    -- when others => 
     end case;
end if; end process; 
end Behavioral;
