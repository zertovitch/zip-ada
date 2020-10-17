--  Syntax: random_data [n [name [min max]]]
--  Default:
--   n = 100
--   name = random.bin
--   min = 0
--   max = 255

with Ada.Command_Line;                  use Ada.Command_Line;

with Ada.Numerics.Discrete_Random,
     Ada.Sequential_IO;

with Interfaces;

procedure Random_Data is
  use Interfaces;
  n : Integer_64 := 100;
  type Byte is mod 2**8;
  package BIO is new Ada.Sequential_IO (Byte);
  use BIO;
  f : File_Type;
  min : Byte := 0;
  max : Byte := 255;
  procedure Spit_random is
    subtype Subyte is Byte range min .. max;
    package Byte_soup is new Ada.Numerics.Discrete_Random (Subyte);
    use Byte_soup;
    cg : Generator;
  begin
    Reset (cg);
    for i in 1 .. n loop
      Write (f, Random (cg));
    end loop;
  end Spit_random;
begin
  if Argument_Count >= 1 then
    n := Integer_64'Value (Argument (1));
  end if;
  if Argument_Count >= 2 then
    Create (f, Out_File, Argument (2));
  else
    Create (f, Out_File, "random.bin");
  end if;
  if Argument_Count >= 4 then
    min := Byte'Value (Argument (3));
    max := Byte'Value (Argument (4));
  end if;
  Spit_random;
  Close (f);
end Random_Data;
