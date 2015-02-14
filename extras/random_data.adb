-- Syntax: random_data [n [name]]
-- Default:
--   n = 100
--   name = random.bin

with Ada.Command_Line;                  use Ada.Command_Line;

with Ada.Numerics.Discrete_Random,
     Ada.Sequential_IO;

procedure Random_Data is
  n: Natural:= 100;
  type Byte is mod 2**8;
  package BIO is new Ada.Sequential_IO(Byte);
  use BIO;
  f: File_Type;
  package Byte_soup is new Ada.Numerics.Discrete_Random(Byte);
  use Byte_soup;
  cg: Generator;
begin
  if Argument_Count >= 1 then
    n:= Integer'Value(Argument(1));
  end if;
  if Argument_Count >= 2 then
    Create(f, Out_File, Argument(2));
  else
    Create(f, Out_File, "random.bin");
  end if;
  Reset(cg);
  for i in 1..n loop
    Write(f, Random(cg));
  end loop;
  Close(f);
end Random_Data;
