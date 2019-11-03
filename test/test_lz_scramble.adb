with Ada.Text_IO;                       use Ada.Text_IO;
with LZ_Scramble;

procedure Test_LZ_Scramble is
begin
  for i in 1 .. 78 loop
    Put_Line (LZ_Scramble (i));
  end loop;
end Test_LZ_Scramble;
