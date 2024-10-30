--  Demo of the Burrows-Wheeler data pre-compression algorithm.
--  See BWT_Enc and BWT_Dec for pre-processing files.
--

with BWT;

with Ada.Text_IO;

procedure BWT_Demo is
  use Ada.Text_IO;
begin
  Put ("Enter message: ");
  declare
    msg : String := Get_Line;
    index : Positive;
  begin
    Put_Line ("Encoding . . . . . . . . [" & msg & "]");
    BWT.Encode (msg, index, smart => True);
    New_Line;
    Put_Line ("Transformed message is . [" & msg & "]");
    BWT.Decode (msg, index);
    New_Line;
    Put_Line ("Decoded message is . . . [" & msg & "]");
  end;
  New_Line;
  Put ("Press Return");
  Skip_Line;
end BWT_Demo;
