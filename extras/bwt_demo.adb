--  Demo of the Burrows-Wheeler data precompression algorithm.
--  See BWT_Enc and BWT_Dec for preprocessing files.
--

with BWT;

with Ada.Text_IO; use Ada.Text_IO;

procedure BWT_Demo is
begin
  Put ("Enter message:");
  declare
    msg : String := Get_Line;
    index : Positive;
  begin
    Put_Line ("Encoding [" & msg & "]");
    BWT.Encode (msg, index);
    Put_Line ("Transformed message is [" & msg & "]");
    BWT.Decode (msg, index);
    Put_Line ("Decoded message is [" & msg & "]");
  end;
end BWT_Demo;
