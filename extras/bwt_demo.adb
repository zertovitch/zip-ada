--  Demo of the Burrows-Wheeler data pre-compression algorithm.
--  See BWT_Enc and BWT_Dec for pre-processing files.
--

with BWT;

with Ada.Calendar, Ada.Text_IO;

procedure BWT_Demo is
  use Ada.Calendar, Ada.Text_IO;
  t0, t1 : Time;
begin
  Put ("Enter message: ");
  declare
    --  "BIACI" fails the Suffix Array method with non-duplicated
    --  message and a terminator symbol.
    msg : String := Get_Line;
    ori : constant String := msg;
    index : Positive;
  begin
    for method in BWT.Encoding_Method loop
      New_Line;
      Put_Line ("BWT Method: " & method'Image);
      New_Line;
      Put_Line ("  Encoding . . . . . . . . [" & msg & "]");
      t0 := Clock;
      BWT.Encode (msg, index, method);
      t1 := Clock;
      New_Line;
      Put_Line ("  Transformed message is . [" & msg & "]");
      BWT.Decode (msg, index);
      Put_Line ("  Index:" & index'Image);
      New_Line;
      Put_Line ("  Decoded message is . . . [" & msg & "]");
      if msg = ori then
        Put_Line ("---> Correct");
      else
        Put_Line ("---> INCORRECT");
        raise Program_Error;
      end if;
      New_Line;
      Put_Line ("  Encoding time:" & Duration'Image (t1 - t0));
    end loop;
  end;
  New_Line;
  Put ("Press Return");
  Skip_Line;
end BWT_Demo;
