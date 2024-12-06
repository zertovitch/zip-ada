--  Burrows-Wheeler data pre-compression tool - encoding
--
--  See BWT_Dec for decoding and BWT_Demo for a simple demo.
--

with BWT;

with Ada.Calendar,
     Ada.Command_Line,
     Ada.Text_IO,
     Ada.Streams.Stream_IO;

procedure BWT_Enc is
  use Ada.Streams.Stream_IO;
  fi, fo : File_Type;
  use Ada.Calendar, Ada.Command_Line, Ada.Text_IO;
  t0, t1 : Time;
begin
  if Argument_Count < 2 then
    Put_Line ("Syntax: bwt_enc infile outfile");
    return;
  end if;
  Open (fi, In_File, Argument (1));
  Create (fo, Out_File, Argument (2));
  declare
    msg : String (1 .. Integer (Size (fi)));
    idx : Positive;
  begin
    String'Read (Stream (fi), msg);
    t0 := Clock;
    BWT.Encode (msg, idx, BWT.suffix_array);
    t1 := Clock;
    String'Write (Stream (fo), msg);
    Put_Line (Current_Error, "Index: " & idx'Image);
    Put_Line (Current_Error, "Encoding time:" & Duration'Image (t1 - t0));
  end;
  Close (fi);
  Close (fo);
end BWT_Enc;
