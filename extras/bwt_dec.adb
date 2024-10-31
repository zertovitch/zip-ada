--  Burrows-Wheeler data pre-compression tool - decoding
--
--  Caution: stack memory required is the SQUARE of the file size!...
--
--  See BWT_Enc for encoding and BWT_Demo for a simple demo.
--

with BWT;

with Ada.Text_IO,
     Ada.Streams.Stream_IO,
     Ada.Command_Line;

procedure BWT_Dec is
  use Ada.Streams.Stream_IO;
  fi, fo : File_Type;
  use Ada.Command_Line, Ada.Text_IO;
begin
  if Argument_Count < 3 then
    Put_Line ("Syntax: bwt_dec infile index outfile");
    Put_Line ("The parameter 'index' is the number given by the encoder, bwt_enc.");
    return;
  end if;
  Open (fi, In_File, Argument (1));
  Create (fo, Out_File, Argument (3));
  declare
    msg : String (1 .. Integer (Size (fi)));
    idx : constant Positive := Integer'Value (Argument (2));
  begin
    String'Read (Stream (fi), msg);
    BWT.Decode (msg, idx);
    String'Write (Stream (fo), msg);
  end;
  Close (fi);
  Close (fo);
end BWT_Dec;
