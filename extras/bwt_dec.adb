--  Burrows-Wheeler data precompression tool - decoding
--
--  Caution: stack memory required is the SQUARE of the file size!...
--
--  See BWT_Enc for encoding and BWT_Demo for a simple demo.
--

with BWT;

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Streams.Stream_IO; use Ada.Streams.Stream_IO;
with Ada.Command_Line; use Ada.Command_Line;

procedure BWT_Dec is
  fi, fo : Ada.Streams.Stream_IO.File_Type;
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
