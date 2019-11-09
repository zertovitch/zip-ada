--  Burrows-Wheeler data precompression tool - encoding
--
--  Caution: stack memory required is the SQUARE of the file size!...
--
--  See BWT_Dec for decoding and BWT_Demo for a simple demo.
--

with BWT;

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Streams.Stream_IO; use Ada.Streams.Stream_IO;
with Ada.Command_Line; use Ada.Command_Line;

procedure BWT_Enc is
  fi, fo : Ada.Streams.Stream_IO.File_Type;
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
    BWT.Encode (msg, idx);
    String'Write (Stream (fo), msg);
    Put_Line (Current_Error, "Index: " & Integer'Image (idx));
  end;
  Close (fi);
  Close (fo);
end BWT_Enc;
