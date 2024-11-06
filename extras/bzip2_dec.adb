--  Standalone BZip2 decoder (for .bz2 files)

with BZip2.Decoding;

with Ada.Text_IO,
     Ada.Streams.Stream_IO,
     Ada.Command_Line;

procedure BZip2_Dec is

  use Ada.Streams.Stream_IO, BZip2;

  f_in, f_out : Ada.Streams.Stream_IO.File_Type;

  --  NB: The Byte I/O below is not buffered, so it is very slow.
  --  You need to implement a circular buffer of type Stream_Element_Array for a fast I/O.
  --  For instance, see the BlockRead in the Zip package for how to do it.

  function Demo_Read_Byte return Byte is
    b : Byte;
  begin
    Byte'Read (Stream (f_in), b);
    return b;
  end Demo_Read_Byte;

  procedure Demo_Write_Byte (b : Byte) is
  begin
    Byte'Write (Stream (f_out), b);
  end Demo_Write_Byte;

  package My_BZip2 is new BZip2.Decoding
    (Demo_Read_Byte, Demo_Write_Byte, check_CRC => True);

  use Ada.Command_Line, Ada.Text_IO;

  default : constant String := "bunzip.out";

begin
  if Argument_Count = 0 then
    Put_Line ("BZip2_Dec: a standalone BZip2 decoder.");
    New_Line;
    Put_Line ("Usage: bzip2_dec infile.bz2 {outfile}");
    New_Line;
    Put_Line ("Decompresses a bzip2 compressed file (.bz2)");
    Put_Line ("Output: if outfile is not given, the name """ & default & """ will be used");
  else
    Open (f_in, In_File, Argument (1));
    Create (f_out, Out_File, (if Argument_Count > 1 then Argument (2) else default));
    My_BZip2.Decompress;
    Close (f_out);
  end if;
end BZip2_Dec;
