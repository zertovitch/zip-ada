--  Standalone BZip2 decoder (for .bz2 files)

with BZip2.Decoding;

with Ada.Text_IO,
     Ada.Streams.Stream_IO,
     Ada.Command_Line;

with Interfaces;

procedure BZip2_Dec is

  use Ada.Streams.Stream_IO;

  f_in, f_out : Ada.Streams.Stream_IO.File_Type;

  type Buffer is array (Natural range <>) of Interfaces.Unsigned_8;

  --  Code with SE_Buffer below:
  --  workaround for the severe xxx'Read xxx'Write performance
  --  problems in the GNAT and ObjectAda compilers (as in 2009)
  --  This possible if and only if Byte = Stream_Element and
  --  arrays types are both packed.

  procedure BU_Read (b : out Buffer) is
    use Ada.Streams;
    Last : Stream_Element_Offset;
    SE_Buffer : Stream_Element_Array (1 .. b'Length);
    for SE_Buffer'Address use b'Address;
    pragma Import (Ada, SE_Buffer);
  begin
    --    Buffer'Read(Stream(f_in), b);
    --  exception
    --    when Ada.Streams.Stream_IO.End_Error =>
    --      null;
    --      -- Nothing bad, just some garbage in the buffer
    --      -- after end of compressed code
    --
    Read (Stream (f_in).all, SE_Buffer, Last);
  end BU_Read;

  procedure BU_Write (b : in Buffer) is
    use Ada.Streams;
    SE_Buffer : Stream_Element_Array (1 .. b'Length);
    for SE_Buffer'Address use b'Address;
    pragma Import (Ada, SE_Buffer);
  begin
    --
    --  Buffer'Write(Stream(f_out), b);
    --
    --  ^ Using this, instead of the lines below, more than doubles
    --  the whole run time (incl. decompression and slow reading!) on GNAT 2008
    --  and makes +60% more time on ObjectAda 7.2.2
    --
    Write (Stream (f_out).all, SE_Buffer);
  end BU_Write;

  package My_BZip2 is new BZip2.Decoding
    (input_buffer_size  => 1024,
     output_buffer_size => 4096,
     Buffer             => Buffer,
     check_CRC          => True,
     Read               => BU_Read,
     Write              => BU_Write);

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
