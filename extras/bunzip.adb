-- Standalone BZip2 decoder (for .bz2 files)

with BZip2.Decoding;

with Ada.Text_IO;                       use Ada.Text_IO;
with Ada.Streams.Stream_IO;             use Ada.Streams.Stream_IO;
with Ada.Command_Line;                  use Ada.Command_Line;
with Interfaces;                        use Interfaces;

procedure bunzip is

  f_in, f_out: Ada.Streams.Stream_IO.File_Type;

  type Buffer is array(Natural range <>) of Unsigned_8;

  -- Code with SE_Buffer below:
  -- workaround for the severe xxx'Read xxx'Write performance
  -- problems in the GNAT and ObjectAda compilers (as in 2009)
  -- This possible if and only if Byte = Stream_Element and
  -- arrays types are both packed.

  procedure Read( b: out Buffer ) is
    use Ada.Streams;
    Last: Stream_Element_Offset;
    SE_Buffer : Stream_Element_Array (1..b'Length);
    for SE_Buffer'Address use b'Address;
    pragma Import (Ada, SE_Buffer);
  begin
    --  Buffer'Read(Stream(f_in), b);
    --exception
    --  when Ada.Streams.Stream_IO.End_Error =>
    --    null;
    --    -- Nothing bad, just some garbage in the buffer
    --    -- after end of compressed code
    --
    Read(Stream(f_in).all, SE_Buffer, Last);
  end Read;

  procedure Write( b: in Buffer ) is
    use Ada.Streams;
    SE_Buffer : Stream_Element_Array (1..b'Length);
    for SE_Buffer'Address use b'Address;
    pragma Import (Ada, SE_Buffer);
  begin
    --
    -- Buffer'Write(Stream(f_out), b);
    --
    -- ^ Using this, instead of the lines below, more than doubles
    -- the whole run time (incl. decompression and slow reading!) on GNAT 2008
    -- and makes +60% more time on ObjectAda 7.2.2
    --
    Write(Stream(f_out).all, SE_Buffer);
  end Write;

  package My_BZip2 is new BZip2.Decoding
  (
    input_buffer_size  => 1024,
    output_buffer_size => 4096,
    Buffer             => Buffer,
    check_CRC          => True,
    Read               => Read,
    Write              => Write
  );

begin
  if Argument_Count = 0 then
    Put_Line("Usage: bunzip <file>");
    New_Line;
    Put_Line("Decompresses a bzip2 compressed file (.bz2)");
    New_Line;
    Put_Line("Output is written in the file: bunzip.out");
  else
    Open(f_in, In_File, Argument(1));
    Create(f_out,Out_File, "bunzip.out");
    My_BZip2.Decompress;
    Close(f_out);
  end if;
end bunzip;
