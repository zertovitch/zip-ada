--  Standalone, command-line, BZip2 encoder (for .bzip2 files).

with BZip2.Encoding;

with Ada.Command_Line,
     Ada.Text_IO,
     Ada.Streams.Stream_IO;

procedure BZip2_Enc is

  use BZip2, BZip2.Encoding;

  level : Positive := 4;

  procedure Encode_BZip2_Stream (s_in, s_out : Ada.Streams.Stream_IO.Stream_Access) is
    EOS : Boolean := False;
    mem_b : Byte := Character'Pos ('X');  --  delayed by 1 byte to catch the EOS (End-Of-Stream)

    --  NB: The Byte I/O below is not buffered, so it is very slow.
    --  You need to implement a circular buffer of type Stream_Element_Array for a fast I/O.
    --  For instance, see the BlockRead in the Zip package for how to do it.

    function Read_Byte return Byte is  --  One dummy call to Read_byte is needed before compression
      prev_b : Byte;
    begin
      prev_b := mem_b;
      Byte'Read (s_in, mem_b);
      return prev_b;
    exception
      when Ada.Streams.Stream_IO.End_Error =>
        EOS := True;
        return prev_b;
    end Read_Byte;

    function More_Bytes return Boolean is
    begin
      return not EOS;
    end More_Bytes;

    procedure Put_Byte (b : Byte) is
    begin
      Byte'Write (s_out, b);
    end Put_Byte;

    procedure BZip2_Encode is new BZip2.Encoding.Encode (Read_Byte, More_Bytes, Put_Byte);

    dummy : Byte := Read_Byte;  --  Consume the initial 'X'

  begin
    --  Whole processing is done here:
    BZip2_Encode
      (case level is
         when 1      => block_50k,
         when 2      => block_100k,
         when 3      => block_400k,
         when others => block_900k);
  end Encode_BZip2_Stream;

  use Ada.Streams.Stream_IO;

  f_in, f_out : File_Type;

  use Ada.Text_IO;

  procedure Print_Data_Bytes_Count (title : String; v : Data_Bytes_Count) is
    package CIO is new Integer_IO (Data_Bytes_Count);
  begin
    Put (title);
    Put (" : ");
    CIO.Put (v, 0);
    Put (" bytes");
    New_Line;
  end Print_Data_Bytes_Count;

  use Ada.Command_Line;

begin
  New_Line;
  Put_Line ("BZip2_Enc: a standalone BZip2 encoder.");
  if Argument_Count = 0 then
    Put_Line ("Use: bzip2_enc infile outfile [options]");
    New_Line;
    Put_Line ("NB: - The "".bz2"" extension is automatically added to outfile.");
    Put_Line ("    - The I/O is not buffered => may be slow. Use the ZipAda tool for fast I/O.");
    New_Line;
    Put_Line ("Options: -n, n in 1 .. 4 : strength");
    New_Line;
    Put ("Press Return");
    Skip_Line;
    return;
  elsif Argument_Count < 2 then
    Put_Line ("You must specify at least two parameters");
    return;
  end if;
  for i in 2 .. Argument_Count loop
    declare
      arg : constant String := Argument (i);
    begin
      if arg (arg'First) = '-' then
        if i = 2 then
          Put_Line ("Option needs to be 3rd or later parameter");
          return;
        end if;
        if arg (arg'Last) in '1' .. '4' then
          level := Character'Pos (arg (arg'Last)) - Character'Pos ('0');
        end if;
      end if;
    end;
  end loop;
  Open (f_in, In_File, Argument (1));
  Create (f_out, Out_File, Argument (2) & ".bz2");
  Encode_BZip2_Stream (Stream (f_in), Stream (f_out));
  New_Line;
  Print_Data_Bytes_Count ("Read    ", Data_Bytes_Count (Index (f_in) - 1));
  Print_Data_Bytes_Count ("Written ", Data_Bytes_Count (Index (f_out) - 1));
  Close (f_in);
  Close (f_out);

end BZip2_Enc;
