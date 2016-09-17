-- Standalone, command-line, LZMA encoder (for .lzma files).

with LZMA.Encoding;

with Ada.Command_Line;                  use Ada.Command_Line;
with Ada.Text_IO;                       use Ada.Text_IO;
with Ada.Streams.Stream_IO;             use Ada.Streams.Stream_IO;
with Interfaces;                        use Interfaces;

procedure LZMA_Enc is

  subtype Data_Bytes_Count is Ada.Streams.Stream_IO.Count;

  subtype Byte is Unsigned_8;

  use LZMA, LZMA.Encoding;

  level                 : Compression_level           := Level_2;
  literal_context_bits  : Literal_context_bits_range  := 3;
  literal_position_bits : Literal_position_bits_range := 0;
  position_bits         : Position_bits_range         := 2;

  procedure Encode_LZMA_stream (s_in, s_out: Stream_Access) is
    EOS: Boolean:= False;
    mem_b: Byte:= Character'Pos('X');  --  delayed by 1 byte to catch the EOS

    --  NB: The Byte I/O below is not buffered, so it is very slow.
    --  You need to implement a circular buffer of type Stream_Element_Array for a fast I/O.
    --  For instance, see the BlockRead in the Zip package for how to do it.

    function Read_byte return Byte is  --  One dummy call to Read_byte is needed before compression
      prev_b: Byte;
    begin
      prev_b:= mem_b;
      Byte'Read(s_in, mem_b);
      return prev_b;
    exception
      when Ada.Streams.Stream_IO.End_Error =>
        EOS:= True;
        return prev_b;
    end Read_byte;

    function More_bytes return Boolean is
    begin
      return not EOS;
    end More_bytes;

    procedure Put_byte(b : Byte) is
    begin
      Byte'Write(s_out, b);
    end Put_byte;

    procedure LZMA_Encode is new LZMA.Encoding.Encode(Read_byte, More_bytes, Put_byte);

    dummy: Byte:= Read_byte;  --  Consume the initial 'X'

  begin
    -- Whole processing here:
    LZMA_Encode(
      level,
      literal_context_bits,
      literal_position_bits,
      position_bits,
      dictionary_size => 2**20,
      uncompressed_size_info => True
    );
  end Encode_LZMA_stream;

  f_in, f_out: Ada.Streams.Stream_IO.File_Type;

  procedure Print_Data_Bytes_Count(title: String; v: Data_Bytes_Count) is
    package CIO is new Integer_IO(Data_Bytes_Count);
  begin
    Put(title);
    Put(" : ");
    CIO.Put(v, 0);
    Put(" bytes");
    New_Line;
  end Print_Data_Bytes_Count;

  bench: Boolean:= False;
  z: constant:= Character'Pos('0');

begin
  New_Line;
  Put_Line("LZMA_Enc: a standalone LZMA encoder.");
  if Argument_Count = 0 then
    Put_Line("Use: lzma_enc infile outfile [options]");
    New_Line;
    Put_Line("NB: - The "".lzma"" extension automatically added to outfile.");
    Put_Line("    - The I/O is not buffered => may be slow. Use the ZipAda tool for fast I/O.");
    New_Line;
    Put_Line("Options: -b: benchmark LZ77's and the context parameters (675 .lzma output files!)");
    return;
  elsif Argument_Count < 2 then
    Put_Line("You must specify at least two parameters");
    return;
  end if;
  for i in 3..Argument_Count loop
    bench:= bench or Argument(i) = "-b";
  end loop;
  if bench then
    for lc in reverse Literal_context_bits_range loop
      for lp in reverse Literal_position_bits_range loop
        for pb in reverse Position_bits_range loop
          for lv in Level_1 .. Level_3 loop
            Open(f_in, In_File, Argument(1));
            Create(f_out, Out_File,
              Argument(2) & '_' &
              Character'Val(z+lc) & Character'Val(z+lp) & Character'Val(z+pb) &
              "_l" &
              Character'Val(z+1+Compression_level'Pos(lv)) & ".lzma"
            );
            literal_context_bits  := lc;
            literal_position_bits := lp;
            position_bits         := pb;
            level                 := lv;
            Encode_LZMA_stream(Stream(f_in), Stream(f_out));
            Close(f_in);
            Close(f_out);
          end loop;
        end loop;
      end loop;
    end loop;
  else
    Open(f_in, In_File, Argument(1));
    Create(f_out, Out_File, Argument(2) & ".lzma");
    Encode_LZMA_stream(Stream(f_in), Stream(f_out));
    New_Line;
    Print_Data_Bytes_Count("Read    ", Data_Bytes_Count(Index(f_in) - 1));
    Print_Data_Bytes_Count("Written ", Data_Bytes_Count(Index(f_out) - 1));
    Close(f_in);
    Close(f_out);
  end if;

end LZMA_Enc;
