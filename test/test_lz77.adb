with Ada.Command_Line;                  use Ada.Command_Line;
with Ada.Sequential_IO;
with Ada.Text_IO;                       use Ada.Text_IO;
with LZ77;

procedure Test_LZ77 is

  package BIO is new Ada.Sequential_IO (LZ77.Byte);

  use LZ77, BIO;

  f_in, f_out: BIO.File_Type;
  f_dump: Ada.Text_IO.File_Type;

  function Read_byte return Byte is
    b: Byte;
  begin
    Read(f_in, b);
    return b;
  end Read_byte;

  function More_bytes return Boolean is
  begin
    return not End_Of_File(f_in);
  end More_bytes;

  ----- LZSS Parameters -----
  String_buffer_size : constant := 2**12;
  Look_Ahead         : constant := 2048;
  Threshold          : constant := 2;

  type Text_Buffer is array ( 0..String_buffer_size+Look_Ahead-1 ) of Byte;
  Text_Buf: Text_Buffer;
  R: Natural;

  procedure Write_and_record_literal( literal: Byte ) is
  begin
    Write(f_out, literal);
    Text_Buf(R):= literal;
    R:= (R+1) mod String_buffer_size;
  end Write_and_record_literal;

  procedure Emit_literal( literal: Byte ) is
  begin
    Write_and_record_literal(literal);
    --
    Put(f_dump, "Lit" & Byte'Image(literal));
    if literal in 32..126 then
      Put(f_dump, " '" & Character'Val(literal) & ''');
    end if;
    New_Line(f_dump);
  end Emit_literal;

  max_dis, max_len: Natural:= 0;
  min_dis, min_len: Natural:= Natural'Last;

  procedure Emit_code( distance, length: Natural ) is
    b: Byte;
    I: Natural;
  begin
    max_dis:= Natural'Max(max_dis,distance);
    max_len:= Natural'Max(max_len,length);
    min_dis:= Natural'Min(min_dis,distance);
    min_len:= Natural'Min(min_len,length);
    --
    Put_Line(f_dump, "DLE" & Integer'Image(distance) & Integer'Image(length));
    --
    --  Expand DL code:
    I:= (R - distance) mod String_buffer_size;
    for K in 0..length-1 loop
      b:= Text_Buf((I+K) mod String_buffer_size);
      Write_and_record_literal( b );
    end loop;
  end Emit_code;

  procedure My_LZ77 is
    new LZ77.Encode(
      String_buffer_size, Look_Ahead, Threshold,
      LZHuf,
      Read_byte, More_bytes,
      Emit_literal, Emit_code
    );

begin
  if Argument_Count < 1 then
    Open(f_in, In_File, "test/test_lz77.adb");
  else
    Open(f_in, In_File, Argument(1));
  end if;
  R:= String_buffer_size-Look_Ahead;
  Create(f_out, Out_File, "lz77.out");
  --  The dump.lz77 file is used in various places:
  --  - Input:
  --       LZ77_Stats
  --       Zip.Compress.Deflate (bypass_LZ77 mode)
  --  - Output:
  --       Test_LZ77
  --       UnZip.Decompress
  Create(f_dump, Out_File, "dump.lz77");
  My_LZ77;
  Close(f_in);
  Close(f_out);
  Close(f_dump);
  New_Line;
  Put_Line("Min distance:" & Integer'Image(min_dis));
  Put_Line("Max distance:" & Integer'Image(max_dis));
  Put_Line("Min length:" & Integer'Image(min_len));
  Put_Line("Max length:" & Integer'Image(max_len));
end Test_LZ77;
