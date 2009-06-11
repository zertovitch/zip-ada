with Ada.Command_Line;                  use Ada.Command_Line;
with Ada.Sequential_IO;
with Ada.Text_IO;                       use Ada.Text_IO;
with Zip.LZ77;

procedure Test_LZ77 is

  package BIO is new Ada.Sequential_IO (Zip.Byte);

  use Zip, BIO;

  f_in, f_out: BIO.File_Type;

  function Read_byte return Byte is
    b: Byte;
  begin
    Read(f_in, b);
    return b;
  end Read_byte;

  function More_bytes return Boolean is
  begin
    return not End_of_File(f_in);
  end More_bytes;

  ----- LZSS Parameters -----
  String_buffer_size : constant := 2**12;
  Look_Ahead         : constant := 2048;
  Threshold          : constant := 2;

  type Text_Buffer is array ( 0..String_buffer_size+Look_Ahead-1 ) of Byte;
  Text_Buf: Text_Buffer;
  R: Natural;

  procedure Write_byte( b: Byte ) is
  begin
    Write(f_out, b);
    Text_Buf(R):= b;
    R:= (R+1) mod String_buffer_size;
  end Write_byte;

  max_dis, max_len: Natural:= 0;
  min_dis, min_len: Natural:= Natural'Last;

  procedure Write_code( distance, length: Natural ) is
    b: Byte;
    I: Natural;
  begin
    max_dis:= Natural'Max(max_dis,distance);
    max_len:= Natural'Max(max_len,length);
    min_dis:= Natural'Min(min_dis,distance);
    min_len:= Natural'Min(min_len,length);
    --  Put("[d:" & Integer'Image(distance) & ",l:" & Integer'Image(length) & ",");
    I:= (R - distance) mod String_buffer_size;
    for K in 0..length-1 loop
      b:= Text_Buf((I+K) mod String_buffer_size);
      Write_byte( b );
    end loop;
    --  Put("]");
  end Write_code;

  procedure My_LZ77 is
    new Zip.LZ77(
      String_buffer_size, Look_Ahead, Threshold,
      Read_byte, More_bytes,
      Write_byte, Write_code
    );

begin
  if Argument_Count < 1 then
    Open(f_in, In_File, "Test_LZ77.adb");
  else
    Open(f_in, In_File, Argument(1));
  end if;
  R:= String_buffer_size-Look_Ahead;
  Create(f_out,Out_File,"lz77.out");
  My_LZ77;
  Close(f_in);
  Close(f_out);
  New_Line;
  Put_Line("Max distance:" & Integer'Image(max_dis));
  Put_Line("Max length:  " & Integer'Image(max_len));
  Put_Line("Min distance:" & Integer'Image(min_dis));
  Put_Line("Min length:  " & Integer'Image(min_len));
end Test_LZ77;
