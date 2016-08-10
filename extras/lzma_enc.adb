--------------------------------
-- DRAFT - not yet functional --
--------------------------------

-- Standalone, command-line, experimental LZMA encoder (for .lzma files).

with LZMA.Encoding;

with Ada.Command_Line;                  use Ada.Command_Line;
with Ada.Text_IO;                       use Ada.Text_IO;
with Ada.Streams.Stream_IO;             use Ada.Streams.Stream_IO;
with Interfaces;                        use Interfaces;

with Ada.Exceptions;
with GNAT.Traceback.Symbolic;

procedure LZMA_Enc is

  subtype Data_Bytes_Count is Ada.Streams.Stream_IO.Count;

  subtype Byte is Unsigned_8;

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

    procedure LZMA_Encode is
      new LZMA.Encoding.Encode(LZMA.Encoding.Level_1, Read_byte, More_bytes, Put_byte);

    dummy: Byte:= Read_byte;

  begin
    -- Whole processing here:
    LZMA_Encode;
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

begin
  New_Line;
  Put_Line("LZMA_Enc: a standalone LZMA encoder.");
  Put_Line("DRAFT - not yet functional");
  if Argument_Count = 0 then
    Put_Line("Use: lzma_enc infile outfile.lzma");
    return;
  elsif Argument_Count /= 2 then
    Put_Line("You must specify two parameters");
    return;
  end if;
  Open(f_in, In_File, Argument(1));
  Create(f_out, Out_File, Argument(2));
  Encode_LZMA_stream(Stream(f_in), Stream(f_out));
  New_Line;

  Print_Data_Bytes_Count("Read    ", Data_Bytes_Count(Index(f_in) - 1));
  Print_Data_Bytes_Count("Written ", Data_Bytes_Count(Index(f_out) - 1));

  Close(f_in);
  Close(f_out);

exception
  when E: others =>
    New_Line(Standard_Error);
    Put_Line(Standard_Error,
             "--------------------[ Unhandled exception ]-----------------");
    Put_Line(Standard_Error, " > Name of exception . . . . .: " &
             Ada.Exceptions.Exception_Name(E) );
    Put_Line(Standard_Error, " > Message for exception . . .: " &
             Ada.Exceptions.Exception_Message(E) );
    Put_Line(Standard_Error, " > Trace-back of call stack: " );
    Put_Line(Standard_Error, GNAT.Traceback.Symbolic.Symbolic_Traceback(E) );

end LZMA_Enc;
