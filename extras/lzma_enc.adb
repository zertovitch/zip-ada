--------------------------------
-- DRAFT - not yet functional --
--------------------------------

-- Standalone, command-line, experimental LZMA encoder (for .lzma files).

with LZ77;

with Ada.Command_Line;                  use Ada.Command_Line;
with Ada.Text_IO;                       use Ada.Text_IO;
with Ada.Streams.Stream_IO;             use Ada.Streams.Stream_IO;
with Interfaces;                        use Interfaces;
with System;

with Ada.Exceptions;
with GNAT.Traceback.Symbolic;

procedure LZMA_Enc is

  subtype Data_Bytes_Count is Ada.Streams.Stream_IO.Count;

  subtype Byte is Unsigned_8;
  subtype UInt16 is Unsigned_16;
  subtype UInt32 is Unsigned_32;
  type Unsigned is mod 2 ** System.Word_Size;

  subtype LC_range is Integer range 0..8;
  subtype LP_range is Integer range 0..4;
  subtype PB_range is Integer range 0..4;

  LZMA_DIC_MIN : constant := 2 ** 12; -- 4096

  type LZMA_Params_Info is record
    header_with_size     : Boolean;  -- header is either 5 bytes or 13 bytes (with size specified)
    unpackSize           : Data_Bytes_Count;
    unpackSizeDefined    : Boolean;
    markerIsMandatory    : Boolean;
    dictSize             : UInt32  := LZMA_DIC_MIN;
    lc                   : LC_range:= 3; -- the number of "literal context" bits
    lp                   : LP_range:= 0; -- the number of "literal pos" bits
    pb                   : PB_range:= 2; -- the number of "pos" bits
  end record;

  procedure Encode_LZMA_stream(
    lzma_params: LZMA_Params_Info;
    s_in, s_out: Stream_Access
  )
  is
    -- LZ77 params
    Look_Ahead         : constant Integer:= 258;
    String_buffer_size : constant := 2**15; -- 2**n optimizes "mod" to "and"
    Threshold          : constant := 3;

    EOS: Boolean:= False;

    function Read_byte return Byte is
      b: Byte;
    begin
      Byte'Read(s_in, b); -- !! non-buffered
      return b;
    exception
      when Ada.Streams.Stream_IO.End_Error =>
        EOS:= True;
        return Character'Pos('X'); -- cf Zip.Compress.Deflate pour éviter ce pb...
    end Read_byte;

    function More_bytes return Boolean is
    begin
      return not EOS;
    end More_bytes;

    procedure Put_byte(b : Byte) is
    begin
      Byte'Write(s_out, b);  -- !! non-buffered
    end Put_byte;

    --
    -- Finite State Machine to shortcut the LZ stream
    --
    Num_States: constant := 12;
    subtype State_range is Unsigned range 0 .. Num_States - 1;
    --  States 0 to 6 are for literals
    --  States 7 to 11 are for distance-length matches

    state : State_range := 0;

    type Transition is array(State_range) of State_range;

    Update_State_Literal  : constant Transition:= (0, 0, 0, 0, 1, 2, 3,  4,  5,  6,  4,  5);

    procedure Write_literal_byte( b: Byte ) is
    begin
      null; -- !!
      state := Update_State_Literal(state);
    end Write_literal_byte;

    procedure Write_DL_code( distance, length: Integer ) is
    begin
      null; -- !!
    end Write_DL_code;

    procedure My_LZ77 is
      new LZ77.Encode(
        String_buffer_size, Look_Ahead, Threshold,
        LZ77.IZ_9,
        Read_byte, More_bytes,
        Write_literal_byte, Write_DL_code
      );

    procedure Write_LZMA_header is
      dw: UInt32:= lzma_params.dictSize;
      uw: Data_Bytes_Count:= lzma_params.unpackSize;
    begin
      Put_byte(Byte(lzma_params.lc + 9 * lzma_params.lp + 9 * 5 * lzma_params.pb));
      for i in 0..3 loop
        Put_byte(Byte(dw mod 256));
        dw:= dw / 256;
      end loop;
      if lzma_params.unpackSizeDefined then
        for i in 0..7 loop
          Put_byte(Byte(uw mod 256));
          uw:= uw / 256;
        end loop;
      else
        for i in 0..7 loop
          Put_byte(16#FF#);
        end loop;
      end if;
    end Write_LZMA_header;

  begin
    Write_LZMA_header;
    -- Whole processing here:
    My_LZ77;
  end Encode_LZMA_stream;

  lzma_params: LZMA_Params_Info;
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
  lzma_params.header_with_size  := True;
  lzma_params.unpackSize        := Size(f_in);
  lzma_params.unpackSizeDefined := True;
  Create(f_out,Out_File, Argument(2));
  Put_Line(
    "lc="   & Natural'Image(lzma_params.lc) &
    ", lp=" & Natural'Image(lzma_params.lp) &
    ", pb=" & Natural'Image(lzma_params.pb)
  );
  Put_Line("Dictionary size =" & Unsigned_32'Image(lzma_params.dictSize));
  New_Line;
  Encode_LZMA_stream(lzma_params, Stream(f_in), Stream(f_out));
  if lzma_params.unpackSizeDefined then
    Print_Data_Bytes_Count("Uncompressed size", lzma_params.unpackSize);
  else
    Put_Line("Uncompressed size not defined, end marker has been emitted.");
  end if;
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
