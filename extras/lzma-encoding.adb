--  DRAFT - WORK IN PROGRESS - DRAFT - WORK IN PROGRESS - DRAFT - WORK IN PROGRESS

with LZ77;

with Ada.Streams.Stream_IO;             use Ada.Streams.Stream_IO;
with Interfaces;                        use Interfaces;

package body LZMA.Encoding is

  procedure Encode is

    procedure LZ77_emits_literal_byte (b: Byte) is
    begin
      null; -- !!
    end LZ77_emits_literal_byte;

    procedure LZ77_emits_DL_code (distance, length: Integer) is
    begin
      null; -- !!
    end LZ77_emits_DL_code;

    ------------------------
    --  LZ77 compression  --
    ------------------------

    LZ77_choice: constant array(LZMA_Level) of LZ77.Method_Type:=
      (Level_1   => LZ77.IZ_6,
       Level_2   => LZ77.IZ_10);

    Look_Ahead         : constant Integer:= 258;
    String_buffer_size : constant := 2**15;  --  2**15 = size for Deflate

    procedure My_LZ77 is
      new LZ77.Encode
        ( String_buffer_size => String_buffer_size,
          Look_Ahead         => Look_Ahead,
          Threshold          => 2,  --  From a string match length > 2, a DL code is sent
          Method             => LZ77_choice(level),
          Read_byte          => Read_byte,
          More_bytes         => More_bytes,
          Write_byte         => LZ77_emits_literal_byte,
          Write_code         => LZ77_emits_DL_code
        );

    subtype Data_Bytes_Count is Ada.Streams.Stream_IO.Count;

    LZMA_DIC_MIN : constant := 2 ** 12; -- 4096

    type LZMA_Params_Info is record
      unpackSize           : Data_Bytes_Count:= 0;
      unpackSizeDefined    : Boolean := False;
      markerIsMandatory    : Boolean := True;
      dictSize             : UInt32  := LZMA_DIC_MIN;
      lc                   : LC_range:= 3; -- the number of "literal context" bits
      lp                   : LP_range:= 0; -- the number of "literal pos" bits
      pb                   : PB_range:= 2; -- the number of "pos" bits
    end record;

    lzma_params: LZMA_Params_Info;

    procedure Write_LZMA_header is
      dw: UInt32:= lzma_params.dictSize;
      uw: Data_Bytes_Count:= lzma_params.unpackSize;
    begin
      Write_byte(Byte(lzma_params.lc + 9 * lzma_params.lp + 9 * 5 * lzma_params.pb));
      for i in 0..3 loop
        Write_byte(Byte(dw mod 256));
        dw:= dw / 256;
      end loop;
      if lzma_params.unpackSizeDefined then
        for i in 0..7 loop
          Write_byte(Byte(uw mod 256));
          uw:= uw / 256;
        end loop;
      else
        for i in 0..7 loop
          Write_byte(16#FF#);
        end loop;
      end if;
    end Write_LZMA_header;
  begin
    Write_LZMA_header;
    My_LZ77;
  end Encode;

end LZMA.Encoding;
