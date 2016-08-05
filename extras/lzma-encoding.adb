--  DRAFT - WORK IN PROGRESS - DRAFT - WORK IN PROGRESS - DRAFT - WORK IN PROGRESS

with LZ77;

with Ada.Streams.Stream_IO;             use Ada.Streams.Stream_IO;
with Interfaces;                        use Interfaces;

package body LZMA.Encoding is

  subtype UInt64 is Unsigned_64;

  type Range_Encoder is record
    width     : UInt32  := 16#FFFF_FFFF#;  --  (*)
    low       : UInt64  := 0;  --  The current range is [low, low+width[
    cache     : Byte    := 0;
    cache_size: UInt64  := 1;
  end record;
  --  (*) called "range" in LZMA spec and "remaining width" in G.N.N. Martin's
  --      article about range encoding.

  procedure Encode is

    --  Range encoding of single bits - see equivalent LZMA.Decoding
    --  parts for comments with some explanations.

    range_enc: Range_Encoder;

    --  This part corresponds to G.N.N. Martin's revised algorithm's adding
    --  of trailing digits, zeroes. The leftmost digits of the range don't
    --  change anymore and can be output.

    procedure Shift_low is
      --  Top 32 bits of the lower range bound.
      low_top32    : constant UInt64:= Shift_Right(range_enc.low, 32);
      low_bottom32 : constant UInt64:= range_enc.low and 16#FFFF_FFFF#;
      temp: Byte;
    begin
      if low_bottom32 < 16#FF00_0000# or else low_top32 /= 0 then
        --  Flush range_enc.cache_size bytes, based on only
        --  2 byte values: range_enc.cache and (low_top32 and 16#FF#).
        --  The mechanism is a bit obscure...
        temp:= range_enc.cache;
        loop
          Write_byte(temp + Byte(low_top32 and 16#FF#));
          temp:= 16#FF#;
          range_enc.cache_size:= range_enc.cache_size - 1;
          exit when range_enc.cache_size = 0;
        end loop;
        range_enc.cache:= Byte(Shift_Right(range_enc.low, 24) and 16#FF#);
      end if;
      range_enc.cache_size:= range_enc.cache_size + 1;
      range_enc.low:= Shift_Left(low_bottom32, 8);  --  Here are the trailing zeroes added.
    end Shift_low;

    procedure Normalize is
    pragma Inline(Normalize);
    begin
      if range_enc.width < width_threshold then
        range_enc.width := Shift_Left(range_enc.width, 8);
        Shift_low;
      end if;
    end Normalize;

    procedure Encode_Bit(prob_io: in out CProb; symbol: in Unsigned) is
    pragma Inline(Encode_Bit);
      prob: constant CProb:= prob_io;  --  Local copy
      bound: constant UInt32:= Shift_Right(range_enc.width, Probability_model_bits) * prob;
    begin
      if symbol = 0 then
        prob_io:= prob + Shift_Right(Probability_model_count - prob, Probability_change_bits);
        range_enc.width := bound;
        Normalize;
      else
        prob_io:= prob - Shift_Right(prob, Probability_change_bits);
        range_enc.low := range_enc.low + UInt64(bound);
        range_enc.width := range_enc.width - bound;
        Normalize;
      end if;
    end Encode_Bit;

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

    type LZMA_Params_Info is record
      unpackSize           : Data_Bytes_Count:= 0;
      unpackSizeDefined    : Boolean := False;
      markerIsMandatory    : Boolean := True;
      dictSize             : UInt32  := String_buffer_size;
      lc                   : Literal_context_bits_range:= 3;   --  number of "literal context" bits
      lp                   : Literal_position_bits_range:= 0;  --  number of "literal pos" bits
      pb                   : Position_bits_range:= 2;          --  number of "pos" bits
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
