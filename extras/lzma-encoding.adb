--  DRAFT - WORK IN PROGRESS - DRAFT - WORK IN PROGRESS - DRAFT - WORK IN PROGRESS

--  Parts from original LzmaEnc.c by Igor Pavlov and
--  the LZMAEncoder.java translation by Lasse Collin.
--  Other parts from LZMA.Decoding when symmetric. E.g.
--      Bit_Tree_Decode(probs_len.low_coder(pos_state), Len_low_bits, len);
--  becomes
--      Bit_Tree_Encode(probs_len.low_coder(pos_state), Len_low_bits, len);

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

    procedure Flush_range_encoder is
    begin
      for i in 1..4 loop
        Shift_low;
      end loop;
    end Flush_range_encoder;

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

    subtype Data_Bytes_Count is Ada.Streams.Stream_IO.Count;
    String_buffer_size : constant := 2**15;  --  2**15 = size for Deflate

    type LZMA_Params_Info is record
      unpackSize           : Data_Bytes_Count:= 0;
      unpackSizeDefined    : Boolean := False;
      has_end_mark         : Boolean := True;
      dictSize             : UInt32  := String_buffer_size;
      lc                   : Literal_context_bits_range:= 3;   --  number of "literal context" bits
      lp                   : Literal_position_bits_range:= 0;  --  number of "literal pos" bits
      pb                   : Position_bits_range:= 2;          --  number of "pos" bits
    end record;

    lzma_params: LZMA_Params_Info;

    state : State_range := 0;
    rep0, rep1, rep2, rep3 : UInt32 := 0;  --  Recent distances used for LZ
    total_pos : Unsigned := 0;
    pos_state: Pos_state_range := 0;
    probs: All_probabilities(
      last_lit_prob_index => 16#300# * 2 ** (lzma_params.lc + lzma_params.lp) - 1
    );
    pos_bits_mask : constant UInt32 := 2 ** lzma_params.pb - 1;

    procedure Update_pos_state is
    begin
      pos_state := Pos_state_range(UInt32(total_pos) and pos_bits_mask);
    end Update_pos_state;

    procedure LZ77_emits_literal_byte (b: Byte) is
    begin
      Encode_Bit(probs.switch.match(state, pos_state), literal_choice);
      null; -- !!!
      total_pos:= total_pos + 1;
      Update_pos_state;
    end LZ77_emits_literal_byte;

    procedure Bit_Tree_Encode(
      prob     : in out CProb_array;
      num_bits :        Positive;
      symbol   :        Unsigned)
    is
      bit, m: Unsigned;
    begin
      m:= 1;
      for i in reverse 1..num_bits loop
        bit:= Unsigned(Shift_Right(UInt32(symbol), i)) and 1;
        Encode_Bit(prob(m + prob'First), bit);
        m:= m + m + bit;
      end loop;
    end Bit_Tree_Encode;

    procedure Encode_Length(probs_len: in out Probs_for_LZ_Lengths; length: Unsigned) is
      len: Unsigned:= length - Min_match_length;
    begin
      if len < Len_low_symbols then
        Encode_Bit(probs_len.choice_1, 0);
        Bit_Tree_Encode(probs_len.low_coder(pos_state), Len_low_bits, len);
      else
        Encode_Bit(probs_len.choice_1, 1);
        if len < Len_mid_symbols then
          Encode_Bit(probs_len.choice_2, 0);
          len:= len - Len_low_symbols;
          Bit_Tree_Encode(probs_len.mid_coder(pos_state), Len_mid_bits, len);
        else
          Encode_Bit(probs_len.choice_2, 1);
          len:= len - Len_low_symbols - Len_mid_symbols;
          Bit_Tree_Encode(probs_len.high_coder, Len_high_bits, len);
        end if;
      end if;
    end Encode_Length;

    --  Gets an integer [0, 63] matching the highest two bits of an integer.
    --  It is a log2 with one "decimal".

    function Get_dist_slot(dist: UInt32) return Unsigned is
      n: UInt32;
      i: Natural;
    begin
      if dist <= Start_dist_model_index then
        return Unsigned(dist);
      end if;
      n := dist;
      i := 31;
      if (n and 16#FFFF_0000#) = 0 then
        n := Shift_Left(n, 16);
        i := 15;
      end if;
      if (n and 16#FF00_0000#) = 0 then
        n := Shift_Left(n, 8);
        i := i - 8;
      end if;
      if (n and 16#F000_0000#) = 0 then
        n := Shift_Left(n, 4);
        i := i - 4;
      end if;
      if (n and 16#C000_0000#) = 0 then
        n := Shift_Left(n, 2);
        i := i - 2;
      end if;
      if (n and 16#8000_0000#) = 0 then
        i := i - 1;
      end if;
      return Unsigned(i * 2) + Unsigned(Shift_Right(dist, i - 1) and 1);
    end Get_dist_slot;

    procedure Write_Simple_Match(distance: UInt32; length: Unsigned) is
      procedure Encode_Distance is
        len_state : constant Unsigned := Unsigned'Min(length, Len_to_pos_states - 1);
        dist_slot : constant Unsigned := Get_dist_slot(distance);
      begin
        Bit_Tree_Encode(probs.dist.slot_coder(len_state), Dist_slot_bits, dist_slot);
        -- !!! Dist. See encodeMatch line 289 (Java), line 1898 (C)
      end Encode_Distance;
    begin
      Encode_Bit(probs.switch.rep(state), Simple_match_choice);
      state := Update_State_Match(state);
      Encode_Length(probs.len, length);
      Encode_Distance;
      rep3 := rep2;
      rep2 := rep1;
      rep1 := rep0;
      rep0 := distance;
    end Write_Simple_Match;

    procedure LZ77_emits_DL_code (distance, length: Integer) is
    begin
      if length not in Min_match_length .. Max_match_length then
        raise Program_Error;  --  !! should not happen
      end if;
      Encode_Bit(probs.switch.match(state, pos_state), DL_code_choice);
      --  !!  Later we will choose betweeen different tactics with memorized distances
      Write_Simple_Match(UInt32(distance), Unsigned(length));
      total_pos:= total_pos + Unsigned(length);
      Update_pos_state;
    end LZ77_emits_DL_code;

    ------------------------
    --  LZ77 compression  --
    ------------------------

    LZ77_choice: constant array(LZMA_Level) of LZ77.Method_Type:=
      (Level_1   => LZ77.IZ_6,
       Level_2   => LZ77.IZ_10);

    Look_Ahead : constant Integer:= 258;

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

    --  The end-of-stream marker is a fake "Simple Match" with
    --  a special length value: 16#FFFF_FFFF#.
    --
    procedure Write_end_marker is
    begin
      Encode_Bit(probs.switch.match(state, pos_state), DL_code_choice);
      Write_Simple_Match(distance => 16#FFFF_FFFF#, length => Min_match_length);
      --
      --  LzmaEnc.c
      --
      --    static void WriteEndMarker(CLzmaEnc *p, UInt32 posState)
      --  {
      --    UInt32 len;
      --    RangeEnc_EncodeBit(&p->rc, &p->isMatch[p->state][posState], 1);
      --    RangeEnc_EncodeBit(&p->rc, &p->isRep[p->state], 0);
      --    p->state = kMatchNextStates[p->state];
      --    len = LZMA_MATCH_LEN_MIN;
      --    LenEnc_Encode2(&p->lenEnc, &p->rc, len - LZMA_MATCH_LEN_MIN, posState, !p->fastMode, p->ProbPrices);
      --    RcTree_Encode(&p->rc, p->posSlotEncoder[GetLenToPosState(len)], kNumPosSlotBits, (1 << kNumPosSlotBits) - 1);
      --    RangeEnc_EncodeDirectBits(&p->rc, (((UInt32)1 << 30) - 1) >> kNumAlignBits, 30 - kNumAlignBits);
      --    RcTree_ReverseEncode(&p->rc, p->posAlignEncoder, kNumAlignBits, kAlignMask);
      --  }
    end Write_end_marker;

  begin
    Write_LZMA_header;
    My_LZ77;
    if lzma_params.has_end_mark then
      Write_end_marker;
    end if;
    Flush_range_encoder;
  end Encode;

end LZMA.Encoding;
