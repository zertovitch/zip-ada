--  DRAFT - WORK IN PROGRESS - DRAFT - WORK IN PROGRESS - DRAFT - WORK IN PROGRESS

--  Parts of the base mechanism are from the original LzmaEnc.c by Igor Pavlov or
--  from the LZMAEncoder.java translation by Lasse Collin.
--  Other parts are mirrored from LZMA.Decoding when symmetric. For instance,
--      Bit_Tree_Decode(probs_len.low_coder(pos_state), Len_low_bits, len);
--  becomes:
--      Bit_Tree_Encode(probs_len.low_coder(pos_state), Len_low_bits, len);

with LZ77;

with Ada.Streams.Stream_IO;             use Ada.Streams.Stream_IO;
with Interfaces;                        use Interfaces;
with Ada.Text_IO;

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

    -------------------------------------
    --  Range encoding of single bits. --
    -------------------------------------

    range_enc: Range_Encoder;

    procedure Shift_low is
      --  Top 32 bits of the lower range bound.
      lb_top32    : constant UInt64:= Shift_Right(range_enc.low, 32);
      --  Bottom 32 bits of the lower range bound.
      lb_bottom32 : constant UInt32:= UInt32(range_enc.low and 16#FFFF_FFFF#);
      temp, lb_bits_33_40: Byte;
    begin
      if lb_bottom32 < 16#FF00_0000# or else lb_top32 /= 0 then
        --  Flush range_enc.cache_size bytes, based on only
        --  2 byte values: range_enc.cache and lb_bits_33_40.
        --  The mechanism is a bit obscure (seems to be a carry)...
        temp:= range_enc.cache;
        lb_bits_33_40:= Byte(lb_top32 and 16#FF#);
        loop
          Write_byte(temp + lb_bits_33_40);
          temp:= 16#FF#;
          range_enc.cache_size:= range_enc.cache_size - 1;
          exit when range_enc.cache_size = 0;
        end loop;
        range_enc.cache:= Byte(Shift_Right(lb_bottom32, 24) and 16#FF#);  --  bits 25 to 32
      end if;
      range_enc.cache_size:= range_enc.cache_size + 1;
      --  Bits 25 to 32 are erased and the trailing zeroes are added.
      range_enc.low:= UInt64(Shift_Left(lb_bottom32, 8));
    end Shift_low;

    procedure Flush_range_encoder is
    begin
      for i in 1 .. 5 loop
        Shift_low;
      end loop;
    end Flush_range_encoder;

    --  Normalize corresponds to G.N.N. Martin's revised algorithm's adding
    --  of trailing digits (zeroes). The leftmost digits of the range don't
    --  change anymore and can be output.
    --
    procedure Normalize is
    pragma Inline(Normalize);
    begin
      if range_enc.width < width_threshold then
        Ada.Text_IO.Put_Line("        Normalize, low   before" & range_enc.low'img);
        Ada.Text_IO.Put_Line("        Normalize, width before" & range_enc.width'img);
        range_enc.width := Shift_Left(range_enc.width, 8);
        Shift_low;
        Ada.Text_IO.Put_Line("        Normalize, low   after " & range_enc.low'img);
        Ada.Text_IO.Put_Line("        Normalize, width after " & range_enc.width'img);
      end if;
    end Normalize;

    procedure Encode_Bit(prob_io: in out CProb; symbol: in Unsigned) is
    pragma Inline(Encode_Bit);
      prob: constant CProb:= prob_io;  --  Local copy
      --  Depending on the probability, bound is between 0 and width.
      bound: constant UInt32:= Shift_Right(range_enc.width, Probability_model_bits) * prob;
    begin
      Ada.Text_IO.Put_Line("Encode_Bit" & symbol'img);
      Ada.Text_IO.Put_Line("  Prob before" & prob_io'img);
      if symbol = 0 then
        --  Increase probability. In [0, 1] it would be: prob:= prob + (1 - prob) / 2**m
        --  The truncation ensures (proof needed) that prob <= Probability_model_count - (2**m - 1)
        prob_io:= prob + Shift_Right(Probability_model_count - prob, Probability_change_bits);
        --  Set new width. The new range is [low, low+bound[ : low is unchanged, high is new.
        range_enc.width := bound;
        Normalize;
      else
        --  Decrease probability: prob:= prob - prob / 2**m = prob:= prob * (1 - 2**m)
        --  The truncation ensures (proof needed) that prob >= 2**m - 1
        prob_io:= prob - Shift_Right(prob, Probability_change_bits);
        --  The new range is [old low + bound, old low + old width[ : high is unchanged.
        range_enc.low := range_enc.low + UInt64(bound);
        range_enc.width := range_enc.width - bound;
        Normalize;
      end if;
      Ada.Text_IO.Put_Line("  Prob after " & prob_io'img);
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
    pos_bits_mask    : constant UInt32 := 2 ** lzma_params.pb - 1;
    literal_pos_mask : constant UInt32 := 2 ** lzma_params.lp - 1;

    procedure Update_pos_state is
    begin
      pos_state := Pos_state_range(UInt32(total_pos) and pos_bits_mask);
    end Update_pos_state;

    procedure Write_Literal (prob: in out CProb_array; symbol: in UInt32) is
      symb: UInt32:= symbol or 16#100#;
    begin
      loop
        Encode_Bit(
          prob(Unsigned(Shift_Right(symb, 8)) + prob'First),
          Unsigned(Shift_Right(symb, 7)) and 1
        );
        symb:= Shift_Left(symb, 1);
        exit when symb >= 16#10000#;
      end loop;
    end Write_Literal;

    procedure Write_Literal_Matched (prob: in out CProb_array; symbol, matched: in UInt32) is
      symb: UInt32:= symbol or 16#100#;
      offs: UInt32:= 16#100#;
      match: UInt32:= matched;
    begin
      loop
        match:= Shift_Left(match, 1);
        Encode_Bit(
          prob(Unsigned(offs + (match and offs) + Shift_Right(symb, 8)) + prob'First),
          Unsigned(Shift_Right(symb, 7)) and 1
        );
        symb:= Shift_Left(symb, 1);
        offs:= offs and not (match xor symb);
        exit when symb >= 16#10000#;
      end loop;
    end Write_Literal_Matched;

    prev_byte: Byte:= 0;
    --  We expand the DL codes in order to have some past data.
    type Text_Buffer_Index is mod String_buffer_size;
    type Text_Buffer is array (Text_Buffer_Index) of Byte;
    Text_Buf: Text_Buffer;
    R: Text_Buffer_Index:= 0;

    procedure LZ77_emits_literal_byte (b: Byte) is
      lit_state : Unsigned;
      probs_idx : Unsigned;
      b_match: Byte;
    begin
      Encode_Bit(probs.switch.match(state, pos_state), Literal_choice);
      lit_state :=
        Unsigned(
          Shift_Left(UInt32(total_pos) and literal_pos_mask, lzma_params.lc) +
          Shift_Right(UInt32(prev_byte), 8 - lzma_params.lc)
        );
      probs_idx:= 16#300# * lit_state;
      if state < 7 then
        Write_Literal(probs.lit(probs_idx..probs.lit'Last), UInt32(b));
        Ada.Text_IO.Put_Line("  *** LZ77 Literal, simple (seems ok):" & Character'Val(b));
      else
        b_match:= Text_Buf(R-Text_Buffer_Index(rep0)-1);
        Write_Literal_Matched(probs.lit(probs_idx..probs.lit'Last), UInt32(b), UInt32(b_match));
        Ada.Text_IO.Put_Line("  *** LZ77 Literal with match (buggy):" & Character'Val(b));
      end if;
      total_pos:= total_pos + 1;
      Update_pos_state;
      state := Update_State_Literal(state);
      prev_byte:= b;
      Text_Buf(R):= b;
      R:= R+1;  --  This is mod String_buffer_size
    end LZ77_emits_literal_byte;

    procedure Bit_Tree_Encode(
      prob     : in out CProb_array;
      num_bits :        Positive;
      symbol   :        Unsigned)
    is
      bit, m: Unsigned;
    begin
      m:= 1;
      for i in reverse 0 .. num_bits - 1 loop
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
      --
      procedure Bit_Tree_Reverse_Encode(
        prob    : in out CProb_array;
        num_bits: in     Natural;
        symbol  : in     UInt32
      )
      is
        symb: UInt32:= symbol;
        m: Unsigned := 1;
        bit: Unsigned;
      begin
        for count in reverse 1 .. num_bits loop
          bit:= Unsigned(symb) and 1;
          Encode_Bit(prob(m + prob'First), bit);
          m := m + m + bit;
          symb:= Shift_Right(symb, 1);
        end loop;
      end Bit_Tree_Reverse_Encode;
      --  Range encoding of num_bits with equiprobability.
      procedure Encode_Direct_Bits(value: UInt32; num_bits: Natural) is
        num_bits_r: Natural:= num_bits;
      begin
        loop
          range_enc.width:= Shift_Right(range_enc.width, 1);
          num_bits_r:= num_bits_r - 1;
          range_enc.low := range_enc.low +
            (UInt64(range_enc.width) and (0 - UInt64(Shift_Right(value, num_bits_r) and 1)));
          Normalize;
          exit when num_bits_r = 0;
        end loop;
      end Encode_Direct_Bits;
      --
      procedure Encode_Distance is
        len_state : constant Unsigned := Unsigned'Min(length, Len_to_pos_states - 1);
        dist_slot : constant Unsigned := Get_dist_slot(distance);
        base, dist_reduced: UInt32;
        footerBits: Natural;
      begin
        Bit_Tree_Encode(probs.dist.slot_coder(len_state), Dist_slot_bits, dist_slot);
        if dist_slot >= Start_dist_model_index then
          footerBits := Natural(Shift_Right(UInt32(dist_slot), 1)) - 1;
          base := Shift_Left(UInt32(2 or (dist_slot and 1)), footerBits);
          dist_reduced := distance - base;
          if dist_slot < End_dist_model_index then
            Bit_Tree_Reverse_Encode(
              probs.dist.pos_coder(Unsigned(base) - dist_slot - 1 .. Pos_coder_range'Last),
              footerBits,
              dist_reduced
            );
          else
            Encode_Direct_Bits(Shift_Right(dist_reduced, Align_bits), footerBits - Align_bits);
            Bit_Tree_Reverse_Encode(
              probs.dist.align_coder,
              Align_bits,
              dist_reduced and Align_mask
            );
          end if;
        end if;
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
      Copy_start: constant Text_Buffer_Index:= R - Text_Buffer_Index(distance);
    begin
      Ada.Text_IO.Put_Line("  *** LZ77 DL code");
      if length not in Min_match_length .. Max_match_length then
        raise Program_Error;  --  !! should not happen
      end if;
      Encode_Bit(probs.switch.match(state, pos_state), DL_code_choice);
      --  !!  Later we will choose betweeen different tactics with memorized distances
      Write_Simple_Match(UInt32(distance - 1), Unsigned(length));
      --  !!  added "- 1", perhaps it's a fix at the wrong place (check different distances) ?
      total_pos:= total_pos + Unsigned(length);
      Update_pos_state;
      --  Expand in the circular text buffer to have it up to date
      for K in 0..Text_Buffer_Index(length-1) loop
        Text_Buf(R):= Text_Buf(Copy_start+K);
        R:= R+1;  --  This is mod String_buffer_size
      end loop;
      prev_byte:= Text_Buf(R-1);
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
      for i in 0 .. 3 loop
        Write_byte(Byte(dw mod 256));
        dw:= dw / 256;
      end loop;
      for i in 0 .. 7 loop
        if lzma_params.unpackSizeDefined then
          Write_byte(Byte(uw mod 256));
          uw:= uw / 256;
        else
          Write_byte(16#FF#);
        end if;
      end loop;
    end Write_LZMA_header;

    --  The end-of-stream marker is a fake "Simple Match" with
    --  a special length value: 16#FFFF_FFFF#.
    --
    procedure Write_end_marker is
    begin
      Encode_Bit(probs.switch.match(state, pos_state), DL_code_choice);
      Write_Simple_Match(distance => 16#FFFF_FFFF#, length => Min_match_length);
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
