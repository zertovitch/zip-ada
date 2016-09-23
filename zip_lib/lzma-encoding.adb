--  LZMA.Encoding - a standalone, generic LZMA encoder.
--  Author: G. de Montmollin (except parts mentioned below).
--
--  This encoder was built mostly by mirroring from LZMA.Decoding upon
--  the format's symmetries between encoding and decoding.
--    For instance,
--      Bit_Tree_Decode(probs_len.low_coder(pos_state), Len_low_bits, len);
--    becomes:
--      Bit_Tree_Encode(probs_len.low_coder(pos_state), Len_low_bits, len);
--
--  The base mechanism (the range encoding, encoding of literals and DL codes)
--  is from the original LzmaEnc.c by Igor Pavlov.
--  The Get_dist_slot function is from the LZMAEncoder.java by Lasse Collin.
--
--  Change log:
--------------
--
--  18-Aug-2016: Fully functional.
--  28-Jul-2016: Created.

with LZ77;

with Ada.Streams.Stream_IO;             use Ada.Streams.Stream_IO;
with Ada.Unchecked_Deallocation;
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

  procedure Encode(
    level                  : Compression_level           := Level_1;
    literal_context_bits   : Literal_context_bits_range  := 3;   --  Bits of last byte are used.
    literal_position_bits  : Literal_position_bits_range := 0;   --  Position mod 2**bits is used.
    position_bits          : Position_bits_range         := 2;   --  Position mod 2**bits is used.
    end_marker             : Boolean := True;   --  Produce an End-Of-Stream marker ?
    uncompressed_size_info : Boolean := False;  --  Optional extra header needed for .lzma files.
    dictionary_size        : Natural := Default_dictionary_size  --  Not used by Level_1, Level_2.
  )
  is

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
        range_enc.width := Shift_Left(range_enc.width, 8);  --  Trailing zeroes are added to width.
        Shift_low;
      end if;
    end Normalize;

    procedure Encode_Bit(prob: in out CProb; symbol: in Unsigned) is
    pragma Inline(Encode_Bit);
      cur_prob: constant CProb:= prob;  --  Local copy
      --  The current interval is [low, high=low+width[ .
      --  The bound is between 0 and width, closer to 0 if prob
      --  is small, closer to width if prob is large.
      bound: constant UInt32:= Shift_Right(range_enc.width, Probability_model_bits) * UInt32(cur_prob);
    begin
      if symbol = 0 then
        --  Left sub-interval, for symbol 0: [low, low+bound[ .
        --  Set new range. low is unchanged, high is new.
        range_enc.width := bound;
        Normalize;
        --  Increase probability.
        --  The truncation ensures that prob <= Probability_model_count - (2**m - 1). See note (*).
        prob:= cur_prob + Shift_Right(Probability_model_count - cur_prob, Probability_change_bits);
      else
        --  Right sub-interval, for symbol 1: [low+bound, high=low+width[ .
        --  Set new range. low is new, high is unchanged.
        range_enc.low := range_enc.low + UInt64(bound);
        range_enc.width := range_enc.width - bound;
        Normalize;
        --  Decrease probability: prob:= prob - {prob / 2**m}, approx. equal to prob * (1 - 2**m).
        --  The truncation represented by {} ensures that prob >= 2**m - 1. See note (*).
        prob:= cur_prob - Shift_Right(cur_prob, Probability_change_bits);
      end if;
      --  (*) It can be exhaustively checked that it is always he case.
      --      A too low prob could cause the width to be too small or even zero.
      --      Same for "too high". See LZMA sheet in za_work.xls.
    end Encode_Bit;

    -----------------------------------
    --  LZ77 compression parameters  --
    -----------------------------------

    LZ77_choice: constant array(Compression_level) of LZ77.Method_Type:=
      (Level_0   => LZ77.IZ_4,  --  Fake: actually we don't do any LZ77 for level 0
       Level_1   => LZ77.IZ_6,
       Level_2   => LZ77.IZ_10,
       Level_3   => LZ77.BT4);

    Min_length : constant array(Compression_level) of Positive:=
      (Level_1 | Level_2  => 3,    --  Deflate's Value
       others             => 2);

    Max_length : constant array(Compression_level) of Positive:=
      (Level_1 | Level_2  => 258,  --  Deflate's Value
       others             => 273);

    --  Round to the next power of two. BT4 borks without this for the window size.
    function Ceiling_power_of_2(x: Natural) return Positive is
      p: Positive:= 1;
    begin
      while p < Integer'Last / 2 and p < x loop
        p:= p * 2;
      end loop;
      return Integer'Max(p, x);
    end Ceiling_power_of_2;

    --  String_buffer_size: the actual dictionary size used.
    String_buffer_size: constant array(Compression_level) of Positive:=
      (Level_0            => 16,       --  Fake: actually we don't use any LZ77 for level 0
       Level_1 | Level_2  => 2 ** 15,  --  Deflate's Value: 32 KB
       Level_3            =>
         Integer'Max(
           Min_dictionary_size,                --  minimum:  4 KB
           Integer'Min(
             --    dictionary_size is specified; default is 32 KB
             Ceiling_power_of_2(dictionary_size),
             2 ** 25                           --  maximum: 32 MB
           )
         )
      );

    -----------------------------------------------------------
    --  The LZMA "machine": here the LZ codes are processed  --
    --  and sent to the above bit encoder in a smart way.    --
    -----------------------------------------------------------

    subtype Data_Bytes_Count is Ada.Streams.Stream_IO.Count;

    type LZMA_Params_Info is record
      unpack_size          : Data_Bytes_Count:= 0;
      unpack_size_defined  : Boolean := False;
      header_has_size      : Boolean := uncompressed_size_info;
      has_end_mark         : Boolean := end_marker;
      dict_size            : UInt32  := UInt32(String_buffer_size(level));
      lc                   : Literal_context_bits_range  := literal_context_bits;
      lp                   : Literal_position_bits_range := literal_position_bits;
      pb                   : Position_bits_range         := position_bits;
    end record;

    params: LZMA_Params_Info;

    --  Finite state machine.
    state : State_range := 0;
    --  Small stack of recent distances used for LZ.
    subtype Repeat_stack_range is Integer range 0..3;
    rep_dist: array(Repeat_stack_range) of UInt32 := (others => 0);
    --
    total_pos : Data_Bytes_Count := 0;
    pos_state : Pos_state_range  := 0;
    probs: All_probabilities(last_lit_prob_index => 16#300# * 2 ** (params.lc + params.lp) - 1);
    pos_bits_mask    : constant UInt32 := 2 ** params.pb - 1;
    literal_pos_mask : constant UInt32 := 2 ** params.lp - 1;

    procedure Update_pos_state is
    begin
      pos_state := Pos_state_range(UInt32(total_pos) and pos_bits_mask);
    end Update_pos_state;

    -----------------------------------------------------------------------------------
    --  This part processes the case where LZ77 sends a literal (a plain text byte)  --
    -----------------------------------------------------------------------------------

    procedure Write_Literal (prob: in out CProb_array; symbol: in UInt32) is
      symb: UInt32:= symbol or 16#100#;
    begin
      loop
        Encode_Bit(
          prob   => prob(Integer(Shift_Right(symb, 8)) + prob'First),
          symbol => Unsigned(Shift_Right(symb, 7)) and 1
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
          prob   => prob(Integer(offs + (match and offs) + Shift_Right(symb, 8)) + prob'First),
          symbol => Unsigned(Shift_Right(symb, 7)) and 1
        );
        symb:= Shift_Left(symb, 1);
        offs:= offs and not (match xor symb);
        exit when symb >= 16#10000#;
      end loop;
    end Write_Literal_Matched;

    prev_byte: Byte:= 0;
    --  We expand the DL codes in order to have some past data.
    subtype Text_Buffer_Index is UInt32 range 0 .. UInt32(String_buffer_size(level) - 1);
    type Text_Buffer is array (Text_Buffer_Index) of Byte;
    Text_Buf_Mask: constant UInt32:= UInt32(String_buffer_size(level) - 1);
    --  NB: heap allocation used only for convenience because of
    --      small default stack sizes on some compilers.
    type p_Text_Buffer is access Text_Buffer;
    procedure Dispose is new Ada.Unchecked_Deallocation(Text_Buffer, p_Text_Buffer);
    Text_Buf: p_Text_Buffer:= new Text_Buffer;
    R: UInt32:= 0;

    type MProb is new Long_Float range 0.0 .. 1.0;

    function To_Math(cp: CProb) return MProb is
    pragma Inline(To_Math);
    begin
      return MProb'Base(cp) / MProb'Base(Probability_model_count);
    end To_Math;

    function Short_Rep_Match_better_than_Literal return Boolean is
      --  Probability threshold for the "Short Rep Match" case, b = b_match.
      --  Some values:
      --    0.0                 : always do (when possible of course...)
      --    1.0 / 16.0 = 0.0625 : minimum is the equiprobable case
      --    1.0                 : never do
      threshold_short_rep_match: constant:= 0.01;  --  Empirical optimum
      --  Currently modeled probability for the "Short Rep Match" comination in the decision tree.
      --  We assume independent (multiplicative) probabilities, just like the range encoder does
      --  when adapting the range width. With high probabilities, the width will decrease less
      --  and the compression will be better. When choice bit is 1 the case's probability is 1-p.
      prob_literal_choice: constant MProb:=
        To_Math(probs.switch.match(state, pos_state));
      prob_short_rep_match: constant MProb:=
        (1.0 - prob_literal_choice) *                       --  1
        (1.0 - To_Math(probs.switch.rep(state))) *          --  1
        To_Math(probs.switch.rep_g0(state)) *               --  0
        To_Math(probs.switch.rep0_long(state, pos_state));  --  0
    begin
      return prob_short_rep_match >= threshold_short_rep_match;
    end Short_Rep_Match_better_than_Literal;

    procedure LZ77_emits_literal_byte (b: Byte) is
      lit_state : Integer;
      probs_idx : Integer;
      b_match: constant Byte:= Text_Buf((R - rep_dist(0) - 1) and Text_Buf_Mask);
    begin
      if b = b_match and then total_pos > Data_Bytes_Count(rep_dist(0) + 1)
        and then Short_Rep_Match_better_than_Literal
      then
        --  We are lucky: both bytes are the same. No literal to encode, "Short Rep Match" case.
        --  The cost (4 bits) is affordable since the combined probabilities are high enough.
        Encode_Bit(probs.switch.match(state, pos_state), DL_code_choice);              --  1
        Encode_Bit(probs.switch.rep(state), Rep_match_choice);                         --  1
        Encode_Bit(probs.switch.rep_g0(state), The_distance_is_rep0_choice);           --  0
        Encode_Bit(probs.switch.rep0_long(state, pos_state), The_length_is_1_choice);  --  0
        state := Update_State_ShortRep(state);
      else
        Encode_Bit(probs.switch.match(state, pos_state), Literal_choice);
        lit_state :=
          Integer(
            Shift_Left(UInt32(total_pos) and literal_pos_mask, params.lc) +
            Shift_Right(UInt32(prev_byte), 8 - params.lc)
          );
        probs_idx:= 16#300# * lit_state;
        if state < 7 then
          Write_Literal(probs.lit(probs_idx..probs.lit'Last), UInt32(b));
        else
          Write_Literal_Matched(probs.lit(probs_idx..probs.lit'Last), UInt32(b), UInt32(b_match));
        end if;
        state := Update_State_Literal(state);
      end if;
      total_pos:= total_pos + 1;
      Update_pos_state;
      prev_byte:= b;
      Text_Buf(R):= b;
      R:= (R + 1) and Text_Buf_Mask;  --  This is mod String_buffer_size
    end LZ77_emits_literal_byte;

    ---------------------------------------------------------------------------------
    --  This part processes the case where LZ77 sends a Distance-Length (DL) code  --
    ---------------------------------------------------------------------------------

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
        Encode_Bit(prob(Integer(m) + prob'First), bit);
        m:= m + m + bit;
      end loop;
    end Bit_Tree_Encode;

    procedure Encode_Length(probs_len: in out Probs_for_LZ_Lengths; length: Unsigned) is
      len: Unsigned:= length - Min_match_length;
    begin
      if len < Len_low_symbols then
        Encode_Bit(probs_len.choice_1, 0);
        --  LZ length in [2..9], i.e. len in [0..7]
        Bit_Tree_Encode(probs_len.low_coder(pos_state), Len_low_bits, len);
      else
        Encode_Bit(probs_len.choice_1, 1);
        len:= len - Len_low_symbols;
        if len < Len_mid_symbols then
          Encode_Bit(probs_len.choice_2, 0);
          --  LZ length in [10..17], i.e. len in [0..7]
          Bit_Tree_Encode(probs_len.mid_coder(pos_state), Len_mid_bits, len);
        else
          Encode_Bit(probs_len.choice_2, 1);
          len:= len - Len_mid_symbols;
          --  LZ length in [18..273], i.e. len in [0..255]
          Bit_Tree_Encode(probs_len.high_coder, Len_high_bits, len);
        end if;
      end if;
    end Encode_Length;

    --  Gets an integer [0, 63] matching the highest two bits of an integer.
    --  It is a log2 function with one "decimal".
    --
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
          Encode_Bit(prob(Integer(m) + prob'First), bit);
          m := m + m + bit;
          symb:= Shift_Right(symb, 1);
        end loop;
      end Bit_Tree_Reverse_Encode;

      --  Range encoding of num_bits with equiprobability.
      --
      procedure Encode_Direct_Bits(value: UInt32; num_bits: Natural) is
      begin
        for i in reverse 0 .. num_bits - 1 loop
          --  Bound is the half width. New width is halved anyway.
          range_enc.width:= Shift_Right(range_enc.width, 1);
          --  Either low is unchanged (bit=0), or new low := old low + bound (bit=1).
          range_enc.low := range_enc.low +
            (UInt64(range_enc.width) and (0 - UInt64(Shift_Right(value, i) and 1)));
          Normalize;
        end loop;
      end Encode_Direct_Bits;
      --
      procedure Encode_Distance is
        len_state : constant Unsigned := Unsigned'Min(length - 2, Len_to_pos_states - 1);
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
              probs.dist.pos_coder(Integer(base) - Integer(dist_slot) - 1 .. Pos_coder_range'Last),
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
      --
    begin
      Encode_Bit(probs.switch.rep(state), Simple_match_choice);
      state := Update_State_Match(state);
      Encode_Length(probs.len, length);
      Encode_Distance;
      --  Shift the stack of recent distances; the new distance becomes the first item.
      for i in reverse 1 .. Repeat_stack_range'Last loop
        rep_dist(i) := rep_dist(i-1);
      end loop;
      rep_dist(0) := distance;
    end Write_Simple_Match;

    procedure Write_Repeat_Match(index: Repeat_stack_range; length: Unsigned) is
      aux: UInt32;
    begin
      Encode_Bit(probs.switch.rep(state), Rep_match_choice);
      case index is
        when 0 =>
          Encode_Bit(probs.switch.rep_g0(state), The_distance_is_rep0_choice);
          Encode_Bit(probs.switch.rep0_long(state, pos_state), The_length_is_not_1_choice);
        when 1 =>
          Encode_Bit(probs.switch.rep_g0(state), The_distance_is_not_rep0_choice);
          Encode_Bit(probs.switch.rep_g1(state), The_distance_is_rep1_choice);
        when 2 =>
          Encode_Bit(probs.switch.rep_g0(state), The_distance_is_not_rep0_choice);
          Encode_Bit(probs.switch.rep_g1(state), The_distance_is_not_rep1_choice);
          Encode_Bit(probs.switch.rep_g2(state), The_distance_is_rep2_choice);
        when 3 =>
          Encode_Bit(probs.switch.rep_g0(state), The_distance_is_not_rep0_choice);
          Encode_Bit(probs.switch.rep_g1(state), The_distance_is_not_rep1_choice);
          Encode_Bit(probs.switch.rep_g2(state), The_distance_is_not_rep2_choice);
      end case;
      --  Roll the stack of recent distances up to the found item, which becomes first.
      aux:= rep_dist(index);
      for i in reverse 1..index loop
        rep_dist(i) := rep_dist(i-1);
      end loop;
      rep_dist(0):= aux;
      --
      Encode_Length(probs.rep_len, length);
      state := Update_State_Rep(state);
    end Write_Repeat_Match;

    procedure LZ77_emits_DL_code (distance: Integer; length: Match_length_range) is
      Copy_start: constant UInt32:= (R - UInt32(distance)) and Text_Buf_Mask;
      dist_ip: constant UInt32:= UInt32(distance - 1);
      found_repeat: Integer:= rep_dist'First - 1;
    begin
      Encode_Bit(probs.switch.match(state, pos_state), DL_code_choice);
      for i in rep_dist'Range loop
        if dist_ip = rep_dist(i) then
          found_repeat:= i;
          exit;
        end if;
      end loop;
      if found_repeat >= rep_dist'First then
        Write_Repeat_Match(found_repeat, Unsigned(length));
      else
        Write_Simple_Match(dist_ip, Unsigned(length));
      end if;
      total_pos:= total_pos + Data_Bytes_Count(length);
      Update_pos_state;
      --  Expand in the circular text buffer to have it up to date
      for K in 0 .. UInt32(length-1) loop
        Text_Buf(R):= Text_Buf((Copy_start + K) and Text_Buf_Mask);
        R:= (R + 1) and Text_Buf_Mask;  --  This is mod String_buffer_size
      end loop;
      prev_byte:= Text_Buf((R - 1) and Text_Buf_Mask);
    end LZ77_emits_DL_code;

    procedure My_LZ77 is
      new LZ77.Encode
        ( String_buffer_size => String_buffer_size(level),
          Look_Ahead         => Max_length(level),
          Threshold          => Min_length(level) - 1,
          Method             => LZ77_choice(level),
          Read_byte          => Read_byte,
          More_bytes         => More_bytes,
          Write_literal      => LZ77_emits_literal_byte,
          Write_DL_code      => LZ77_emits_DL_code
        );

    procedure Write_LZMA_header is
      dw: UInt32:= params.dict_size;
      uw: Data_Bytes_Count:= params.unpack_size;
    begin
      --  5-byte header
      Write_byte(Byte(params.lc + 9 * params.lp + 9 * 5 * params.pb));
      for i in 0 .. 3 loop
        Write_byte(Byte(dw mod 256));
        dw:= dw / 256;
      end loop;
      --  8 bytes for unpacked size; optional => you need a "pre-header" with that option :-(
      if params.header_has_size then
        for i in 0 .. 7 loop
          if params.unpack_size_defined then
            Write_byte(Byte(uw mod 256));
            uw:= uw / 256;
          else
            Write_byte(16#FF#);
          end if;
        end loop;
      end if;
    end Write_LZMA_header;

  begin
    Write_LZMA_header;
    if level = Level_0 then
      while More_bytes loop
        LZ77_emits_literal_byte(Read_byte);
      end loop;
    else
      My_LZ77;
    end if;
    if params.has_end_mark then
      --  The end-of-stream marker is a fake "Simple Match" with a special distance.
      Encode_Bit(probs.switch.match(state, pos_state), DL_code_choice);
      Write_Simple_Match(distance => 16#FFFF_FFFF#, length => Min_match_length);
    end if;
    Flush_range_encoder;
    Dispose(Text_Buf);
  end Encode;

end LZMA.Encoding;
