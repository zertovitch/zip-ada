--  LZMA.Encoding - a standalone, generic LZMA encoder.
--  Author: G. de Montmollin (except parts mentioned below (*)).
--
--  This encoder was built mostly by mirroring from LZMA.Decoding upon
--  the format's symmetries between encoding and decoding. For instance,
--
--      Bit_Tree_Decode(probs_len.low_coder(pos_state), Len_low_bits, len);
--  becomes:
--      Bit_Tree_Encode(probs_len.low_coder(pos_state), Len_low_bits, len);
--
--  Furthermore, cases for which there are alternatives are decided by comparing
--  their respective probabilities (search "MProb" in the code).
--
--  (*) The base mechanism (the encoding of range, literals and DL codes)
--      is from the original LzmaEnc.c by Igor Pavlov.
--      The Get_dist_slot function is from the LZMAEncoder.java by Lasse Collin.

--  Legal licensing note:

--  Copyright (c) 2016 .. 2019 Gautier de Montmollin
--  SWITZERLAND

--  Permission is hereby granted, free of charge, to any person obtaining a copy
--  of this software and associated documentation files (the "Software"), to deal
--  in the Software without restriction, including without limitation the rights
--  to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
--  copies of the Software, and to permit persons to whom the Software is
--  furnished to do so, subject to the following conditions:

--  The above copyright notice and this permission notice shall be included in
--  all copies or substantial portions of the Software.

--  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
--  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
--  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
--  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
--  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
--  OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
--  THE SOFTWARE.

--  NB: this is the MIT License, as found on the site
--  http://www.opensource.org/licenses/mit-license.php

--
--  Change log:
--------------
--
--  18-Aug-2016: Fully functional.
--  28-Jul-2016: Created.

with LZ77;

with Ada.Unchecked_Deallocation;

package body LZMA.Encoding is

  use type Data_Bytes_Count;

  procedure Encode (
    level                  : Compression_level           := Level_1;
    literal_context_bits   : Literal_context_bits_range  := 3;   --  Bits of last byte are used.
    literal_position_bits  : Literal_position_bits_range := 0;   --  Position mod 2**bits is used.
    position_bits          : Position_bits_range         := 2;   --  Position mod 2**bits is used.
    end_marker             : Boolean := True;   --  Produce an End-Of-Stream marker ?
    uncompressed_size_info : Boolean := False;  --  Optional extra header needed for .lzma files.
                                                --  In LZMA.Decoding, type LZMA_Hints: has_size.
    dictionary_size        : Natural := Default_dictionary_size  --  Not used by Level_1, Level_2.
  )
  is

    -------------------------------------
    --  Range encoding of single bits. --
    -------------------------------------

    type Range_Encoder is record
      width      : UInt32  := 16#FFFF_FFFF#;  --  (*)
      low        : UInt64  := 0;  --  The current range is [low, low+width[
      cache      : Byte    := 0;
      cache_size : UInt64  := 1;
    end record;
    --  (*) "width" is called "range" in LZMA spec and "remaining width" in G.N.N. Martin's
    --      article about range encoding.

    range_enc : Range_Encoder;

    procedure Shift_low is
      --  Top 32 bits of the lower range bound.
      lb_top32    : constant UInt64 := Shift_Right (range_enc.low, 32);
      --  Bottom 32 bits of the lower range bound.
      lb_bottom32 : constant UInt32 := UInt32 (range_enc.low and 16#FFFF_FFFF#);
      temp, lb_bits_33_40 : Byte;
    begin
      if lb_bottom32 < 16#FF00_0000# or else lb_top32 /= 0 then
        --  Flush range_enc.cache_size bytes, based on only
        --  2 byte values: range_enc.cache and lb_bits_33_40.
        --  The mechanism is a bit obscure (seems to be a carry)...
        temp := range_enc.cache;
        lb_bits_33_40 := Byte (lb_top32 and 16#FF#);
        loop
          Write_Byte (temp + lb_bits_33_40);
          temp := 16#FF#;
          range_enc.cache_size := range_enc.cache_size - 1;
          exit when range_enc.cache_size = 0;
        end loop;
        range_enc.cache := Byte (Shift_Right (lb_bottom32, 24) and 16#FF#);  --  bits 25 to 32
      end if;
      range_enc.cache_size := range_enc.cache_size + 1;
      --  Bits 25 to 32 are erased and the trailing zeroes are added.
      range_enc.low := UInt64 (Shift_Left (lb_bottom32, 8));
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
    pragma Inline (Normalize);
    begin
      if range_enc.width < width_threshold then
        range_enc.width := Shift_Left (range_enc.width, 8);  --  Trailing zeroes are added to width.
        Shift_low;
      end if;
    end Normalize;

    procedure Encode_Bit (prob : in out CProb; symbol : in Unsigned) is
    pragma Inline (Encode_Bit);
      cur_prob : constant CProb := prob;  --  Local copy
      --  The current interval is [low, high=low+width[ .
      --  The bound is between 0 and width, closer to 0 if prob
      --  is small, closer to width if prob is large.
      bound : constant UInt32 := Shift_Right (range_enc.width, Probability_model_bits) * UInt32 (cur_prob);
    begin
      if symbol = 0 then
        --  Left sub-interval, for symbol 0: [low, low+bound[ .
        --  Set new range. low is unchanged, high is new.
        range_enc.width := bound;
        Normalize;
        --  Increase probability.
        --  The truncation ensures that prob <= Probability_model_count - (2**m - 1). See note (*).
        prob := cur_prob + Shift_Right (Probability_model_count - cur_prob, Probability_change_bits);
      else
        --  Right sub-interval, for symbol 1: [low+bound, high=low+width[ .
        --  Set new range. low is new, high is unchanged.
        range_enc.low := range_enc.low + UInt64 (bound);
        range_enc.width := range_enc.width - bound;
        Normalize;
        --  Decrease probability: prob:= prob - {prob / 2**m}, approx. equal to prob * (1 - 2**m).
        --  The truncation represented by {} ensures that prob >= 2**m - 1. See note (*).
        prob := cur_prob - Shift_Right (cur_prob, Probability_change_bits);
      end if;
      --  (*) It can be checked exhaustively that it is always the case.
      --      A too low prob could cause the width to be too small or even zero.
      --      Same for "too high". See LZMA sheet in za_work.xls.
    end Encode_Bit;

    --  Gets an integer [0, 63] matching the highest two bits of an integer.
    --  It is a log2 function with one "decimal".
    --
    function Get_dist_slot (dist : UInt32) return Unsigned is
      n : UInt32;
      i : Natural;
    begin
      if dist <= Start_dist_model_index then
        return Unsigned (dist);
      end if;
      n := dist;
      i := 31;
      if (n and 16#FFFF_0000#) = 0 then
        n := Shift_Left (n, 16);
        i := 15;
      end if;
      if (n and 16#FF00_0000#) = 0 then
        n := Shift_Left (n, 8);
        i := i - 8;
      end if;
      if (n and 16#F000_0000#) = 0 then
        n := Shift_Left (n, 4);
        i := i - 4;
      end if;
      if (n and 16#C000_0000#) = 0 then
        n := Shift_Left (n, 2);
        i := i - 2;
      end if;
      if (n and 16#8000_0000#) = 0 then
        i := i - 1;
      end if;
      return Unsigned (i * 2) + Unsigned (Shift_Right (dist, i - 1) and 1);
    end Get_dist_slot;

    --  Round to the next power of two. BT4 borks without this for the window size.
    function Ceiling_power_of_2 (x : Natural) return Positive is
      p : Positive := 1;
    begin
      while p < Integer'Last / 2 and p < x loop
        p := p * 2;
      end loop;
      return Integer'Max (p, x);
    end Ceiling_power_of_2;

    -----------------------------------
    --  LZ77 compression parameters  --
    -----------------------------------

    LZ77_choice : constant array (Compression_level) of LZ77.Method_Type :=
      (Level_0   => LZ77.No_LZ77,  --  We don't do any LZ77 for level 0
       Level_1   => LZ77.IZ_6,
       Level_2   => LZ77.IZ_10,
       Level_3   => LZ77.BT4);

    Min_length : constant array (Compression_level) of Positive :=
      (Level_1 | Level_2  => 3,    --  Deflate's Value
       others             => 2);

    Max_length : constant array (Compression_level) of Positive :=
      (Level_1 | Level_2  => 258,  --  Deflate's Value
       others             => 273);

    --  String_buffer_size: the actual dictionary size used.
    String_buffer_size : constant array (Compression_level) of Positive :=
      (Level_0            => 16,       --  Fake: actually we don't use any LZ77 for level 0
       Level_1 | Level_2  => 2 ** 15,  --  Deflate's Value: 32 KiB
       Level_3            =>
         Integer'Max (
           Min_dictionary_size,                --  minimum:  4 KiB
           Integer'Min (
             --    dictionary_size is specified; default is 32 KiB
             Ceiling_power_of_2 (dictionary_size),
             2 ** 25                           --  maximum: 32 MiB
           )
         )
      );

    -----------------------------------------------------------
    --  The LZMA "machine": here the LZ codes are processed  --
    --  and sent to the above bit encoder in a smart way.    --
    -----------------------------------------------------------

    type LZMA_Params_Info is record
      unpack_size          : Data_Bytes_Count := 0;
      --  unpack_size_defined is always False in this implementation:
      --  size is not known in advance and the header cannot be
      --  rewritten when processing is done.
      unpack_size_defined  : Boolean := False;
      header_has_size      : Boolean := uncompressed_size_info;
      has_end_mark         : Boolean := end_marker;
      dict_size            : UInt32  := UInt32 (String_buffer_size (level));
      lc                   : Literal_context_bits_range  := literal_context_bits;
      lp                   : Literal_position_bits_range := literal_position_bits;
      pb                   : Position_bits_range         := position_bits;
    end record;

    params : LZMA_Params_Info;

    --  Finite state machine.
    state : State_range := 0;
    --  Small stack of recent distances used for LZ. Required: initialized with zero values.
    subtype Repeat_stack_range is Integer range 0 .. 3;
    rep_dist : array (Repeat_stack_range) of UInt32 := (others => 0);
    --
    total_pos : Data_Bytes_Count := 0;
    pos_state : Pos_state_range  := 0;
    probs : All_probabilities (last_lit_prob_index => 16#300# * 2 ** (params.lc + params.lp) - 1);
    pos_bits_mask    : constant UInt32 := 2 ** params.pb - 1;
    literal_pos_mask : constant UInt32 := 2 ** params.lp - 1;

    procedure Update_pos_state is
    pragma Inline (Update_pos_state);
    begin
      pos_state := Pos_state_range (UInt32 (total_pos) and pos_bits_mask);
    end Update_pos_state;

    --  We expand the DL codes in order to have some past data.
    subtype Text_Buffer_Index is UInt32 range 0 .. UInt32 (String_buffer_size (level) - 1);
    type Text_Buffer is array (Text_Buffer_Index) of Byte;
    Text_Buf_Mask : constant UInt32 := UInt32 (String_buffer_size (level) - 1);
    --  NB: heap allocation used only for convenience because of
    --      small default stack sizes on some compilers.
    type p_Text_Buffer is access Text_Buffer;
    procedure Dispose is new Ada.Unchecked_Deallocation (Text_Buffer, p_Text_Buffer);
    Text_Buf : p_Text_Buffer := new Text_Buffer;
    R : UInt32 := 0;

    function Idx_for_Literal_prob (position : Data_Bytes_Count; prev_byte : Byte) return Integer is
    pragma Inline (Idx_for_Literal_prob);
    begin
      return 16#300# *
          Integer (
            Shift_Left (UInt32 (position) and literal_pos_mask, params.lc) +
            Shift_Right (UInt32 (prev_byte), 8 - params.lc)
          );
    end Idx_for_Literal_prob;

    prev_byte : Byte := 0;

    ------------------------
    --  Package Predicted --
    ------------------------
    --
    --  Purpose: compute predicted probabilities of different alternative encodings,
    --  in order to choose the most probable. Note that the LZMA encoding is already very efficient
    --  by taking the obvious choices and ignoring this package (see constant named "quick").
    --
    --  In the following probability computations, we assume independent
    --  (multiplicative) probabilities, just like the range encoder does
    --  when adapting the range width. With higher probabilities, the width
    --  will decrease less and the compression will be better.
    --  Since the probability model is constantly adapting, we have kind of self-fulfilling
    --  predictions - e.g. if a Short Rep Match is chosen against a Literal, the context
    --  probabilities of the former will be increased instead of the latter.

    package Predicted is
      type MProb is new Long_Float range 0.0 .. 1.0;
      --
      function Strict_Literal (
        b, b_match    : Byte;
        prob          : CProb_array;
        sim_state     : State_range;
        sim_pos_state : Pos_state_range
      ) return MProb;
      function Short_Rep_Match (
        sim_state     : State_range;
        sim_pos_state : Pos_state_range
      ) return MProb;
      function Any_literal (
        b, b_match, b_prev : Byte;  --  b_match is the byte at distance rep_dist(0) + 1.
        sim_state          : State_range;
        offset             : Data_Bytes_Count
      ) return MProb;
      --
      function Repeat_Match (index_rm : Repeat_stack_range; length : Unsigned) return MProb;
      function Simple_Match (distance : UInt32; length : Unsigned) return MProb;
      --  Strict_DL_code is either a Simple_Match or a Repeat_Match.
      function Strict_DL_code (
        distance      : Integer;
        length        : Match_length_range;
        sim_state     : State_range;
        sim_pos_state : Pos_state_range
      ) return MProb;
      --  Expanded_DL_code is a DL code expanded as a string of literals.
      function Expanded_DL_code (
        distance : Integer;
        length   : Match_length_range;
        give_up  : MProb)
      return MProb;
      --  End of the obvious cases. Now things get tougher...
      --
      --  Case of DL code split into a shorter DL code (strict or expanded), then a literal.
      function DL_code_then_Literal (
        distance  : Integer;
        length    : Match_length_range;
        recursion : Natural)
      return MProb;
      --
      --  Empirical, tuned, magic numbers
      --
      --  Over the long run, it's better to let repeat matches happen.
      Malus_simple_match_vs_rep : constant := 0.55;
      --  DL code for short lengths may be unnecessary and replaced by fully expanded bytes.
      Malus_DL_short_len : constant := 0.995;
      --  It is better to split a DL code as a very frequent literal, then a DL code with length-1.
      Lit_then_DL_threshold : constant := 0.875;   -- naive approach: literal's prob. only considered
      Malus_lit_then_DL     : constant := 0.0625;  -- full evaluation
      --
      Malus_DL_then_lit : constant := 0.125;
    end Predicted;

    package body Predicted is
      To_Prob_Factor : constant MProb := 1.0 / MProb'Base (Probability_model_count);

      function To_Math (cp : CProb) return MProb is
      pragma Inline (To_Math);
      begin
        return MProb'Base (cp) * To_Prob_Factor;
      end To_Math;

      function Simulate_bit (prob_bit : CProb; bit : Unsigned) return MProb is
      pragma Inline (Simulate_bit);
        b : constant MProb'Base := MProb'Base (bit);  --  b = 0.0 or 1.0
      begin
        return b + (1.0 - 2.0 * b) * To_Math (prob_bit);
        --  Branch-less equivalent of:
        --    if bit = 0 then
        --      return prob_bit;
        --    else
        --      return 1.0 - prob_bit;
        --    end if;
      end Simulate_bit;

      function Strict_Literal (
        b, b_match    : Byte;
        prob          : CProb_array;
        sim_state     : State_range;
        sim_pos_state : Pos_state_range
      ) return MProb
      is
        prob_lit : MProb := Simulate_bit (probs.switch.match (sim_state, sim_pos_state), Literal_choice);
        symb : UInt32 := UInt32 (b) or 16#100#;
        --
        procedure Simulate_Literal is
        begin
          loop
            prob_lit := prob_lit *
              Simulate_bit (
                prob_bit => prob (Integer (Shift_Right (symb, 8)) + prob'First),
                bit      => Unsigned (Shift_Right (symb, 7)) and 1
              );
            symb := Shift_Left (symb, 1);
            exit when symb >= 16#10000#;
          end loop;
        end Simulate_Literal;
        --
        procedure Simulate_Literal_Matched is
          offs  : UInt32 := 16#100#;
          match : UInt32 := UInt32 (b_match);
        begin
          loop
            match := Shift_Left (match, 1);
            prob_lit := prob_lit *
              Simulate_bit (
                prob_bit => prob (Integer (offs + (match and offs) +
                                                 Shift_Right (symb, 8)) + prob'First),
                bit      => Unsigned (Shift_Right (symb, 7)) and 1
              );
            symb := Shift_Left (symb, 1);
            offs := offs and not (match xor symb);
            exit when symb >= 16#10000#;
          end loop;
        end Simulate_Literal_Matched;
        --
      begin
        if sim_state < 7 then
          Simulate_Literal;
        else
          Simulate_Literal_Matched;
        end if;
        return prob_lit;
      end Strict_Literal;

      function Short_Rep_Match (
        sim_state     : State_range;
        sim_pos_state : Pos_state_range
      ) return MProb
      is
      begin
        return
          Simulate_bit (probs.switch.match (sim_state, sim_pos_state), DL_code_choice) *
          Simulate_bit (probs.switch.rep (sim_state), Rep_match_choice) *
          Simulate_bit (probs.switch.rep_g0 (sim_state), The_distance_is_rep0_choice) *
          Simulate_bit (probs.switch.rep0_long (sim_state, sim_pos_state), The_length_is_1_choice);
      end Short_Rep_Match;

      --  We simulate here LZ77_emits_literal_byte.
      procedure Any_literal (
        b, b_match, b_prev : Byte;  --  b_match is the byte at distance rep_dist(0) + 1.
        offset             : Data_Bytes_Count;
        sim_state          : in out State_range;
        sim_rep_dist_0     : UInt32;
        prob               : in out MProb
      )
      is
        probs_lit_idx : constant Integer := Idx_for_Literal_prob (total_pos + offset, b_prev);
        sim_pos_state : constant Pos_state_range := Pos_state_range (UInt32 (total_pos + offset) and pos_bits_mask);
        ltr : constant MProb :=
          Strict_Literal (b, b_match, probs.lit (probs_lit_idx .. probs.lit'Last), sim_state, sim_pos_state);
        srm : MProb;
      begin
        if b = b_match and then total_pos + offset > Data_Bytes_Count (sim_rep_dist_0 + 1) then
          srm := Short_Rep_Match (sim_state, sim_pos_state);
          if srm > ltr then
            --  Short Rep would be preferred.
            sim_state := Update_State_ShortRep (sim_state);
            prob := prob * srm;
            return;
          end if;
        end if;
        sim_state := Update_State_Literal (sim_state);
        prob := prob * ltr;
      end Any_literal;

      function Any_literal (
        b, b_match, b_prev : Byte;  --  b_match is the byte at distance rep_dist(0) + 1.
        sim_state          : State_range;
        offset             : Data_Bytes_Count
      )
      return MProb
      is
        sim_state_var : State_range := sim_state;
        prob : MProb := 1.0;
      begin
        Any_literal (b, b_match, b_prev, offset, sim_state_var, rep_dist (0), prob);
        return prob;
      end Any_literal;

      function Simulate_Bit_Tree (prob : CProb_array; num_bits : Positive; symbol : Unsigned) return MProb is
        res : MProb := 1.0;
        bit, m : Unsigned;
      begin
        m := 1;
        for i in reverse 0 .. num_bits - 1 loop
          bit := Unsigned (Shift_Right (UInt32 (symbol), i)) and 1;
          res := res * Simulate_bit (prob (Integer (m) + prob'First), bit);
          m := m + m + bit;
        end loop;
        return res;
      end Simulate_Bit_Tree;

      function Simulate_Length (probs_len : Probs_for_LZ_Lengths; length : Unsigned) return MProb is
        len : Unsigned := length - Min_match_length;
        res : MProb;
      begin
        if len < Len_low_symbols then
          res := Simulate_bit (probs_len.choice_1, 0) *
                 Simulate_Bit_Tree (probs_len.low_coder (pos_state), Len_low_bits, len);
        else
          res := Simulate_bit (probs_len.choice_1, 1);
          len := len - Len_low_symbols;
          if len < Len_mid_symbols then
            res := res * Simulate_bit (probs_len.choice_2, 0)
                       * Simulate_Bit_Tree (probs_len.mid_coder (pos_state), Len_mid_bits, len);
          else
            res := res * Simulate_bit (probs_len.choice_2, 1);
            len := len - Len_mid_symbols;
            res := res * Simulate_Bit_Tree (probs_len.high_coder, Len_high_bits, len);
          end if;
        end if;
        return res;
      end Simulate_Length;

      function Repeat_Match (index_rm : Repeat_stack_range; length : Unsigned) return MProb is
        res : MProb := Simulate_bit (probs.switch.rep (state), Rep_match_choice);
      begin
        case index_rm is
          when 0 =>
            res := res * Simulate_bit (probs.switch.rep_g0 (state), The_distance_is_rep0_choice)
                       * Simulate_bit (probs.switch.rep0_long (state, pos_state), The_length_is_not_1_choice);
          when 1 =>
            res := res * Simulate_bit (probs.switch.rep_g0 (state), The_distance_is_not_rep0_choice)
                       * Simulate_bit (probs.switch.rep_g1 (state), The_distance_is_rep1_choice);
          when 2 =>
            res := res * Simulate_bit (probs.switch.rep_g0 (state), The_distance_is_not_rep0_choice)
                       * Simulate_bit (probs.switch.rep_g1 (state), The_distance_is_not_rep1_choice)
                       * Simulate_bit (probs.switch.rep_g2 (state), The_distance_is_rep2_choice);
          when 3 =>
            res := res * Simulate_bit (probs.switch.rep_g0 (state), The_distance_is_not_rep0_choice)
                       * Simulate_bit (probs.switch.rep_g1 (state), The_distance_is_not_rep1_choice)
                       * Simulate_bit (probs.switch.rep_g2 (state), The_distance_is_not_rep2_choice);
        end case;
        return res * Simulate_Length (probs.rep_len, length);
      end Repeat_Match;

      function Simple_Match (distance : UInt32; length : Unsigned) return MProb is
        --
        function Simulate_Bit_Tree_Reverse (prob : CProb_array; num_bits : Natural; symbol : UInt32)
        return MProb
        is
          res : MProb := 1.0;
          symb : UInt32 := symbol;
          m : Unsigned := 1;
          bit : Unsigned;
        begin
          for count_bits in reverse 1 .. num_bits loop
            bit := Unsigned (symb) and 1;
            res := res * Simulate_bit (prob (Integer (m) + prob'First), bit);
            m := m + m + bit;
            symb := Shift_Right (symb, 1);
          end loop;
          return res;
        end Simulate_Bit_Tree_Reverse;
        --
        function Simulate_Distance return MProb is
          len_state : constant Unsigned := Unsigned'Min (length - 2, Len_to_pos_states - 1);
          dist_slot : constant Unsigned := Get_dist_slot (distance);
          base, dist_reduced : UInt32;
          footerBits : Natural;
          res : MProb;
        begin
          res := Simulate_Bit_Tree (probs.dist.slot_coder (len_state), Dist_slot_bits, dist_slot);
          if dist_slot >= Start_dist_model_index then
            footerBits := Natural (Shift_Right (UInt32 (dist_slot), 1)) - 1;
            base := Shift_Left (UInt32 (2 or (dist_slot and 1)), footerBits);
            dist_reduced := distance - base;
            if dist_slot < End_dist_model_index then
              res := res *
                Simulate_Bit_Tree_Reverse (
                  probs.dist.pos_coder (Integer (base) - Integer (dist_slot) - 1 .. Pos_coder_range'Last),
                  footerBits,
                  dist_reduced
                );
            else
              res := res *
                (0.5 ** (footerBits - Align_bits)) *  --  direct bits
                Simulate_Bit_Tree_Reverse (
                  probs.dist.align_coder,
                  Align_bits,
                  dist_reduced and Align_mask
                );
            end if;
          end if;
          return res;
        end Simulate_Distance;
      begin
        return
          Simulate_bit (probs.switch.rep (state), Simple_match_choice) *
          Simulate_Length (probs.len, length) *
          Simulate_Distance;
      end Simple_Match;

      --  We simulate here LZ77_emits_DL_code
      function Strict_DL_code (
        distance      : Integer;
        length        : Match_length_range;
        sim_state     : State_range;
        sim_pos_state : Pos_state_range
      ) return MProb
      is
        dist_ip : constant UInt32 := UInt32 (distance - 1);
        found_repeat : Integer := rep_dist'First - 1;
        dlc : constant MProb := Simulate_bit (probs.switch.match (sim_state, sim_pos_state), DL_code_choice);
        sma : constant MProb := Simple_Match (dist_ip, Unsigned (length));
        rma : MProb;
      begin
        for i in rep_dist'Range loop
          if dist_ip = rep_dist (i) then
            found_repeat := i;
            exit;
          end if;
        end loop;
        if found_repeat >= rep_dist'First then
          rma := Repeat_Match (found_repeat, Unsigned (length));
          if rma >= sma * Malus_simple_match_vs_rep  then
            return dlc * rma;
          end if;
        end if;
        return dlc * sma;
      end Strict_DL_code;

      function Expanded_DL_code (
        distance : Integer;
        length   : Match_length_range;
        give_up  : MProb)
      return MProb
      is
        b : Byte;
        sim_prev_byte : Byte := prev_byte;
        sim_state : State_range := state;
        --
        expanded_string_prob : MProb := 1.0;
      begin
        for x in 0 .. length - 1 loop
          b := Text_Buf ((R + UInt32 (x) - UInt32 (distance)) and Text_Buf_Mask);
          Any_literal (
            b              => b,
            b_match        => Text_Buf ((R + UInt32 (x) - rep_dist (0) - 1)  and Text_Buf_Mask),
            b_prev         => sim_prev_byte,
            offset         => Data_Bytes_Count (x),
            sim_state      => sim_state,
            sim_rep_dist_0 => rep_dist (0),
            prob           => expanded_string_prob
          );
          --  Probability is decreasing over the loop, useless to continue under given threshold.
          exit when expanded_string_prob < give_up;
          sim_prev_byte := b;
        end loop;
        return expanded_string_prob;
      end Expanded_DL_code;

      function DL_code_then_Literal (
        distance  : Integer;
        length    : Match_length_range;
        recursion : Natural)
      return MProb
      is
        b : Byte;
        sim_prev_byte : Byte := prev_byte;
        sim_state : State_range := state;
        sim_rep_dist_0 : UInt32 := rep_dist (0);
        --
        prob : MProb := 1.0;
        dlc : MProb := Strict_DL_code (distance, length - 1, sim_state, pos_state) * Malus_DL_short_len;
      begin
        if recursion > 0 and then length > Min_match_length + 1 then
          --  The "DL + Lit" optimization will be done recursively "in real",
          --  we can do it as well in the simulation.
          dlc := MProb'Max (dlc,
            Malus_DL_then_lit * DL_code_then_Literal (distance, length - 1, recursion - 1));
        end if;
        --
        --  We have first a DL code of length 'length-1'. The real compression would try to
        --  look for a full expansion if it is more probable (=> less space). We simulate that.
        for x in 0 .. length - 2 loop
          b := Text_Buf ((R + UInt32 (x) - UInt32 (distance)) and Text_Buf_Mask);
          Any_literal (
            b              => b,
            b_match        => Text_Buf ((R + UInt32 (x) - sim_rep_dist_0 - 1)  and Text_Buf_Mask),
            b_prev         => sim_prev_byte,
            offset         => Data_Bytes_Count (x),
            sim_state      => sim_state,
            sim_rep_dist_0 => sim_rep_dist_0,
            prob           => prob
          );
          if prob < dlc then
            --  Expansion would be less efficient, so we simulate the DL code.
            prob := dlc;
            sim_prev_byte := Text_Buf ((R + UInt32 (length - 2) - UInt32 (distance)) and Text_Buf_Mask);
            sim_state := Update_State_Match (state);  --  !! approximative: could be a rep match (sigh)...
            --  The match (any: simple or repeat) always sets this:
            sim_rep_dist_0 := UInt32 (distance) - 1;
            exit;
          end if;
          sim_prev_byte := b;
        end loop;
        --  In this scenario, the last byte of the match is always sent as a literal.
        Any_literal (
          --  Note that if there was a real DL code simulated, b = b_match :-)
          b              => Text_Buf ((R + UInt32 (length - 1) - UInt32 (distance))   and Text_Buf_Mask),
          b_match        => Text_Buf ((R + UInt32 (length - 1) - sim_rep_dist_0 - 1)  and Text_Buf_Mask),
          b_prev         => sim_prev_byte,
          offset         => Data_Bytes_Count (length - 1),
          sim_state      => sim_state,
          sim_rep_dist_0 => sim_rep_dist_0,
          prob           => prob
        );
        return prob;
      end DL_code_then_Literal;

    end Predicted;

    -----------------------------------------------------------------------------------
    --  This part processes the case where LZ77 sends a literal (a plain text byte)  --
    -----------------------------------------------------------------------------------

    procedure Write_Literal (prob : in out CProb_array; symbol : in UInt32) is
      symb : UInt32 := symbol or 16#100#;
    begin
      loop
        Encode_Bit ( --  Prob. offset is always 1, 2, 4, 8, .. , 128
          prob   => prob (Integer (Shift_Right (symb, 8)) + prob'First),
          symbol => Unsigned (Shift_Right (symb, 7)) and 1
        );
        symb := Shift_Left (symb, 1);
        exit when symb >= 16#10000#;
      end loop;
    end Write_Literal;

    procedure Write_Literal_Matched (prob : in out CProb_array; symbol, matched : in UInt32) is
      symb  : UInt32 := symbol or 16#100#;
      offs  : UInt32 := 16#100#;
      match : UInt32 := matched;
    begin
      loop
        match := Shift_Left (match, 1);
        Encode_Bit (
          prob   => prob (Integer (offs + (match and offs) + Shift_Right (symb, 8)) + prob'First),
          symbol => Unsigned (Shift_Right (symb, 7)) and 1
        );
        symb := Shift_Left (symb, 1);
        offs := offs and not (match xor symb);
        exit when symb >= 16#10000#;
      end loop;
    end Write_Literal_Matched;

    use type Predicted.MProb;
    quick : constant Boolean := level <= Level_1;

    procedure LZ77_emits_literal_byte (b : Byte) is
      pb_lit_idx : constant Integer := Idx_for_Literal_prob (total_pos, prev_byte);
      b_match : constant Byte := Text_Buf ((R - rep_dist (0) - 1) and Text_Buf_Mask);
    begin
      if b = b_match and then total_pos > Data_Bytes_Count (rep_dist (0) + 1)
        and then
          (quick
             or else
           Predicted.Short_Rep_Match (state, pos_state) >
           Predicted.Strict_Literal (b, b_match, probs.lit (pb_lit_idx .. probs.lit'Last), state, pos_state))
      then
        --  We are lucky: both bytes are the same. No literal to encode, "Short Rep Match"
        --  case, and its cost (4 bits) is more affordable than the literal's cost.
        Encode_Bit (probs.switch.match (state, pos_state), DL_code_choice);
        Encode_Bit (probs.switch.rep (state), Rep_match_choice);
        Encode_Bit (probs.switch.rep_g0 (state), The_distance_is_rep0_choice);
        Encode_Bit (probs.switch.rep0_long (state, pos_state), The_length_is_1_choice);
        state := Update_State_ShortRep (state);
      else
        Encode_Bit (probs.switch.match (state, pos_state), Literal_choice);
        if state < 7 then
          Write_Literal (probs.lit (pb_lit_idx .. probs.lit'Last), UInt32 (b));
        else
          Write_Literal_Matched (probs.lit (pb_lit_idx .. probs.lit'Last), UInt32 (b), UInt32 (b_match));
        end if;
        state := Update_State_Literal (state);
      end if;
      total_pos := total_pos + 1;
      Update_pos_state;
      prev_byte := b;
      Text_Buf (R) := b;
      R := (R + 1) and Text_Buf_Mask;  --  This is mod String_buffer_size
    end LZ77_emits_literal_byte;

    ---------------------------------------------------------------------------------
    --  This part processes the case where LZ77 sends a Distance-Length (DL) code  --
    ---------------------------------------------------------------------------------

    procedure Bit_Tree_Encode (
      prob     : in out CProb_array;
      num_bits :        Positive;
      symbol   :        Unsigned)
    is
      bit, m : Unsigned;
    begin
      m := 1;
      for i in reverse 0 .. num_bits - 1 loop
        bit := Unsigned (Shift_Right (UInt32 (symbol), i)) and 1;
        Encode_Bit (prob (Integer (m) + prob'First), bit);
        m := m + m + bit;
      end loop;
    end Bit_Tree_Encode;

    procedure Encode_Length (probs_len : in out Probs_for_LZ_Lengths; length : Unsigned) is
      len : Unsigned := length - Min_match_length;
    begin
      if len < Len_low_symbols then
        Encode_Bit (probs_len.choice_1, 0);
        --  LZ length in [2..9], i.e. len in [0..7]
        Bit_Tree_Encode (probs_len.low_coder (pos_state), Len_low_bits, len);
      else
        Encode_Bit (probs_len.choice_1, 1);
        len := len - Len_low_symbols;
        if len < Len_mid_symbols then
          Encode_Bit (probs_len.choice_2, 0);
          --  LZ length in [10..17], i.e. len in [0..7]
          Bit_Tree_Encode (probs_len.mid_coder (pos_state), Len_mid_bits, len);
        else
          Encode_Bit (probs_len.choice_2, 1);
          len := len - Len_mid_symbols;
          --  LZ length in [18..273], i.e. len in [0..255]
          Bit_Tree_Encode (probs_len.high_coder, Len_high_bits, len);
        end if;
      end if;
    end Encode_Length;

    procedure Write_Simple_Match (distance : UInt32; length : Unsigned) is
      --
      procedure Bit_Tree_Reverse_Encode (
        prob     : in out CProb_array;
        num_bits : in     Natural;
        symbol   : in     UInt32
      )
      is
        symb : UInt32 := symbol;
        m : Unsigned := 1;
        bit : Unsigned;
      begin
        for count_bits in reverse 1 .. num_bits loop
          bit := Unsigned (symb) and 1;
          Encode_Bit (prob (Integer (m) + prob'First), bit);
          m := m + m + bit;
          symb := Shift_Right (symb, 1);
        end loop;
      end Bit_Tree_Reverse_Encode;

      --  Range encoding of num_bits with equiprobability.
      --
      procedure Encode_Direct_Bits (value : UInt32; num_bits : Natural) is
      begin
        for i in reverse 0 .. num_bits - 1 loop
          --  Bound is the half width. New width is halved anyway.
          range_enc.width := Shift_Right (range_enc.width, 1);
          --  Either low is unchanged (bit=0), or new low := old low + bound (bit=1).
          range_enc.low := range_enc.low +
            (UInt64 (range_enc.width) and (0 - UInt64 (Shift_Right (value, i) and 1)));
          Normalize;
        end loop;
      end Encode_Direct_Bits;
      --
      procedure Encode_Distance is
        len_state : constant Unsigned := Unsigned'Min (length - 2, Len_to_pos_states - 1);
        dist_slot : constant Unsigned := Get_dist_slot (distance);
        base, dist_reduced : UInt32;
        footerBits : Natural;
      begin
        Bit_Tree_Encode (probs.dist.slot_coder (len_state), Dist_slot_bits, dist_slot);
        if dist_slot >= Start_dist_model_index then
          footerBits := Natural (Shift_Right (UInt32 (dist_slot), 1)) - 1;
          base := Shift_Left (UInt32 (2 or (dist_slot and 1)), footerBits);
          dist_reduced := distance - base;
          if dist_slot < End_dist_model_index then
            Bit_Tree_Reverse_Encode (
              probs.dist.pos_coder (Integer (base) - Integer (dist_slot) - 1 .. Pos_coder_range'Last),
              footerBits,
              dist_reduced
            );
          else
            Encode_Direct_Bits (Shift_Right (dist_reduced, Align_bits), footerBits - Align_bits);
            Bit_Tree_Reverse_Encode (
              probs.dist.align_coder,
              Align_bits,
              dist_reduced and Align_mask
            );
          end if;
        end if;
      end Encode_Distance;
      --
    begin
      Encode_Bit (probs.switch.rep (state), Simple_match_choice);
      state := Update_State_Match (state);
      Encode_Length (probs.len, length);
      Encode_Distance;
      --  Shift the stack of recent distances; the new distance becomes the first item.
      for i in reverse 1 .. Repeat_stack_range'Last loop
        rep_dist (i) := rep_dist (i - 1);
      end loop;
      rep_dist (0) := distance;
    end Write_Simple_Match;

    procedure Write_Repeat_Match (index_rm : Repeat_stack_range; length : Unsigned) is
      aux : UInt32;
    begin
      Encode_Bit (probs.switch.rep (state), Rep_match_choice);
      case index_rm is
        when 0 =>
          Encode_Bit (probs.switch.rep_g0 (state), The_distance_is_rep0_choice);
          Encode_Bit (probs.switch.rep0_long (state, pos_state), The_length_is_not_1_choice);
        when 1 =>
          Encode_Bit (probs.switch.rep_g0 (state), The_distance_is_not_rep0_choice);
          Encode_Bit (probs.switch.rep_g1 (state), The_distance_is_rep1_choice);
        when 2 =>
          Encode_Bit (probs.switch.rep_g0 (state), The_distance_is_not_rep0_choice);
          Encode_Bit (probs.switch.rep_g1 (state), The_distance_is_not_rep1_choice);
          Encode_Bit (probs.switch.rep_g2 (state), The_distance_is_rep2_choice);
        when 3 =>
          Encode_Bit (probs.switch.rep_g0 (state), The_distance_is_not_rep0_choice);
          Encode_Bit (probs.switch.rep_g1 (state), The_distance_is_not_rep1_choice);
          Encode_Bit (probs.switch.rep_g2 (state), The_distance_is_not_rep2_choice);
      end case;
      --  Roll the stack of recent distances up to the found item, which becomes the first one.
      aux := rep_dist (index_rm);
      for i in reverse 1 .. index_rm loop
        rep_dist (i) := rep_dist (i - 1);
      end loop;
      rep_dist (0) := aux;
      --
      Encode_Length (probs.rep_len, length);
      state := Update_State_Rep (state);
    end Write_Repeat_Match;

    procedure LZ77_emits_DL_code (distance : Integer; length : Match_length_range) is
      Copy_start : constant UInt32 := (R - UInt32 (distance)) and Text_Buf_Mask;
      dist_ip : constant UInt32 := UInt32 (distance - 1);
      found_repeat : Integer := rep_dist'First - 1;
      short : constant := 18;  --  tuned - magic number
      use Predicted;
      strict_dlc, expanded_dlc, any_dlc, dlc_after_lit, dl_then_lit, head_lit : MProb;
      b_head, b_match, b_tail : Byte;
      dlc_computed : Boolean := False;
      procedure Compute_dlc_variants is
      begin
        if not dlc_computed then
          strict_dlc := Predicted.Strict_DL_code (distance, length, state, pos_state) *
                        Predicted.Malus_DL_short_len;
          expanded_dlc := Predicted.Expanded_DL_code (distance, length, strict_dlc);
          any_dlc := MProb'Max (strict_dlc, expanded_dlc);
          dlc_computed := True;
        end if;
      end Compute_dlc_variants;
      sim_pos_state : Pos_state_range;
    begin
      --  Distance-Length code of small length.
      --  It may be better just to expand it as plain literals, fully or partially.
      if (not quick) and then length <= short and then distance >= length then
        --  Consider shorten the DL code's length
        if length > Min_match_length then
          b_head   := Text_Buf (Copy_start and Text_Buf_Mask);
          b_match  := Text_Buf ((R - rep_dist (0) - 1) and Text_Buf_Mask);
          head_lit := Any_literal (b_head, b_match, prev_byte, state, 0);
          --  Literal + shorter DL (naive approach)
          if head_lit >= Predicted.Lit_then_DL_threshold then
            LZ77_emits_literal_byte (b_head);
            LZ77_emits_DL_code (distance, length - 1);
            return;
          end if;
          Compute_dlc_variants;
          --  Literal + shorter DL
          sim_pos_state := Pos_state_range (UInt32 (total_pos + 1) and pos_bits_mask);
          dlc_after_lit := Predicted.Strict_DL_code (distance, length - 1, Update_State_Literal (state), sim_pos_state);
          if head_lit * dlc_after_lit * Predicted.Malus_lit_then_DL > any_dlc then
            LZ77_emits_literal_byte (b_head);
            LZ77_emits_DL_code (distance, length - 1);
            return;
          end if;
          --  Shorter DL + literal
          dl_then_lit :=
            Predicted.DL_code_then_Literal (distance, length, 1) *
            Predicted.Malus_DL_then_lit;
          if dl_then_lit > any_dlc then
            b_tail := Text_Buf ((Copy_start + UInt32 (length - 1)) and Text_Buf_Mask);
            LZ77_emits_DL_code (distance, length - 1);
            LZ77_emits_literal_byte (b_tail);
            return;
          end if;
        end if;
        --
        Compute_dlc_variants;
        if expanded_dlc > strict_dlc then
          --  Full expansion of DL code as literals
          for x in 1 .. length loop
            LZ77_emits_literal_byte (Text_Buf ((Copy_start + UInt32 (x - 1)) and Text_Buf_Mask));
          end loop;
          return;
        end if;
      end if;
      --  Go for the DL code, stricto sensu.
      Encode_Bit (probs.switch.match (state, pos_state), DL_code_choice);
      for i in rep_dist'Range loop
        if dist_ip = rep_dist (i) then
          found_repeat := i;
          exit;
        end if;
      end loop;
      if found_repeat >= rep_dist'First
        and then
          (quick
             or else
           Predicted.Repeat_Match (found_repeat, Unsigned (length)) >=
           Predicted.Simple_Match (dist_ip, Unsigned (length)) *
           Predicted.Malus_simple_match_vs_rep)
      then
        Write_Repeat_Match (found_repeat, Unsigned (length));
      else
        Write_Simple_Match (dist_ip, Unsigned (length));
      end if;
      total_pos := total_pos + Data_Bytes_Count (length);
      Update_pos_state;
      --  Expand in the circular text buffer to have it up to date
      for K in 0 .. UInt32 (length - 1) loop
        Text_Buf (R) := Text_Buf ((Copy_start + K) and Text_Buf_Mask);
        R := (R + 1) and Text_Buf_Mask;  --  This is mod String_buffer_size
      end loop;
      prev_byte := Text_Buf ((R - 1) and Text_Buf_Mask);
    end LZ77_emits_DL_code;

    procedure My_LZ77 is
      new LZ77.Encode
         (String_buffer_size => String_buffer_size (level),
          Look_Ahead         => Max_length (level),
          Threshold          => Min_length (level) - 1,
          Method             => LZ77_choice (level),
          Read_byte          => Read_Byte,
          More_bytes         => More_Bytes,
          Write_literal      => LZ77_emits_literal_byte,
          Write_DL_code      => LZ77_emits_DL_code
        );

    procedure Write_LZMA_header is
      dw : UInt32 := params.dict_size;
      uw : Data_Bytes_Count := params.unpack_size;
    begin
      --  5-byte header
      Write_Byte (Byte (params.lc + 9 * params.lp + 9 * 5 * params.pb));
      for i in 0 .. 3 loop
        Write_Byte (Byte (dw mod 256));
        dw := dw / 256;
      end loop;
      --  8 bytes for unpacked size.
      --  This part of the header is optional => you need a
      --  prior knowledge or a "pre-header" indicating its presence or not.
      if params.header_has_size then
        for i in 0 .. 7 loop
          if params.unpack_size_defined then
            Write_Byte (Byte (uw mod 256));
            uw := uw / 256;
          else
            Write_Byte (16#FF#);
          end if;
        end loop;
      end if;
    end Write_LZMA_header;

  begin
    Write_LZMA_header;
    My_LZ77;
    if params.has_end_mark then
      --  The end-of-stream marker is a fake "Simple Match" with a special distance.
      Encode_Bit (probs.switch.match (state, pos_state), DL_code_choice);
      Write_Simple_Match (
        distance => end_of_stream_magic_distance,
        length   => Min_match_length
      );
    end if;
    Flush_range_encoder;
    Dispose (Text_Buf);
  end Encode;

end LZMA.Encoding;
