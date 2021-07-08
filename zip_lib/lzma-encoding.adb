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

--  Copyright (c) 2016 .. 2020 Gautier de Montmollin
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
      (Level_1 | Level_2  => 3,     --  Deflate's minimum value
       others             => 2);    --  LZMA's minimum value

    Max_length : constant array (Compression_level) of Positive :=
      (Level_1 | Level_2  => 258,   --  Deflate's maximum value
       others             => 273);  --  LZMA's maximum value

    extra_size : constant := 273 + 1 + LZ77.BT4_max_prefetch_positions;
    --  Extra space is used for DL codes scoring before being
    --  sent for real to the encoder.

    --  String_buffer_size: the actual dictionary size used.
    String_buffer_size : constant array (Compression_level) of Positive :=
      (Level_0            => 16,       --  Fake: actually we don't use any LZ77 for level 0
       Level_1 | Level_2  => 2 ** 15,  --  Deflate's Value: 32 KiB
       Level_3            =>
         Integer'Max (
           Min_dictionary_size,                --  minimum:  4 KiB
           Integer'Min (
             --    dictionary_size is specified; default is 32 KiB
             Ceiling_power_of_2 (dictionary_size + extra_size),
             2 ** 28                          --  maximum: 256 MiB
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

    --  Small stack of recent distances used for LZ. Required: initialized with zero values.
    --  lzma-specification.txt: "That set of 4 variables contains zero-based match
    --  distances and these variables are initialized with zero values"
    --
    subtype Repeat_stack_range is Integer range 0 .. 3;
    type Repeat_Stack is array (Repeat_stack_range) of UInt32;
    --
    probs : All_probabilities (last_lit_prob_index => 16#300# * 2 ** (params.lc + params.lp) - 1);
    pos_bits_mask    : constant UInt32 := 2 ** params.pb - 1;
    literal_pos_mask : constant UInt32 := 2 ** params.lp - 1;

    --  We expand the DL codes in order to have some past data.
    subtype Text_Buffer_Index is UInt32 range 0 .. UInt32 (String_buffer_size (level) - 1);
    type Text_Buffer is array (Text_Buffer_Index) of Byte;
    Text_Buf_Mask : constant UInt32 := UInt32 (String_buffer_size (level) - 1);
    --  NB: heap allocation (and then, the only pointer in this package) is used
    --      only for convenience because of small default stack sizes on some compilers.
    type p_Text_Buffer is access Text_Buffer;
    procedure Dispose is new Ada.Unchecked_Deallocation (Text_Buffer, p_Text_Buffer);
    Text_Buf : p_Text_Buffer := new Text_Buffer;

    function Idx_for_Literal_prob (position : Data_Bytes_Count; prev_byte : Byte) return Integer is
    pragma Inline (Idx_for_Literal_prob);
    begin
      return 16#300# *
          Integer (
            Shift_Left (UInt32 (position) and literal_pos_mask, params.lc) +
            Shift_Right (UInt32 (prev_byte), 8 - params.lc)
          );
    end Idx_for_Literal_prob;

    type Variants_Comparison_Choice is
      (
        None,      --  "Mechanical" encoding, straight from the LZ77 algorithm.
        Simple,    --  Compare simple alternative encodings and choose the most probable.
        Splitting  --  More advanced search for alternatives.
      );

    compare_variants : Variants_Comparison_Choice;

    type Machine_State is record
      state     : State_range;
      pos_state : Pos_state_range;
      prev_byte : Byte;
      R         : UInt32;
      total_pos : Data_Bytes_Count;
      rep_dist  : Repeat_Stack;
    end record;

    -------------------------
    --  Package Estimates  --
    -------------------------
    --
    --  Purpose: estimate probabilities of different alternative
    --  encodings, in order to choose the most probable encoding.
    --  Note that the LZMA encoder is already very efficient by
    --  taking the obvious choices. It is possible to ignore this
    --  package and its uses (see occurrences of "compare_variants").
    --
    --  In the following probability computations, we assume independent
    --  (multiplicative) probabilities, just like the range encoder does
    --  when adapting the range width. With higher probabilities, the width
    --  will decrease less and the compression will be better.
    --  Since the probability model is constantly adapting, we have kind of self-fulfilling
    --  predictions - e.g. if a Short Rep Match is chosen against a Literal, the context
    --  probabilities of the former will be increased instead of the latter.

    package Estimates is
      type MProb is digits 15 range 0.0 .. 1.0;
      --
      --  Literals
      --
      procedure Simulate_Literal_Byte (b : Byte; sim : in out Machine_State; prob : in out MProb);
      --
      function Test_Simple_Literal (
        b, b_match : Byte;
        prob       : CProb_array;
        sim        : Machine_State
      )
      return MProb;
      --
      function Test_Short_Rep_Match (sim : Machine_State) return MProb;
      --
      function Test_Literal_Byte (b : Byte; sim : Machine_State) return MProb;
      --
      --  Matches
      --
      function Test_Repeat_Match (
        index_rm : Repeat_stack_range;
        length   : Unsigned;
        sim      : Machine_State
      )
      return MProb;
      --
      function Test_Simple_Match (
        distance : UInt32;
        length   : Unsigned;
        sim      : Machine_State
      )
      return MProb;

      --  End of the obvious cases. Now things get tougher...

      --  Here we get the probability of a general DL code
      --  as Write_any_DL_code would generate it, including variants.

      procedure Simulate_any_DL_Code (
        distance        :        UInt32;
        length          :        Match_length_range;
        sim             : in out Machine_State;
        prob            : in out MProb;
        recursion_limit :        Natural
      );

      function Test_any_DL_Code (
        distance        : UInt32;
        length          : Match_length_range;
        sim             : Machine_State;
        recursion_limit : Natural
      )
      return MProb;

      --  Constants like 0.1234 appearing hereafter are empirical, tuned, magic numbers.
      --  To do: tune them with Machine Learning.

      --  Sometimes, a longer path like sending a simple match
      --  instead of a repeat match has a lower modelled probability.
      --  To avoid underusing repeat matches by letting their probabilities
      --  being adapted lower over time, we penalize the simple match alternative.
      Malus_simple_match_vs_rep : constant := 0.55;

      package DL_Code_Erosion is
        --  It is sometimes better to split a DL code as a very frequent literal,
        --  then a very frequent DL code with length-1.
        Lit_then_DL_threshold : constant := 0.875;
        --
        function Malus_lit_then_DL (distance : UInt32; length : Match_length_range) return MProb;
        pragma Inline (Malus_lit_then_DL);
        --  Case of DL code split into a shorter DL code, then a literal.
        function DL_code_then_Literal (
          distance        : UInt32;
          length          : Match_length_range;
          sim             : Machine_State;
          recursion_limit : Natural
        )
        return MProb;
      end DL_Code_Erosion;

      --  Here we define a generic DL code emission that is the same
      --  for simulation and actual writes. This way, we don't need to
      --  synchronize two pieces of Ada code doing the same operation,
      --  one in simulation and the other in real.
      --
      generic
        with procedure Simulated_or_actual_Literal_Byte (
          b    :        Byte;
          sim  : in out Machine_State;
          prob : in out MProb);
        --
        with procedure Simulated_or_actual_Strict_DL_Code (
          distance :        UInt32;
          length   :        Match_length_range;
          sim      : in out Machine_State;
          prob     : in out MProb
        );
        --
        I_am_a_simulation : Boolean;
      procedure Generic_any_DL_Code (
        distance        :        UInt32;
        length          :        Match_length_range;
        sim             : in out Machine_State;
        prob            : in out MProb;
        recursion_limit :        Natural
      );

    end Estimates;

    package body Estimates is
      To_Prob_Factor : constant  --  We compute the division (more expensive) at compile-time.
        MProb := 1.0 / MProb'Base (Probability_model_count);

      function To_Math (cp : CProb) return MProb is
      pragma Inline (To_Math);
      begin
        return MProb'Base (cp) * To_Prob_Factor;
      end To_Math;

      function Test_Bit_Encoding (prob_bit : CProb; symbol : Unsigned) return MProb is
      pragma Inline (Test_Bit_Encoding);
        b : constant MProb'Base := MProb'Base (symbol);  --  b = 0.0 or 1.0
      begin
        return b + (1.0 - 2.0 * b) * To_Math (prob_bit);
        --  Branch-less equivalent of:
        --    if bit = 0 then
        --      return prob_bit;
        --    else
        --      return 1.0 - prob_bit;
        --    end if;
      end Test_Bit_Encoding;

      function Test_Simple_Literal (
        b, b_match    : Byte;
        prob          : CProb_array;
        sim           : Machine_State
      ) return MProb
      is
        prob_lit : MProb := Test_Bit_Encoding (probs.switch.match (sim.state, sim.pos_state), Literal_choice);
        symb : UInt32 := UInt32 (b) or 16#100#;
        --
        procedure Simulate_Literal is
        begin
          loop
            prob_lit := prob_lit *
              Test_Bit_Encoding (
                prob_bit => prob (Integer (Shift_Right (symb, 8)) + prob'First),
                symbol   => Unsigned (Shift_Right (symb, 7)) and 1
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
              Test_Bit_Encoding (
                prob_bit => prob (Integer (offs + (match and offs) +
                                                 Shift_Right (symb, 8)) + prob'First),
                symbol   => Unsigned (Shift_Right (symb, 7)) and 1
              );
            symb := Shift_Left (symb, 1);
            offs := offs and not (match xor symb);
            exit when symb >= 16#10000#;
          end loop;
        end Simulate_Literal_Matched;
        --
      begin
        if sim.state < 7 then
          Simulate_Literal;
        else
          Simulate_Literal_Matched;
        end if;
        return prob_lit;
      end Test_Simple_Literal;

      function Test_Short_Rep_Match (sim : Machine_State) return MProb is
      begin
        return
          Test_Bit_Encoding (probs.switch.match (sim.state, sim.pos_state), DL_code_choice) *
          Test_Bit_Encoding (probs.switch.rep (sim.state), Rep_match_choice) *
          Test_Bit_Encoding (probs.switch.rep_g0 (sim.state), The_distance_is_rep0_choice) *
          Test_Bit_Encoding (probs.switch.rep0_long (sim.state, sim.pos_state), The_length_is_1_choice);
      end Test_Short_Rep_Match;

      --  We simulate here LZ77_emits_literal_byte.
      procedure Simulate_Literal_Byte (b : Byte; sim : in out Machine_State; prob : in out MProb) is
        probs_lit_idx : constant Integer := Idx_for_Literal_prob (sim.total_pos, sim.prev_byte);
        ltr, srm : MProb;
        procedure Update_pos_related_stuff is
        begin
          sim.R := (sim.R + 1) and Text_Buf_Mask;
          sim.total_pos := sim.total_pos + 1;
          sim.pos_state := Pos_state_range (UInt32 (sim.total_pos) and pos_bits_mask);
          sim.prev_byte := b;
        end Update_pos_related_stuff;
        b_match : constant Byte := Text_Buf ((sim.R - sim.rep_dist (0) - 1) and Text_Buf_Mask);
      begin
        sim.pos_state := Pos_state_range (UInt32 (sim.total_pos) and pos_bits_mask);
        ltr := Test_Simple_Literal (b, b_match, probs.lit (probs_lit_idx .. probs.lit'Last), sim);
        if b = b_match and then sim.total_pos > Data_Bytes_Count (sim.rep_dist (0) + 1) then
          srm := Test_Short_Rep_Match (sim);
          if srm > ltr then
            --  Short Rep would be preferred.
            sim.state := Update_State_ShortRep (sim.state);
            prob := prob * srm;
            Update_pos_related_stuff;
            return;
          end if;
        end if;
        sim.state := Update_State_Literal (sim.state);
        prob := prob * ltr;
        Update_pos_related_stuff;
      end Simulate_Literal_Byte;

      function Test_Literal_Byte (b : Byte; sim : Machine_State) return MProb is
        --  The following variable is discarded after the simulation,
        --  since we only test the literal generation for getting its probability.
        sim_var : Machine_State := sim;
        prob : MProb := 1.0;
      begin
        Simulate_Literal_Byte (b, sim_var, prob);
        return prob;
      end Test_Literal_Byte;

      function Simulate_Bit_Tree (prob : CProb_array; num_bits : Positive; symbol : Unsigned) return MProb is
        res : MProb := 1.0;
        bit, m : Unsigned;
      begin
        m := 1;
        for i in reverse 0 .. num_bits - 1 loop
          bit := Unsigned (Shift_Right (UInt32 (symbol), i)) and 1;
          res := res * Test_Bit_Encoding (prob (Integer (m) + prob'First), bit);
          m := 2 * m + bit;
        end loop;
        return res;
      end Simulate_Bit_Tree;

      function Test_Length (
        probs_len     : Probs_for_LZ_Lengths;
        length        : Unsigned;
        sim_pos_state : Pos_state_range
      )
      return MProb
      is
        len : Unsigned := length - Min_match_length;
        res : MProb;
      begin
        if len < Len_low_symbols then
          res := Test_Bit_Encoding (probs_len.choice_1, 0) *
                 Simulate_Bit_Tree (probs_len.low_coder (sim_pos_state), Len_low_bits, len);
        else
          res := Test_Bit_Encoding (probs_len.choice_1, 1);
          len := len - Len_low_symbols;
          if len < Len_mid_symbols then
            res := res * Test_Bit_Encoding (probs_len.choice_2, 0)
                       * Simulate_Bit_Tree (probs_len.mid_coder (sim_pos_state), Len_mid_bits, len);
          else
            res := res * Test_Bit_Encoding (probs_len.choice_2, 1);
            len := len - Len_mid_symbols;
            res := res * Simulate_Bit_Tree (probs_len.high_coder, Len_high_bits, len);
          end if;
        end if;
        return res;
      end Test_Length;

      function Test_Repeat_Match (
        index_rm : Repeat_stack_range;
        length   : Unsigned;
        sim      : Machine_State
      )
      return MProb
      is
        res : MProb := Test_Bit_Encoding (probs.switch.rep (sim.state), Rep_match_choice);
      begin
        case index_rm is
          when 0 =>
            res := res * Test_Bit_Encoding (probs.switch.rep_g0 (sim.state), The_distance_is_rep0_choice)
                       * Test_Bit_Encoding (probs.switch.rep0_long (sim.state, sim.pos_state), The_length_is_not_1_choice);
          when 1 =>
            res := res * Test_Bit_Encoding (probs.switch.rep_g0 (sim.state), The_distance_is_not_rep0_choice)
                       * Test_Bit_Encoding (probs.switch.rep_g1 (sim.state), The_distance_is_rep1_choice);
          when 2 =>
            res := res * Test_Bit_Encoding (probs.switch.rep_g0 (sim.state), The_distance_is_not_rep0_choice)
                       * Test_Bit_Encoding (probs.switch.rep_g1 (sim.state), The_distance_is_not_rep1_choice)
                       * Test_Bit_Encoding (probs.switch.rep_g2 (sim.state), The_distance_is_rep2_choice);
          when 3 =>
            res := res * Test_Bit_Encoding (probs.switch.rep_g0 (sim.state), The_distance_is_not_rep0_choice)
                       * Test_Bit_Encoding (probs.switch.rep_g1 (sim.state), The_distance_is_not_rep1_choice)
                       * Test_Bit_Encoding (probs.switch.rep_g2 (sim.state), The_distance_is_not_rep2_choice);
        end case;
        return res * Test_Length (probs.rep_len, length, sim.pos_state);
      end Test_Repeat_Match;

      function Test_Simple_Match (
        distance      : UInt32;
        length        : Unsigned;
        sim           : Machine_State
      )
      return MProb
      is
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
            res := res * Test_Bit_Encoding (prob (Integer (m) + prob'First), bit);
            m := 2 * m + bit;
            symb := Shift_Right (symb, 1);
          end loop;
          return res;
        end Simulate_Bit_Tree_Reverse;
        --
        function Test_Distance return MProb is
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
        end Test_Distance;
      begin
        return
          Test_Bit_Encoding (probs.switch.rep (sim.state), Simple_match_choice) *
          Test_Length (probs.len, length, sim.pos_state) *
          Test_Distance;
      end Test_Simple_Match;

      --  We simulate here a Distance-Length code
      --  sent straight to the encoder (no variants).
      procedure Simulate_Strict_DL_Code (
        distance      :        UInt32;
        length        :        Match_length_range;
        sim           : in out Machine_State;
        prob          : in out MProb
      )
      is
      pragma Inline (Simulate_Strict_DL_Code);
        dist_ip : constant UInt32 := UInt32 (distance - 1);  --  7-Zip distance convention (minus 1)
        found_repeat : Integer := Repeat_Stack'First - 1;
        dlc : constant MProb := Test_Bit_Encoding (probs.switch.match (sim.state, sim.pos_state), DL_code_choice);
        sma : constant MProb := Test_Simple_Match (dist_ip, Unsigned (length), sim);
        rma : MProb;
        aux : UInt32;
        procedure Update_pos_related_stuff is
        begin
          sim.total_pos := sim.total_pos + Data_Bytes_Count (length);
          sim.pos_state := Pos_state_range (UInt32 (sim.total_pos) and pos_bits_mask);
          sim.R := (sim.R + UInt32 (length)) and Text_Buf_Mask;  --  This is mod String_buffer_size
          sim.prev_byte := Text_Buf ((sim.R - 1) and Text_Buf_Mask);
        end Update_pos_related_stuff;
      begin
        for i in Repeat_Stack'Range loop
          if dist_ip = sim.rep_dist (i) then
            found_repeat := i;
            exit;
            --  NB: it's possible to pick the most probable duplicate instead, but without clear gain
          end if;
        end loop;
        if found_repeat >= Repeat_Stack'First then
          rma := Test_Repeat_Match (found_repeat, Unsigned (length), sim);
          if rma >= sma * Malus_simple_match_vs_rep  then
            --  Repeat match case:
            prob := prob * dlc * rma;
            --  Roll the stack of recent distances up to the found item, which becomes the first one.
            aux := sim.rep_dist (found_repeat);
            for i in reverse 1 .. found_repeat loop
              sim.rep_dist (i) := sim.rep_dist (i - 1);
            end loop;
            sim.rep_dist (0) := aux;
            sim.state := Update_State_Rep (sim.state);
            Update_pos_related_stuff;
            return;
          end if;
        end if;
        --  Simple match case:
        prob := prob * dlc * sma;
        --  Shift the stack of recent distances; the new distance becomes the first item.
        for i in reverse 1 .. Repeat_stack_range'Last loop
          sim.rep_dist (i) := sim.rep_dist (i - 1);
        end loop;
        sim.rep_dist (0) := dist_ip;  --  0-based distance.
        sim.state := Update_State_Match (sim.state);
        Update_pos_related_stuff;
      end Simulate_Strict_DL_Code;

      function Test_Strict_DL_Code (
        distance      : UInt32;
        length        : Match_length_range;
        sim           : Machine_State
      )
      return MProb
      is
      pragma Inline (Test_Strict_DL_Code);
        --  The following variable is discarded after the simulation,
        --  since we only test strict DL code for getting its probability.
        sim_var : Machine_State := sim;
        --
        prob : MProb := 1.0;
      begin
        Simulate_Strict_DL_Code (distance, length, sim_var, prob);
        return prob;
      end Test_Strict_DL_Code;

      --  Expand fully a DL code as a string of literals.
      procedure Simulate_Expand_DL_code (
        distance      :        UInt32;
        length        :        Match_length_range;
        give_up       :        MProb;
        sim           : in out Machine_State;
        prob          : in out MProb
      )
      is
      pragma Inline (Simulate_Expand_DL_code);
        b : Byte;
        --
        sim_mem : constant Machine_State := sim;
        expanded_string_prob : MProb := 1.0;
        Copy_start : constant UInt32 := (sim.R - distance) and Text_Buf_Mask;
      begin
        for x in 1 .. length loop
          b := Text_Buf ((Copy_start + UInt32 (x - 1)) and Text_Buf_Mask);
          Simulate_Literal_Byte (b, sim, expanded_string_prob);
          --  Probability is decreasing over the loop, so it is
          --  useless to continue under given threshold.
          if expanded_string_prob < give_up then
            sim := sim_mem;
            exit;
          end if;
          sim.prev_byte := b;
        end loop;
        prob := prob * expanded_string_prob;
      end Simulate_Expand_DL_code;

      function Test_Expanded_DL_Code (
        distance      : UInt32;
        length        : Match_length_range;
        give_up       : MProb;
        sim           : Machine_State
      )
      return MProb
      is
        pragma Inline (Test_Expanded_DL_Code);
        --  The following variable is discarded after the simulation,
        --  since we only test the DL code expansion for getting its probability.
        sim_var : Machine_State := sim;
        --
        prob : MProb := 1.0;
      begin
        Simulate_Expand_DL_code (distance, length, give_up, sim_var, prob);
        return prob;
      end Test_Expanded_DL_Code;

      --  Case of a DL code split into two shorter DL codes.
      procedure Test_Split_DL (
        distance        :     UInt32;
        length          :     Match_length_range;
        sim             :     Machine_State;
        hurdle          :     MProb;
        recursion_limit :     Natural;
        best_prob       : out MProb;
        best_cut        : out Match_length_range
      );
      pragma Inline (Test_Split_DL);

      procedure Generic_any_DL_Code (
        distance        :        UInt32;
        length          :        Match_length_range;
        sim             : in out Machine_State;
        prob            : in out MProb;
        recursion_limit :        Natural
      )
      is
        Copy_start : constant UInt32 := (sim.R - distance) and Text_Buf_Mask;
        strict_dlc, expanded_dlc, strict_or_expanded_dlc, dlc_after_lit, head_lit : MProb;
        b_head : Byte;
        sim_post_lit_pos_state : Pos_state_range;
        best_prob : MProb;
        best_cut  : Match_length_range;
        new_recursion_limit : Integer;
      begin
        if I_am_a_simulation then
          new_recursion_limit := recursion_limit - 1;
        else
          new_recursion_limit := recursion_limit;  --  We do not limit in actual emission.
        end if;
        if new_recursion_limit < 0 then
          Simulated_or_actual_Strict_DL_Code (distance, length, sim, prob);
          return;
        end if;
        if compare_variants >= Simple then
          strict_dlc             := Test_Strict_DL_Code (distance, length, sim);
          expanded_dlc           := Test_Expanded_DL_Code (distance, length, strict_dlc, sim);
          strict_or_expanded_dlc := MProb'Max (strict_dlc, expanded_dlc);
          --
          if length > Min_match_length then
            b_head   := Text_Buf (Copy_start and Text_Buf_Mask);
            head_lit := Test_Literal_Byte (b_head, sim);
            --  One literal, then a shorter DL code, case #1:
            --  naive approach: we spot a super-probable literal.
            if head_lit >= DL_Code_Erosion.Lit_then_DL_threshold then
              Simulated_or_actual_Literal_Byte (b_head, sim, prob);
              Generic_any_DL_Code (distance, length - 1, sim, prob, new_recursion_limit);
              return;
            end if;
            --  One literal, then a shorter DL code, case #2:
            --  we estimate the shorter DL code's probability.
            sim_post_lit_pos_state := Pos_state_range (UInt32 (sim.total_pos + 1) and pos_bits_mask);
            dlc_after_lit :=
              Test_any_DL_Code (
                distance, length - 1,
                (Update_State_Literal (sim.state), sim_post_lit_pos_state, b_head,
                 (sim.R + 1) and Text_Buf_Mask, sim.total_pos + 1, sim.rep_dist),
                new_recursion_limit
            );
            if head_lit * dlc_after_lit * DL_Code_Erosion.Malus_lit_then_DL (distance, length)
              > strict_or_expanded_dlc
            then
              Simulated_or_actual_Literal_Byte (b_head, sim, prob);
              Generic_any_DL_Code (distance, length - 1, sim, prob, new_recursion_limit);
              return;
            end if;
            if DL_Code_Erosion.DL_code_then_Literal (distance, length, sim, new_recursion_limit)
              > strict_or_expanded_dlc
            then
              --  We've got a better probability -> redo this variant
              --  (shorter DL code, then literal) for good.
              Generic_any_DL_Code (distance, length - 1, sim, prob, new_recursion_limit);
              Simulated_or_actual_Literal_Byte (Text_Buf ((sim.R - distance) and Text_Buf_Mask), sim, prob);
              return;
            end if;
          end if;
          --
          if expanded_dlc > strict_dlc then
            --  Here we prefer a full expansion of DL code as literals.
            for x in 1 .. length loop
              Simulated_or_actual_Literal_Byte (
                Text_Buf ((Copy_start + UInt32 (x - 1)) and Text_Buf_Mask), sim, prob
              );
            end loop;
            return;
          end if;
        end if;
        if compare_variants >= Splitting then
          Test_Split_DL (
            distance, length,
            sim, strict_or_expanded_dlc, new_recursion_limit,
            best_prob, best_cut
          );
          if best_prob > strict_or_expanded_dlc then
            Generic_any_DL_Code (distance, best_cut,          sim, prob, new_recursion_limit);
            Generic_any_DL_Code (distance, length - best_cut, sim, prob, new_recursion_limit);
            return;
          end if;
        end if;
        --  At this point, we go for simulating or writing the plain DL code.
        Simulated_or_actual_Strict_DL_Code (distance, length, sim, prob);
      end Generic_any_DL_Code;

      --  We simulate here Write_any_DL_code, including the variants!
      procedure Simulate_any_DL_Code_Instance is new Generic_any_DL_Code
        (Simulated_or_actual_Literal_Byte   => Simulate_Literal_Byte,
         Simulated_or_actual_Strict_DL_Code => Simulate_Strict_DL_Code,
         I_am_a_simulation                  => True);

      procedure Simulate_any_DL_Code (
        distance        :        UInt32;
        length          :        Match_length_range;
        sim             : in out Machine_State;
        prob            : in out MProb;
        recursion_limit :        Natural
      )
      renames Simulate_any_DL_Code_Instance;

      function Test_any_DL_Code (
        distance        : UInt32;
        length          : Match_length_range;
        sim             : Machine_State;
        recursion_limit : Natural
      )
      return MProb
      is
        --  The following variable is discarded after the simulation,
        --  since we only test the DL code for getting its probability.
        sim_var : Machine_State := sim;
        --
        prob : MProb := 1.0;
      begin
        Simulate_any_DL_Code (distance, length, sim_var, prob, recursion_limit);
        return prob;
      end Test_any_DL_Code;

      package body DL_Code_Erosion is
        --
        function DL_code_then_Literal (
          distance        : UInt32;
          length          : Match_length_range;
          sim             : Machine_State;
          recursion_limit : Natural
        )
        return MProb
        is
          --  The following variable is discarded after the simulation,
          --  since we only test this variant for getting its probability.
          sim_var : Machine_State := sim;
          --  This "DL erosion" technique empirically works better for shorter distances and lengths.
          Malus_DL_then_lit : constant MProb :=
            MProb'Max (0.0, 0.135 - MProb'Base (distance) * 1.0e-8 - MProb'Base (length) * 1.0e-4);
          --
          prob : MProb := Malus_DL_then_lit;
        begin
          Simulate_any_DL_Code (distance, length - 1, sim_var, prob, recursion_limit);
          Simulate_Literal_Byte (Text_Buf ((sim_var.R - distance) and Text_Buf_Mask), sim_var, prob);
          return prob;
        end DL_code_then_Literal;
        --
        function Malus_lit_then_DL (distance : UInt32; length : Match_length_range) return MProb is
        begin
          --  This "DL erosion" technique empirically works better for shorter distances and lengths.
          return MProb'Max (0.0, 0.064 - MProb'Base (distance) * 1.0e-9 - MProb'Base (length) * 3.0e-5);
        end Malus_lit_then_DL;
        --
      end DL_Code_Erosion;

      subtype Splits_considered is Match_length_range range 4 .. 9;

      procedure Test_Split_DL (
        distance        :     UInt32;
        length          :     Match_length_range;
        sim             :     Machine_State;
        hurdle          :     MProb;
        recursion_limit :     Natural;
        best_prob       : out MProb;
        best_cut        : out Match_length_range
      )
      is
        sim_var : Machine_State := sim;
        --  For long distances, the DL split technique degrades compression and makes
        --  the compression time explode.
        Malus : constant MProb :=
          MProb'Max (0.0, 0.27 - MProb'Base (distance) * 2.0e-6);
        prob : MProb;
        lowered_recursion_limit : constant Natural := Integer'Max (0, recursion_limit - 1);
      begin
        best_prob := 0.0;
        best_cut  := Match_length_range'First;
        if Malus < hurdle then
          return;
        end if;
        for cut in 2 .. length - 2 loop  --  If length < 4 this loop is skipped.
          if cut in Splits_considered or else length - cut in Splits_considered then
            --  If we test all lengths the compression becomes too slow
            --  (huge number of combinations since recursion is involved).
            prob := Malus;
            sim_var := sim;  --  Set or reset simulation state.
            Simulate_any_DL_Code (distance, cut, sim_var, prob, lowered_recursion_limit);
            if prob <= hurdle then
              null;
              --  Give up this iteration, since the probability is already below the required
              --  level -> would be even lower after simulating the second DL code.
            else
              Simulate_any_DL_Code (distance, length - cut, sim_var, prob, lowered_recursion_limit);
              if prob > best_prob then
                best_prob := prob;
                best_cut := cut;
              end if;
            end if;
          end if;
        end loop;
      end Test_Split_DL;

    end Estimates;

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
    encoded_uncompressed_bytes : UInt64 := 0;

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
          Write_Byte (temp + lb_bits_33_40);  --  Finally a byte is output sometimes!
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

    -----------------------------------------------------------------------------------
    --  This part processes the case where LZ77 sends a literal (a plain text byte)  --
    -----------------------------------------------------------------------------------

    procedure Write_Literal (prob : in out CProb_array; symbol : in UInt32) is
    pragma Inline (Write_Literal);
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
    pragma Inline (Write_Literal_Matched);
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

    use type Estimates.MProb;

    --  Encoder State: state of the real LZMA encoder - data is written here, no simulation!
    ES : Machine_State :=
      (R          => 0,
       prev_byte  => 0,
       total_pos  => 0,
       rep_dist   => (others => 0),
       state      => 0,
       pos_state  => 0
      );

    max_recursion : constant := 2;

    procedure Update_pos_state is
    pragma Inline (Update_pos_state);
    begin
      ES.pos_state := Pos_state_range (UInt32 (ES.total_pos) and pos_bits_mask);
    end Update_pos_state;

    procedure LZ77_emits_literal_byte (b : Byte) is
      pb_lit_idx : constant Integer := Idx_for_Literal_prob (ES.total_pos, ES.prev_byte);
      b_match : constant Byte := Text_Buf ((ES.R - ES.rep_dist (0) - 1) and Text_Buf_Mask);
    begin
      if b = b_match and then ES.total_pos > Data_Bytes_Count (ES.rep_dist (0) + 1)
        and then
          (compare_variants = None
             or else
           Estimates.Test_Short_Rep_Match (ES) >
           Estimates.Test_Simple_Literal (b, b_match, probs.lit (pb_lit_idx .. probs.lit'Last), ES))
      then
        --  We are lucky: both bytes are the same. No literal to encode, "Short Rep Match"
        --  case, and its cost (4 bits) is more affordable than the literal's cost.
        Encode_Bit (probs.switch.match (ES.state, ES.pos_state), DL_code_choice);
        Encode_Bit (probs.switch.rep (ES.state), Rep_match_choice);
        Encode_Bit (probs.switch.rep_g0 (ES.state), The_distance_is_rep0_choice);
        Encode_Bit (probs.switch.rep0_long (ES.state, ES.pos_state), The_length_is_1_choice);
        ES.state := Update_State_ShortRep (ES.state);
      else
        Encode_Bit (probs.switch.match (ES.state, ES.pos_state), Literal_choice);
        if ES.state < 7 then
          Write_Literal (probs.lit (pb_lit_idx .. probs.lit'Last), UInt32 (b));
        else
          Write_Literal_Matched (probs.lit (pb_lit_idx .. probs.lit'Last), UInt32 (b), UInt32 (b_match));
        end if;
        ES.state := Update_State_Literal (ES.state);
      end if;
      ES.total_pos := ES.total_pos + 1;
      Update_pos_state;
      ES.prev_byte := b;
      Text_Buf (ES.R) := b;
      ES.R := (ES.R + 1) and Text_Buf_Mask;  --  This is mod String_buffer_size
      encoded_uncompressed_bytes := encoded_uncompressed_bytes + 1;
    end LZ77_emits_literal_byte;

    procedure Write_Literal_Byte (
      b          :        Byte;
      dummy_sim  : in out Machine_State;
      dummy_prob : in out Estimates.MProb)
    is
    begin
      LZ77_emits_literal_byte (b);
    end Write_Literal_Byte;

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
        m := 2 * m + bit;
      end loop;
    end Bit_Tree_Encode;

    procedure Encode_Length (probs_len : in out Probs_for_LZ_Lengths; length : Unsigned) is
      len : Unsigned := length - Min_match_length;
    begin
      if len < Len_low_symbols then
        Encode_Bit (probs_len.choice_1, 0);
        --  LZ length in [2..9], i.e. len in [0..7]
        Bit_Tree_Encode (probs_len.low_coder (ES.pos_state), Len_low_bits, len);
      else
        Encode_Bit (probs_len.choice_1, 1);
        len := len - Len_low_symbols;
        if len < Len_mid_symbols then
          Encode_Bit (probs_len.choice_2, 0);
          --  LZ length in [10..17], i.e. len in [0..7]
          Bit_Tree_Encode (probs_len.mid_coder (ES.pos_state), Len_mid_bits, len);
        else
          Encode_Bit (probs_len.choice_2, 1);
          len := len - Len_mid_symbols;
          --  LZ length in [18..273], i.e. len in [0..255]
          Bit_Tree_Encode (probs_len.high_coder, Len_high_bits, len);
        end if;
      end if;
    end Encode_Length;

    procedure Write_Simple_Match (dist_ip : UInt32; length : Unsigned) is
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
          m := 2 * m + bit;
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
        dist_slot : constant Unsigned := Get_dist_slot (dist_ip);
        base, dist_reduced : UInt32;
        footerBits : Natural;
      begin
        Bit_Tree_Encode (probs.dist.slot_coder (len_state), Dist_slot_bits, dist_slot);
        if dist_slot >= Start_dist_model_index then
          footerBits := Natural (Shift_Right (UInt32 (dist_slot), 1)) - 1;
          base := Shift_Left (UInt32 (2 or (dist_slot and 1)), footerBits);
          dist_reduced := dist_ip - base;
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
      Encode_Bit (probs.switch.rep (ES.state), Simple_match_choice);
      ES.state := Update_State_Match (ES.state);
      Encode_Length (probs.len, length);
      Encode_Distance;
      --  Shift the stack of recent distances; the new distance becomes the first item.
      for i in reverse 1 .. Repeat_stack_range'Last loop
        ES.rep_dist (i) := ES.rep_dist (i - 1);
      end loop;
      ES.rep_dist (0) := dist_ip;
    end Write_Simple_Match;

    procedure Write_Repeat_Match (index_rm : Repeat_stack_range; length : Unsigned) is
      aux : UInt32;
    begin
      Encode_Bit (probs.switch.rep (ES.state), Rep_match_choice);
      case index_rm is
        when 0 =>
          Encode_Bit (probs.switch.rep_g0 (ES.state), The_distance_is_rep0_choice);
          Encode_Bit (probs.switch.rep0_long (ES.state, ES.pos_state), The_length_is_not_1_choice);
        when 1 =>
          Encode_Bit (probs.switch.rep_g0 (ES.state), The_distance_is_not_rep0_choice);
          Encode_Bit (probs.switch.rep_g1 (ES.state), The_distance_is_rep1_choice);
        when 2 =>
          Encode_Bit (probs.switch.rep_g0 (ES.state), The_distance_is_not_rep0_choice);
          Encode_Bit (probs.switch.rep_g1 (ES.state), The_distance_is_not_rep1_choice);
          Encode_Bit (probs.switch.rep_g2 (ES.state), The_distance_is_rep2_choice);
        when 3 =>
          Encode_Bit (probs.switch.rep_g0 (ES.state), The_distance_is_not_rep0_choice);
          Encode_Bit (probs.switch.rep_g1 (ES.state), The_distance_is_not_rep1_choice);
          Encode_Bit (probs.switch.rep_g2 (ES.state), The_distance_is_not_rep2_choice);
      end case;
      --  Roll the stack of recent distances up to the found item, which becomes the first one.
      aux := ES.rep_dist (index_rm);
      for i in reverse 1 .. index_rm loop
        ES.rep_dist (i) := ES.rep_dist (i - 1);
      end loop;
      ES.rep_dist (0) := aux;
      --
      Encode_Length (probs.rep_len, length);
      ES.state := Update_State_Rep (ES.state);
    end Write_Repeat_Match;

    procedure Write_Strict_DL_Code (
      distance   :        UInt32;
      length     :        Match_length_range;
      dummy_sim  : in out Machine_State;
      dummy_prob : in out Estimates.MProb
    )
    is
      dist_ip : constant UInt32 := UInt32 (distance - 1);  --  7-Zip distance convention (minus 1)
      found_repeat : Integer := Repeat_Stack'First - 1;
    begin
      pragma Assert (
        UInt64 (distance) <= encoded_uncompressed_bytes,
        "distance goes before input stream's begin"
      );
      Encode_Bit (probs.switch.match (ES.state, ES.pos_state), DL_code_choice);
      for i in Repeat_Stack'Range loop
        if dist_ip = ES.rep_dist (i) then
          found_repeat := i;
          exit;
          --  NB: it's possible to pick the most probable duplicate instead, but without clear gain
        end if;
      end loop;
      if found_repeat >= Repeat_Stack'First
        and then
          (compare_variants = None
             or else
           Estimates.Test_Repeat_Match (found_repeat, Unsigned (length), ES)
           >=
           Estimates.Test_Simple_Match (dist_ip, Unsigned (length), ES) *
           Estimates.Malus_simple_match_vs_rep
          )
      then
        Write_Repeat_Match (found_repeat, Unsigned (length));
      else
        Write_Simple_Match (dist_ip, Unsigned (length));
      end if;
      ES.total_pos := ES.total_pos + Data_Bytes_Count (length);
      Update_pos_state;
      ES.R := ES.R + UInt32 (length) and Text_Buf_Mask;  --  This is mod String_buffer_size
      ES.prev_byte := Text_Buf ((ES.R - 1) and Text_Buf_Mask);
    end Write_Strict_DL_Code;

    --  All the smart things to optimize the probability model by breaking
    --  DL codes is done in the following procedure:
    --
    procedure Write_any_DL_code is new Estimates.Generic_any_DL_Code
      (Simulated_or_actual_Literal_Byte   => Write_Literal_Byte,
       Simulated_or_actual_Strict_DL_Code => Write_Strict_DL_Code,
       I_am_a_simulation                  => False);

    procedure Expand_DL_Code_to_Buffer (
      sim      : Machine_State;
      distance : Integer;
      length   : Match_length_range
    )
    is
      Rx : UInt32 := sim.R;
      Copy_start : constant UInt32 := (sim.R - UInt32 (distance)) and Text_Buf_Mask;
    begin
      --  Expand early into the circular "text" buffer to have it up to date
      --  and available to simulations.
      for K in 0 .. UInt32 (length - 1) loop
        Text_Buf (Rx) := Text_Buf ((Copy_start + K) and Text_Buf_Mask);
        Rx := (Rx + 1) and Text_Buf_Mask;  --  This is mod String_buffer_size
      end loop;
    end Expand_DL_Code_to_Buffer;

    procedure LZ77_emits_DL_code (distance : Integer; length : Match_length_range) is
      dummy_prob : Estimates.MProb := 0.0;
    begin
      Expand_DL_Code_to_Buffer (ES, distance, length);
      encoded_uncompressed_bytes := encoded_uncompressed_bytes + UInt64 (length);
      Write_any_DL_code (UInt32 (distance), length, ES, dummy_prob, max_recursion);
    end LZ77_emits_DL_code;

    procedure Estimate_DL_Codes_for_LZ77 (
      matches          : in out LZ77.Matches_Array;
      old_match_index  : in     Natural;
      prefixes         : in     LZ77.Byte_Array;
      best_score_index :    out Positive;
      best_score_set   :    out LZ77.Prefetch_Index_Type;
      match_trace      :    out LZ77.DLP_Array
    )
    is
    pragma Unreferenced (match_trace);
      use LZ77, Estimates;
      last_pos_any_DL : Natural := 0;
      sim_new : Machine_State := ES;
      offset_new_match_set : constant array (Boolean) of Natural := (0, 1);
      head_lit_prob : MProb;
      --
      --  Compare different ways of sending DL codes starting with
      --  DL (i), with a total length 'last_pos_any_DL', in order to
      --  compare sequences of the same length starting with the different
      --  matches in DL_old and DL_new. The matches in DL_new are preceded
      --  by the literal 'head_literal_new'.
      --
      procedure Scoring (
        state           :     Machine_State;
        start           :     Positive;
        recursion_level :     Positive;
        prob            : out MProb;
        index           : out Positive;
        match_set       : out Prefetch_Index_Type
      )
      is
        --  We compute the probability of the message sent for the bytes
        --  at position 'start' to the position 'last_pos_any_DL' and find the
        --  optimal combination using the matches in the DL array.
        prob_i, tail_prob : MProb;
        test_state : Machine_State;
        length_trunc, some_index : Positive;
        some_match_set : Prefetch_Index_Type;
        last_pos_i : Integer;
      begin
        prob := 0.0;
        for m in matches'Range loop
          for i in 1 .. matches (m).count loop
            last_pos_i := matches (m).dl (i).length + offset_new_match_set (m /= old_match_index);
            if last_pos_i >= start then
              if last_pos_i < last_pos_any_DL and recursion_level >= 2 then
                --  Skip case requiring insane recursion level: level = number of DL codes chained!
                null;
              else
                if m /= old_match_index and then start = 1 then
                  test_state := sim_new;  --  Shortcut to avoid resimulating the head literal.
                  prob_i := head_lit_prob;
                else
                  test_state := state;  --  Clone the current state (general case including recursion).
                  prob_i := 1.0;
                end if;
                --
                --  'length_trunc' is what remains of the DL code, DL (i), to be consumed.
                --
                if m = old_match_index then
                  --  Easy case: we execute one of the "old" matches.
                  length_trunc := matches (m).dl (i).length   - start + 1; --  always >= 1
                elsif start = 1 then
                  --  We execute the full new DL code after the head literal.
                  length_trunc := matches (m).dl (i).length;
                else  --  start >= 2. (2: full DL, 3: truncate by 1, etc.)
                  length_trunc := matches (m).dl (i).length   - start + 2;  --  always >= 1
                end if;
                pragma Assert (length_trunc >= 1);
                --
                if length_trunc = 1 then
                  --  Just simulate the literal we are sitting on: the buffer
                  --  has been already filled via Expand_DL_Code_to_Buffer.
                  --  It is a shortcut for and should be equivalent to the position checked below.
                  pragma Assert (
                    Text_Buf (state.R) =
                    Text_Buf ((state.R - UInt32 (matches (m).dl (i).distance)) and Text_Buf_Mask),
                    "Bytes of simulated copy do not match; start =" & Integer'Image (start) &
                    "; DL code distance="  & Distance_Type'Image (matches (m).dl (i).distance) &
                    "; new match set="  & Boolean'Image (m /= old_match_index)
                  );
                  Simulate_Literal_Byte (
                    Text_Buf (state.R),
                    test_state,
                    prob_i);
                else  --  Here, length_trunc >= 2
                  Simulate_any_DL_Code (
                    distance        => UInt32 (matches (m).dl (i).distance),
                    length          => length_trunc,
                    sim             => test_state,
                    prob            => prob_i,
                    recursion_limit => 1);
                end if;
                if last_pos_i < last_pos_any_DL then
                  Scoring (test_state, last_pos_i + 1, recursion_level + 1, tail_prob, some_index, some_match_set);
                  prob_i := prob_i * tail_prob;
                end if;
                if prob_i > prob then
                  prob      := prob_i;
                  index     := i;
                  match_set := m;
                end if;
              end if;
            end if;
          end loop;
        end loop;
      end Scoring;
      --
      best_prob : MProb;
      new_wins : Boolean := False;
      last_pos_single_DL : Natural;
      match_for_max_last_pos : Distance_Length_Pair;
      sim_expand : Machine_State := ES;
      sim_old : constant Machine_State := ES;
    begin
      for m in matches'Range loop
        for i in 1 .. matches (m).count loop
          last_pos_single_DL := matches (m).dl (i).length + offset_new_match_set (m /= old_match_index);
          if last_pos_single_DL > last_pos_any_DL then
            last_pos_any_DL := last_pos_single_DL;
            match_for_max_last_pos := matches (m).dl (i);
            new_wins := m /= old_match_index;
          end if;
        end loop;
      end loop;
      if new_wins then  --  Copy the literal to the buffer.
        Text_Buf (sim_expand.R) := prefixes (1);
        sim_expand.R := (sim_expand.R + 1) and Text_Buf_Mask;
      end if;
      Expand_DL_Code_to_Buffer (sim_expand, match_for_max_last_pos.distance, match_for_max_last_pos.length);
      --
      head_lit_prob := 1.0;
      Simulate_Literal_Byte (prefixes (1), sim_new, head_lit_prob);
      --
      Scoring (sim_old, 1, 1, best_prob, best_score_index, best_score_set);
    end Estimate_DL_Codes_for_LZ77;

    procedure My_LZ77 is
      new LZ77.Encode
         (String_buffer_size => String_buffer_size (level),
          Look_Ahead         => Max_length (level),
          Threshold          => Min_length (level) - 1,
          Method             => LZ77_choice (level),
          Read_Byte          => Read_Byte,
          More_Bytes         => More_Bytes,
          Write_Literal      => LZ77_emits_literal_byte,
          Write_DL_Code      => LZ77_emits_DL_code,
          Estimate_DL_Codes  => Estimate_DL_Codes_for_LZ77
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
    case level is
      when Level_0 | Level_1 =>
        compare_variants := None;
      when Level_2  =>
        compare_variants := Simple;
      when Level_3 =>
        compare_variants := Splitting;
    end case;
    Write_LZMA_header;
    My_LZ77;
    if params.has_end_mark then
      --  The end-of-stream marker is a fake "Simple Match" with a special distance.
      Encode_Bit (probs.switch.match (ES.state, ES.pos_state), DL_code_choice);
      Write_Simple_Match (
        dist_ip => end_of_stream_magic_distance,
        length  => Min_match_length
      );
    end if;
    Flush_range_encoder;
    Dispose (Text_Buf);
  exception
    when others =>
      Dispose (Text_Buf);
      raise;
  end Encode;

end LZMA.Encoding;
