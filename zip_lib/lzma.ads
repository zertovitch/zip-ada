--  LZMA library
----------------
--  Library for encoding and decoding data streams in the LZMA compression
--  format invented by Igor Pavlov.
--
--  Pure Ada 95+ code, 100% portable: OS-, CPU- and compiler- independent.

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

--  NB: this is the MIT License, as found 21-Aug-2016 on the site
--  http://www.opensource.org/licenses/mit-license.php

with Ada.Direct_IO;  --  Only used for the type Data_Bytes_Count below.
with Interfaces;
with System;

package LZMA is

  --  The compression and decompression procedures are located
  --  in child packages LZMA.Encoding and LZMA.Decoding respectively.

  --  Bits of last byte being used as context.
  --    With the value 8, LZMA uses a complete Markov chain for predicting
  --    a literal from the previous one, like PKZip's Reduce format.
  subtype Literal_context_bits_range  is Integer range 0 .. 8;

  --  Position mod 2**bits is used, but for literal context only.
  subtype Literal_position_bits_range is Integer range 0 .. 4;

  --  Position mod 2**bits is used in various places.
  subtype Position_bits_range         is Integer range 0 .. 4;

  Default_dictionary_size : constant := 2 ** 15;  --  32 KiB, like Deflate.

  subtype Byte is Interfaces.Unsigned_8;

  --  Ada.Direct_IO is only there for the Data_Bytes_Count type.
  --  In case you want to avoid reference to Ada.Direct_IO,
  --  you can customize the definition of Data_Bytes_Count, provided
  --  it has enough capacity for counting bytes in the streams involved.
  package BIO is new Ada.Direct_IO (Byte);
  subtype Data_Bytes_Count is BIO.Count;

private

  use Interfaces;

  --  These integer types are defined in the LZMA specification
  --  (DRAFT version, 2015-06-14, by Igor Pavlov)

  type Unsigned is mod 2 ** System.Word_Size;
  subtype UInt64 is Unsigned_64;
  subtype UInt32 is Unsigned_32;
  subtype UInt16 is Unsigned_16;

  ----------------------------
  --  Finite state machine  --
  ----------------------------

  States_count : constant := 12;  --  LZMA specification name: "kNumStates"
  subtype State_range is Unsigned range 0 .. States_count - 1;
  type Transition is array (State_range) of State_range;

  ------------------------------------ From ...  0  1  2  3  4  5  6   7   8   9  10  11
  Update_State_Literal  : constant Transition := (0, 0, 0, 0, 1, 2, 3,  4,  5,  6,  4,  5);
  Update_State_Match    : constant Transition := (7, 7, 7, 7, 7, 7, 7, 10, 10, 10, 10, 10);
  Update_State_Rep      : constant Transition := (8, 8, 8, 8, 8, 8, 8, 11, 11, 11, 11, 11);
  Update_State_ShortRep : constant Transition := (9, 9, 9, 9, 9, 9, 9, 11, 11, 11, 11, 11);

  --  Context for improving compression of aligned data,
  --  modulo 2**n = 2, 4, 8 or 16 (max) bytes, or disabled: n = 0.
  Max_pos_bits : constant := 4;  --  LZMA specification name: "kNumPosBitsMax"
  Max_pos_states_count : constant := 2**Max_pos_bits;
  subtype Pos_state_range is Unsigned range 0 .. Max_pos_states_count - 1;

  ----------------------------------------
  --  Probability model for bit coding  --
  ----------------------------------------

  Probability_model_bits  : constant := 11;  --  LZMA specification name: "kNumBitModelTotalBits"
  Probability_model_count : constant := 2 ** Probability_model_bits;

  Probability_change_bits : constant := 5;   --  LZMA specification name: "kNumMoveBits"

  --  All probabilities are initialized with p=0.5. LZMA specification name: "PROB_INIT_VAL"
  Initial_probability : constant := Probability_model_count / 2;

  --  Type for storing probabilities, must have at least Probability_model_bits bits.
  --  LZMA specification recommends UInt16. LzmaEnc.c uses UInt16 or optionally UInt32.
  type CProb is new UInt16;

  --  Integer (signed) used as index because there is a -1 (unused) index in Pos_coder_range.
  type CProb_array is array (Integer range <>) of CProb;

  Align_bits       : constant := 4;  --  LZMA specification name: "kNumAlignBits"
  Align_table_size : constant := 2 ** Align_bits;
  Align_mask       : constant := Align_table_size - 1;

  subtype Bits_3_range is Integer range 0 .. 2**3 - 1;
  subtype Bits_6_range is Integer range 0 .. 2**6 - 1;
  subtype Bits_8_range is Integer range 0 .. 2**8 - 1;
  subtype Bits_NAB_range is Integer range 0 .. 2**Align_bits - 1;

  subtype Probs_3_bits is CProb_array (Bits_3_range);
  subtype Probs_6_bits is CProb_array (Bits_6_range);
  subtype Probs_8_bits is CProb_array (Bits_8_range);
  subtype Probs_NAB_bits is CProb_array (Bits_NAB_range);

  --------------------------------------------------
  --  Probabilities for the binary decision tree  --
  --------------------------------------------------

  type Probs_state is array (State_range) of CProb;
  type Probs_state_and_pos_state is array (State_range, Pos_state_range) of CProb;

  type Probs_for_switches is record
    --  This is the context for the switch between a Literal and a LZ Distance-Length code
    match     : Probs_state_and_pos_state := (others => (others => Initial_probability));
    --  These are contexts for various repetition modes
    rep       : Probs_state := (others => Initial_probability);
    rep_g0    : Probs_state := (others => Initial_probability);
    rep_g1    : Probs_state := (others => Initial_probability);
    rep_g2    : Probs_state := (others => Initial_probability);
    rep0_long : Probs_state_and_pos_state := (others => (others => Initial_probability));
  end record;

  ------------------------------------
  --  Probabilities for LZ lengths  --
  ------------------------------------

  type Low_mid_coder_probs is array (Pos_state_range) of Probs_3_bits;

  --  Probabilities used for encoding LZ lengths. LZMA specification name: "CLenDecoder"
  type Probs_for_LZ_Lengths is record
    choice_1   : CProb               := Initial_probability;  --  0: low coder; 1: mid or high
    choice_2   : CProb               := Initial_probability;  --  0: mid; 1: high
    low_coder  : Low_mid_coder_probs := (others => (others => Initial_probability));
    mid_coder  : Low_mid_coder_probs := (others => (others => Initial_probability));
    high_coder : Probs_8_bits        := (others => Initial_probability);
  end record;

  --------------------------------------
  --  Probabilities for LZ distances  --
  --------------------------------------

  Len_to_pos_states  : constant := 4;
  subtype Slot_coder_range is Unsigned range 0 .. Len_to_pos_states - 1;
  type Slot_coder_probs is array (Slot_coder_range) of Probs_6_bits;
  Dist_slot_bits : constant := 6;  --  "kNumPosSlotBits"

  Start_dist_model_index : constant :=  4;  --  "kStartPosModelIndex"
  End_dist_model_index   : constant := 14;  --  LZMA specification name: "kEndPosModelIndex"
  Num_full_distances  : constant := 2 ** (End_dist_model_index / 2);  --  "kNumFullDistances"

  --  Pos_coder_range: index -1 is never used as such but appears
  --  when calling Bit_Tree_Reverse_Encode (as in the original C version, RcTree_ReverseEncode).
  subtype Pos_coder_range is Integer range -1 .. Num_full_distances - End_dist_model_index;
  subtype Pos_coder_probs is CProb_array (Pos_coder_range);

  type Probs_for_LZ_Distances is record
    slot_coder  : Slot_coder_probs := (others => (others => Initial_probability));
    align_coder : Probs_NAB_bits   := (others => Initial_probability);
    pos_coder   : Pos_coder_probs  := (others => Initial_probability);
  end record;

  --------------------------------------
  --  All probabilities used by LZMA  --
  --------------------------------------

  type All_probabilities (last_lit_prob_index : Integer) is record
    --  Literals:
    lit     : CProb_array (0 .. last_lit_prob_index) := (others => Initial_probability);
    --  Distances:
    dist    : Probs_for_LZ_Distances;
    --  Lengths:
    len     : Probs_for_LZ_Lengths;
    rep_len : Probs_for_LZ_Lengths;
    --  Decision tree switches:
    switch  : Probs_for_switches;
  end record;

  -------------
  --  Misc.  --
  -------------

  --  Minimum dictionary (= plain text buffer of n previous bytes)
  --  size is 4096. LZMA specification name: "LZMA_DIC_MIN"
  Min_dictionary_size : constant := 2 ** 12;

  --  Log2-style encoding of LZ lengths
  Len_low_bits     : constant := 3;
  Len_low_symbols  : constant := 2 ** Len_low_bits;
  Len_mid_bits     : constant := 3;
  Len_mid_symbols  : constant := 2 ** Len_mid_bits;
  Len_high_bits    : constant := 8;
  Len_high_symbols : constant := 2 ** Len_high_bits;
  Len_symbols      : constant := Len_low_symbols + Len_mid_symbols + Len_high_symbols;

  Min_match_length : constant := 2;  --  "LZMA_MATCH_LEN_MIN"
  Max_match_length : constant := Min_match_length + Len_symbols - 1;  --  "LZMA_MATCH_LEN_MAX"

  subtype Match_length_range is Integer range Min_match_length .. Max_match_length;

  --  Fake distance, used as an end-of-stream marker.
  end_of_stream_magic_distance : constant := 16#FFFF_FFFF#;

  --------------------------------------------------
  --  Binary values of various decision switches  --
  --------------------------------------------------

  --  LZ literal vs. DL code
  Literal_choice : constant := 0;
  DL_code_choice : constant := 1;

  --  Within DL code: "Simple match" vs. "Rep match"
  Simple_match_choice : constant := 0;
  Rep_match_choice    : constant := 1;

  --  Within "Rep match": "Distance is rep0" vs. "Distance is not rep0"
  The_distance_is_rep0_choice     : constant := 0;
  The_distance_is_not_rep0_choice : constant := 1;
  --  Within "Distance is rep0":
  The_length_is_1_choice     : constant := 0;
  The_length_is_not_1_choice : constant := 1;
  --  Within "Distance is not rep0": "Distance is rep1" vs. "Distance is not rep1"
  The_distance_is_rep1_choice     : constant := 0;
  The_distance_is_not_rep1_choice : constant := 1;
  --  Within "Distance is not rep1": "Distance is rep2" vs. "Distance is not rep2"
  The_distance_is_rep2_choice     : constant := 0;
  The_distance_is_not_rep2_choice : constant := 1;

  ----------------------
  --  Range encoding  --
  ----------------------

  --  Normalization threshold. When the range width is below that value,
  --  a shift is needed.
  width_threshold : constant := 2**24;  --  LZMA specification name: "kTopValue"

  --  The following article (the only reference in the LZMA specification)
  --  explains how range encoding works:
  --
  --     G. N. N. Martin, Range encoding: an algorithm for removing redundancy
  --     from a digitized message, Video & Data Recording Conference,
  --     Southampton, UK, July 24-27, 1979.

end LZMA;
