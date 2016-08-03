--  Items that are common to LZMA encoding and LZMA decoding.

with Interfaces;
with System;

package LZMA is

  --  Nothing public so far...

private

  use Interfaces;

  --  These types are defined in the LZMA specification
  --  (DRAFT version, 2013-07-28, by Igor Pavlov)

  subtype Byte is Unsigned_8;
  subtype UInt16 is Unsigned_16;
  subtype UInt32 is Unsigned_32;
  type Unsigned is mod 2 ** System.Word_Size;

  subtype Literal_context_bits_range is Integer range 0..8;
  subtype Literal_position_bits_range is Integer range 0..4;
  subtype Position_bits_range is Integer range 0..4;

  ----------------------------
  --  Finite state machine  --
  ----------------------------

  States_count : constant := 12;  --  LZMA specification name: "kNumStates"
  subtype State_range is Unsigned range 0..States_count-1;
  type Transition is array(State_range) of State_range;

  ------------------------------------ From ...  0  1  2  3  4  5  6   7   8   9  10  11
  Update_State_Literal  : constant Transition:= (0, 0, 0, 0, 1, 2, 3,  4,  5,  6,  4,  5);
  Update_State_Match    : constant Transition:= (7, 7, 7, 7, 7, 7, 7, 10, 10, 10, 10, 10);
  Update_State_Rep      : constant Transition:= (8, 8, 8, 8, 8, 8, 8, 11, 11, 11, 11, 11);
  Update_State_ShortRep : constant Transition:= (9, 9, 9, 9, 9, 9, 9, 11, 11, 11, 11, 11);

  --  Context for improving compression of aligned data,
  --  modulo 2**n = 2, 4, 8 or 16 (max) bytes, or disabled: n = 0.
  Max_pos_bits : constant := 4;  --  LZMA specification name: "kNumPosBitsMax"
  Max_pos_states_count : constant := 2**Max_pos_bits;
  subtype Pos_state_range is Unsigned range 0 .. Max_pos_states_count-1;

  ----------------------------------------
  --  Probability model for bit coding  --
  ----------------------------------------

  Probability_model_bits  : constant:= 11;  --  LZMA specification name: "kNumBitModelTotalBits"
  Probability_model_count : constant:= 2 ** Probability_model_bits;

  Probability_change_bits : constant:= 5;   --  LZMA specification name: "kNumMoveBits"

  --  All probabilities are initialized with p=0.5. LZMA specification name: "PROB_INIT_VAL"
  Initial_probability : constant := Probability_model_count / 2;

  --  Type for storing probabilities, must be at least 11 bit.
  subtype CProb is UInt32;  --  LZMA specification recommends UInt16.
  type CProb_array is array(Unsigned range <>) of CProb;

  Align_bits       : constant := 4;  --  LZMA specification name: "kNumAlignBits"

  subtype Bits_3_range is Unsigned range 0 .. 2**3 - 1;
  subtype Bits_6_range is Unsigned range 0 .. 2**6 - 1;
  subtype Bits_8_range is Unsigned range 0 .. 2**8 - 1;
  subtype Bits_NAB_range is Unsigned range 0 .. 2**Align_bits - 1;

  subtype Probs_3_bits is CProb_array(Bits_3_range);
  subtype Probs_6_bits is CProb_array(Bits_6_range);
  subtype Probs_8_bits is CProb_array(Bits_8_range);
  subtype Probs_NAB_bits is CProb_array(Bits_NAB_range);

  type Low_mid_coder_probs is array(Pos_state_range) of Probs_3_bits;

  type Probs_for_LZ_Lengths is record
    choice_1   : CProb               := Initial_probability;  --  0: low coder; 1: mid or high
    choice_2   : CProb               := Initial_probability;  --  0: mid; 1: high
    low_coder  : Low_mid_coder_probs := (others => (others => Initial_probability));
    mid_coder  : Low_mid_coder_probs := (others => (others => Initial_probability));
    high_coder : Probs_8_bits        := (others => Initial_probability);
  end record;

end LZMA;
