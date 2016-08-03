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

  --  Probability model for bit coding
  ------------------------------------

  Probability_model_bits  : constant:= 11;  --  LZMA specification name: "kNumBitModelTotalBits"
  Probability_model_count : constant:= 2 ** Probability_model_bits;

  Probability_change_bits : constant:= 5;   --  LZMA specification name: "kNumMoveBits"

  --  All probabilities are initialized with p=0.5. LZMA specification name: "PROB_INIT_VAL"
  Initial_probability : constant := Probability_model_count / 2;

  --  Type for storing probabilities, must be at least 11 bit.
  subtype CProb is UInt32;  --  LZMA specification recommends UInt16.
  type CProb_array is array(Unsigned range <>) of CProb;

  --  Finite state machine
  ------------------------

  States_count : constant := 12;  --  LZMA specification name: "kNumStates"
  subtype State_range is Unsigned range 0..States_count-1;
  type Transition is array(State_range) of State_range;

  ------------------------------------ From ...  0  1  2  3  4  5  6   7   8   9  10  11
  Update_State_Literal  : constant Transition:= (0, 0, 0, 0, 1, 2, 3,  4,  5,  6,  4,  5);
  Update_State_Match    : constant Transition:= (7, 7, 7, 7, 7, 7, 7, 10, 10, 10, 10, 10);
  Update_State_Rep      : constant Transition:= (8, 8, 8, 8, 8, 8, 8, 11, 11, 11, 11, 11);
  Update_State_ShortRep : constant Transition:= (9, 9, 9, 9, 9, 9, 9, 11, 11, 11, 11, 11);

end LZMA;
