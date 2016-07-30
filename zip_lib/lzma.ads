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
  Probability_change_bits : constant:= 5;   --  LZMA specification name: "kNumMoveBits"
  Probability_model_count : constant:= 2 ** Probability_model_bits;
  --  All proabilities are initialized with p=0.5. LZMA specification name: "PROB_INIT_VAL"
  Initial_probability : constant := Probability_model_count / 2;

end LZMA;
