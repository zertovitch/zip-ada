-- LZMA_Decoding - a generic LZMA decoder.
-- Based on a translation of LzmaSpec.cpp, the LZMA Reference Decoder, by Igor Pavlov.
-- Public domain.

with Ada.Direct_IO, Ada.Finalization, Interfaces;

generic
  -- Input:
  with function Read_Byte return Interfaces.Unsigned_8;
  -- Output:
  with procedure Write_Byte(b: Interfaces.Unsigned_8);

package LZMA_Decoding is

  type LZMA_Result is (
    LZMA_finished_with_marker,
    LZMA_finished_without_marker
  );

  ---------------------------------------------------------------------------------
  -- Usage 1 : parameterless procedure, if you care only about the decompression --
  ---------------------------------------------------------------------------------

  procedure Decompress;

  -------------------------------------------------------
  -- Usage 2 : object-oriented, with technical details --
  -------------------------------------------------------

  type CLzmaDecoder is limited private;
  procedure Decode(o: in out CLzmaDecoder; res: out LZMA_Result);

  -- The technical details:
  function Literal_context_bits(o: CLzmaDecoder) return Natural; 
  function Literal_pos_bits(o: CLzmaDecoder) return Natural; 
  function Pos_bits(o: CLzmaDecoder) return Natural; 

  package BIO is new Ada.Direct_IO(Interfaces.Unsigned_8); -- BIO is only there for the Count type
  subtype Data_Bytes_Count is BIO.Count;

  function Unpack_size_defined(o: CLzmaDecoder) return Boolean;
  function Unpack_size_as_defined(o: CLzmaDecoder) return  Data_Bytes_Count;
  function Dictionary_size(o: CLzmaDecoder) return Interfaces.Unsigned_32;
  function Dictionary_size_in_properties(o: CLzmaDecoder) return Interfaces.Unsigned_32;
  function Range_decoder_corrupted(o: CLzmaDecoder) return Boolean;

  ------------------------------------------------------------------------------------------
  -- Usage 3 : Decode in three steps: Decode_Header, Create_Large_Arrays, Decode_Contents --
  ------------------------------------------------------------------------------------------

  procedure Decode_Header(o: in out CLzmaDecoder);
  procedure Create_Large_Arrays(o: in out CLzmaDecoder);
  procedure Decode_Contents(o: in out CLzmaDecoder; res: out LZMA_Result);

private

  use Interfaces;

  subtype Byte is Unsigned_8;
  subtype UInt16 is Unsigned_16;
  subtype UInt32 is Unsigned_32;
  type Unsigned is mod 2 ** Standard'Address_Size;

    type Byte_buffer is array(UInt32 range <>) of Byte;
  type p_Byte_buffer is access Byte_buffer;

  type COutWindow is record
    Buf      : p_Byte_buffer:= null;
    Pos      : UInt32;
    Size     : UInt32;
    IsFull   : Boolean;
    TotalPos : Unsigned;
  end record;

  type CRangeDecoder is record
    RangeZ    : UInt32;
    Code      : UInt32;
    Corrupted : Boolean;
  end record;

  type CProb is new UInt16;
  type CProb_array is array(Unsigned range <>) of CProb;
  type p_CProb_array is access CProb_array;

  kNumPosBitsMax : constant := 4;
  kNumPosBitsMax_Count : constant := 2**kNumPosBitsMax;

  kNumStates          : constant := 12;
  kNumLenToPosStates  : constant := 4;
  kNumAlignBits       : constant := 4;
  kEndPosModelIndex   : constant := 14;
  kNumFullDistances   : constant := 2 ** (kEndPosModelIndex / 2);

  generic
    NumBits: Positive;
  package CBitTreeDecoder is
    subtype Probs is CProb_array(0 .. 2**NumBits - 1);
    procedure Init(p: out Probs);
    procedure Decode(p: in out Probs; rc: in out CRangeDecoder; res: out Unsigned);
    procedure ReverseDecode(p: in out Probs; rc: in out CRangeDecoder; res: out UInt32);
  end;

  subtype Probs_3_bits is CProb_array(0 .. 2**3 - 1);
  subtype Probs_6_bits is CProb_array(0 .. 2**6 - 1);
  subtype Probs_8_bits is CProb_array(0 .. 2**8 - 1);
  subtype Probs_NAB_bits is CProb_array(0 .. 2**kNumAlignBits - 1);
  
  type LM_Coder_Probs is array(Unsigned'(0) .. kNumPosBitsMax_Count - 1) of Probs_3_bits;

  type CLenDecoder is record
    Choice    : CProb;
    Choice2   : CProb;
    LowCoder  : LM_Coder_Probs;
    MidCoder  : LM_Coder_Probs;
    HighCoder : Probs_8_bits;
  end record;

  type Slot_Coder_Probs is array(Unsigned'(0) .. kNumLenToPosStates - 1) of Probs_6_bits;

  Last_PosDecoders: constant := kNumFullDistances - kEndPosModelIndex;

  subtype LC_range is Integer range 0..8;
  subtype LP_range is Integer range 0..4;
  subtype PB_range is Integer range 0..4;

  type CLzmaDecoder is new Ada.Finalization.Limited_Controlled with record
    RangeDec             : CRangeDecoder;
    OutWindow            : COutWindow;
    markerIsMandatory    : Boolean;
    lc                   : LC_range; -- the number of "literal context" bits
    lp                   : LP_range; -- the number of "literal pos" bits
    pb                   : PB_range; -- the number of "pos" bits
    literal_pos_mask     : UInt32;
    pos_bits_mask        : UInt32;
    dictSize             : UInt32;
    dictSizeInProperties : UInt32;
    LitProbs             : p_CProb_array;
    PosSlotDecoder       : Slot_Coder_Probs;
    AlignDecoder         : Probs_NAB_bits;
    PosDecoders          : CProb_array(0..Last_PosDecoders);
    IsMatch              : CProb_array(0..kNumStates * kNumPosBitsMax_Count - 1);
    IsRep0Long           : CProb_array(0..kNumStates * kNumPosBitsMax_Count - 1);
    IsRep                : CProb_array(0..kNumStates - 1);
    IsRepG0              : CProb_array(0..kNumStates - 1);
    IsRepG1              : CProb_array(0..kNumStates - 1);
    IsRepG2              : CProb_array(0..kNumStates - 1);
    LenDecoder           : CLenDecoder;
    RepLenDecoder        : CLenDecoder;
    unpackSize           : Data_Bytes_Count;
    unpackSize_as_defined: Data_Bytes_Count;
    unpackSizeDefined    : Boolean;
  end record;

  procedure Finalize(o: in out CLzmaDecoder);

  LZMA_Error: exception;
  
end LZMA_Decoding;
