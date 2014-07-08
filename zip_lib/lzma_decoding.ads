-- LZMA_Decoding - a generic LZMA decoder.
-- Based on a translation of LzmaSpec.cpp, the LZMA Reference Decoder, by Igor Pavlov.
-- Public domain.

with Ada.Direct_IO, Interfaces, System;

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

  package BIO is new Ada.Direct_IO(Interfaces.Unsigned_8); -- BIO is only there for the Count type
  subtype Data_Bytes_Count is BIO.Count;

  dummy_size: constant Data_Bytes_Count:= Data_Bytes_Count'Last;

  type LZMA_Hints is record
    has_size               : Boolean;           -- Is size is part of header data ?
    given_size             : Data_Bytes_Count;  -- If has_size = False, we use given_size.
    marker_expected        : Boolean;           -- Is an End-Of-Stream marker expected ?
    fail_on_bad_range_code : Boolean;           -- Raise exception if range decoder corrupted ?
    -- The LZMA specification is a bit ambiguous on this point: a decoder has to ignore
    -- corruption cases, but an encoder is required to avoid them...
  end record;

  -------------------------------------------------------------------------------
  -- Usage 1 : Object-less procedure, if you care only about the decompression --
  -------------------------------------------------------------------------------

  procedure Decompress(hints: LZMA_Hints);

  ------------------------------------------------------------------------
  -- Usage 2 : Object-oriented, with stored technical details as output --
  ------------------------------------------------------------------------

  type LZMA_Decoder_Info is limited private;
  procedure Decode(o: in out LZMA_Decoder_Info; hints: LZMA_Hints; res: out LZMA_Result);

  -- The technical details:
  function Literal_context_bits(o: LZMA_Decoder_Info) return Natural;
  function Literal_pos_bits(o: LZMA_Decoder_Info) return Natural;
  function Pos_bits(o: LZMA_Decoder_Info) return Natural;

  function Unpack_size_defined(o: LZMA_Decoder_Info) return Boolean;
  function Unpack_size_as_defined(o: LZMA_Decoder_Info) return  Data_Bytes_Count;
  function Dictionary_size(o: LZMA_Decoder_Info) return Interfaces.Unsigned_32;
  function Dictionary_size_in_properties(o: LZMA_Decoder_Info) return Interfaces.Unsigned_32;
  function Range_decoder_corrupted(o: LZMA_Decoder_Info) return Boolean;

  LZMA_Error: exception;

private

  use Interfaces;

  subtype Byte is Unsigned_8;
  subtype UInt16 is Unsigned_16;
  subtype UInt32 is Unsigned_32;
  type Unsigned is mod 2 ** System.Word_Size;

  subtype LC_range is Integer range 0..8;
  subtype LP_range is Integer range 0..4;
  subtype PB_range is Integer range 0..4;

  type LZMA_Decoder_Info is record
    unpackSize           : Data_Bytes_Count;
    unpackSize_as_defined: Data_Bytes_Count;
    unpackSizeDefined    : Boolean;
    markerIsMandatory    : Boolean;
    dictSize             : UInt32;
    dictSizeInProperties : UInt32;
    lc                   : LC_range; -- the number of "literal context" bits
    lp                   : LP_range; -- the number of "literal pos" bits
    pb                   : PB_range; -- the number of "pos" bits
    range_dec_corrupted  : Boolean;
  end record;

end LZMA_Decoding;
