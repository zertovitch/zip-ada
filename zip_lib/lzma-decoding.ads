--  LZMA.Decoding - a standalone, generic LZMA decoder.
-----------------
--
--  Based on a translation of LzmaSpec.cpp, the LZMA Reference Decoder, by Igor Pavlov.
--  Further rework documented in body.

--  Examples of use:
--    LZMA_Dec, a standalone decoder for .lzma files
--    UnZip.Decompress, extracts Zip files entries with, among others, LZMA encoding

--  Legal licensing note:

--  Copyright (c) 2014 .. 2019 Gautier de Montmollin (maintainer of the Ada version)
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

generic
  --  Input:
  with function Read_Byte return Byte;
  --  Output:
  with procedure Write_Byte (b : Byte);

package LZMA.Decoding is

  type LZMA_Result is (
    LZMA_finished_with_marker,
    LZMA_finished_without_marker
  );

  --  The passing of the uncompressed data size follows very
  --  confusing rules in the LZMA format. If you have a noiseless
  --  data transmission it is better to bypass any size check,
  --  by using a End-Of-Stream marker (marker_expected = True on the decoder
  --  side below: type LZMA_Hints) and *not* giving any hardcoded
  --  size (has_size = False), and keeping given_size = dummy_size (default).

  dummy_size : constant Data_Bytes_Count := Data_Bytes_Count'Last;

  --  The hints represent knowledge prior to decompressing a LZMA
  --  stream. Notably, the LZMA header with has_size = True is *not*
  --  compatible with the LZMA header with has_size = False.
  --
  type LZMA_Hints is record
    has_size               : Boolean;         --  Is size is part of header data ?
                                              --  In LZMA.Encoding.Encode: uncompressed_size_info.
    given_size             : Data_Bytes_Count --  If has_size = False, we use given_size.
                              := dummy_size;
    marker_expected        : Boolean;         --  Is an End-Of-Stream marker expected ?
    fail_on_bad_range_code : Boolean;         --  Raise exception if range decoder corrupted ?
    --  The LZMA specification is a bit ambiguous on this point: a decoder has to ignore
    --  corruption cases, but an encoder is required to avoid them...
  end record;

  ----------------------------------------------------------------------------------
  --  Usage #1 : Object-less procedure, if you care only about the decompression  --
  ----------------------------------------------------------------------------------

  procedure Decompress (hints : LZMA_Hints);
  --  The parameter hints.has_size must have the same value as
  --  uncompressed_size_info in LZMA.Encoding.Encode: there are unfortunately
  --  two incompatible LZMA header variants: one including uncompressed
  --  data size, and one without that size.
  --  Note that hints.has_size = uncompressed_size_info = True for .lzma files.

  ---------------------------------------------------------------------------
  --  Usage #2 : Object-oriented, with stored technical details as output  --
  ---------------------------------------------------------------------------

  type LZMA_Decoder_Info is limited private;
  procedure Decode (info : out LZMA_Decoder_Info; hints : LZMA_Hints; res : out LZMA_Result);

  --  The technical details:
  function Literal_context_bits (info : LZMA_Decoder_Info) return Natural;
  function Literal_pos_bits (info : LZMA_Decoder_Info) return Natural;
  function Pos_bits (info : LZMA_Decoder_Info) return Natural;

  function Unpack_size_defined (info : LZMA_Decoder_Info) return Boolean;
  function Unpack_size_as_defined (info : LZMA_Decoder_Info) return Data_Bytes_Count;
  --  Sizes in bytes:
  function Probability_model_size (info : LZMA_Decoder_Info) return Interfaces.Unsigned_32;
  function Dictionary_size (info : LZMA_Decoder_Info) return Interfaces.Unsigned_32;
  function Dictionary_size_in_properties (info : LZMA_Decoder_Info) return Interfaces.Unsigned_32;
  --
  function Range_decoder_corrupted (info : LZMA_Decoder_Info) return Boolean;

  LZMA_Error : exception;

private

  type LZMA_Decoder_Info is record
    unpackSize            : Data_Bytes_Count;
    unpackSize_as_defined : Data_Bytes_Count;
    unpackSizeDefined     : Boolean;
    markerIsMandatory     : Boolean;
    dictionary_size       : UInt32;
    dictSizeInProperties  : UInt32;
    lc                    : Literal_context_bits_range;   -- number of "literal context" bits
    lp                    : Literal_position_bits_range;  -- number of "literal pos" bits
    pb                    : Position_bits_range;          -- number of "pos" bits
    range_dec_corrupted   : Boolean;
  end record;

end LZMA.Decoding;
