--  LZMA.Encoding - a standalone, generic LZMA encoder.
-----------------
--  See body for credits and other informations.
--
--  Examples of use:
--    LZMA_Enc, a standalone encoder to .lzma files
--    Zip.Compress.LZMA_E, creates Zip files entries with LZMA encoding

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

package LZMA.Encoding is

  --  Low level: faster but weaker compression
  --  High level: slower but stronger compression
  --
  type Compression_level is (
    Level_0,  --  no LZ compression
    Level_1,  --  uses Info-Zip's match finder for Deflate (32KB  sliding window), level 6
    Level_2,  --  uses Info-Zip's match finder for Deflate (32KB  sliding window), level 10
    Level_3   --  uses LZMA SDK's BT4 match finder, dictionary's size specified in dictionary_size
  );

  generic
    --  Input of data:
    with function  Read_Byte return Byte;
    with function  More_Bytes return Boolean;
    --  Output of LZMA-compressed data:
    with procedure Write_Byte (b : Byte);
    --
  procedure Encode (
    level                  : Compression_level           := Level_1;
    literal_context_bits   : Literal_context_bits_range  := 3;   --  Bits of last byte are used.
    literal_position_bits  : Literal_position_bits_range := 0;   --  Position mod 2**bits is used.
    position_bits          : Position_bits_range         := 2;   --  Position mod 2**bits is used.
    end_marker             : Boolean := True;   --  Produce an End-Of-Stream marker (*) ?
    uncompressed_size_info : Boolean := False;  --  Optional extra header needed for .lzma files.
                                                --  In LZMA.Decoding, type LZMA_Hints: has_size.
    dictionary_size        : Natural := Default_dictionary_size  --  Not used by Level_1, Level_2.
  );

  --  (*) In PKWARE's Appnote (5.8.9), the use of an EOS marker is "highly recommended" for LZMA.
  --
  --  NB: the value of uncompressed_size_info actually determines two variants
  --      of the LZMA header, which are *not* compatible with each other!
  --
  --      In .zip files, uncompressed_size_info = False.
  --      This information is already available in Zip entry headers.
  --      In .lzma files, uncompressed_size_info = True.
  --
  --      When uncompressed_size_info = True, this implementation sets
  --      a special size value indicating that the size is unknown.
  --      Reason: size is not known in advance and the header cannot be
  --      rewritten when processing is done.
  --
  --      See also the has_size field of the LZMA_Hints record in LZMA.Decoding.

end LZMA.Encoding;
