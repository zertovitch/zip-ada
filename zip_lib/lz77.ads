--  Standalone LZ77 compression (encoding) package.
---------------------------------------------------
--  This is a collection of various free LZ77 match finders and encoders.
--  The differences reside in the way matches are found, or skipped.
--  See body (lz77.adb) for details and credits.
--
--  Pure Ada 95+ code, 100% portable: OS-, CPU- and compiler- independent.

--  Legal licensing note:

--  Copyright (c) 2016 .. 2020 Gautier de Montmollin (maintainer of the Ada version)
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

with Interfaces;

package LZ77 is

  type Method_Type is (
    --  Use the LZHuf algorithm (see body for details and credits)
    LZHuf,
    --  Use the Info-Zip algorithm, levels 4-10 (see body for details and credits)
    IZ_4,
    IZ_5,
    IZ_6,
    IZ_7,
    IZ_8,
    IZ_9,
    IZ_10,
    --  Use LZMA SDK's BT4 algorithm (see body for details and credits)
    BT4,
    --  A nice simple LZ77 compressor by Rich Geldreich, Jr.
    Rich,
    --  Just send literals (plain bytes), no LZ77 compression at all.
    --  It is better with LZMA on some rare image formats for instance.
    No_LZ77,
    --  Read LZ77 codes from a text files (for research purposes)
    Read_LZ77_Codes
  );

  subtype Byte is Interfaces.Unsigned_8;

  subtype Distance_Type is Positive;  --  TBD: use Positive_M32

  type Distance_Length_Pair is record
    distance : Distance_Type;
    length   : Positive;
  end record;

  type DLP_Array is array (Positive range <>) of Distance_Length_Pair;

  Max_Length_any_Algo : constant := 2**9;  --  Practically it is 273 with LZMA.

  type Matches_Type is record
    count : Integer := 0;
    dl    : DLP_Array (1 .. Max_Length_any_Algo);
  end record;

  function Are_Matches_Sorted (m : Matches_Type) return Boolean;

  type Byte_Array is array (Natural range <>) of Byte;

  BT4_max_prefetch_positions : constant := 64;

  subtype Prefetch_Index_Type is Natural range 0 .. BT4_max_prefetch_positions;

  type Matches_Array is array (Prefetch_Index_Type range <>) of Matches_Type;

  generic
    ----- LZSS Parameters -----
    String_buffer_size : Integer := 2**12;  --  Default values.
    Look_Ahead         : Integer := 65;     --  Default values.
    Threshold          : Integer := 2;      --  Default values.
    --
    Method : Method_Type;
    --
    --  Input of data:
    with function  Read_Byte return Byte;
    with function  More_Bytes return Boolean;
    --  Output of LZ-compressed data:
    with procedure Write_Literal (b : Byte);
    with procedure Write_DL_Code (distance : Distance_Type; length : Integer);
    --
    LZMA_friendly : Boolean := True;  --  Up to 4 recent distances may be preferred
    --
    --  Scoring of potential DL code emission by the entropy encoder.
    --  This helps choosing between various matches at a given point.
    --  This function is only used by BT4.
    with procedure Estimate_DL_Codes (
      matches          : in out Matches_Array;
      old_match_index  : in     Natural;
      prefixes         : in     Byte_Array;
      best_score_index :    out Positive;
      best_score_set   :    out Prefetch_Index_Type;
      match_trace      :    out DLP_Array
    );
  procedure Encode;

end LZ77;
