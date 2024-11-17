--  BZip2
---------
--
--  Pure Ada 95+ code, 100% portable: OS-, CPU- and compiler- independent.
--
--  bzip2 compresses files using the Burrows-Wheeler block-sorting text
--  compression algorithm, and Huffman coding. Compression is generally
--  considerably better than that achieved by more conventional
--  LZ77/LZ78-based compressors, and approaches the performance of the
--  PPM family of statistical compressors.
--
--  bzip2 was created by Julian Seward in 1996.
--  Web site: https://sourceware.org/bzip2/
--
--  Documentation pointers:
--
--    Burrows-Wheeler transform
--      https://en.wikipedia.org/wiki/Burrows%E2%80%93Wheeler_transform
--
--    MTF Move-To-Front
--      https://en.wikipedia.org/wiki/Move-to-front_transform
--
--  Legal licensing note:
--
--  Copyright (c) 2018 .. 2024 Gautier de Montmollin (maintainer of the Ada version)
--  SWITZERLAND
--
--  Permission is hereby granted, free of charge, to any person obtaining a copy
--  of this software and associated documentation files (the "Software"), to deal
--  in the Software without restriction, including without limitation the rights
--  to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
--  copies of the Software, and to permit persons to whom the Software is
--  furnished to do so, subject to the following conditions:
--
--  The above copyright notice and this permission notice shall be included in
--  all copies or substantial portions of the Software.
--
--  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
--  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
--  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
--  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
--  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
--  OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
--  THE SOFTWARE.
--
--  NB: this is the MIT License, as found 21-Aug-2016 on the site
--  http://www.opensource.org/licenses/mit-license.php

with Ada.Direct_IO;
with Interfaces;

package BZip2 is

  subtype Byte is Interfaces.Unsigned_8;

  --  Ada.Direct_IO is only there for the Data_Bytes_Count type.
  --  In case you want to avoid reference to Ada.Direct_IO,
  --  you can customize the definition of Data_Bytes_Count, provided
  --  it has enough capacity for counting bytes in the streams involved.
  package BIO is new Ada.Direct_IO (Byte);
  subtype Data_Bytes_Count is BIO.Count;

private

  --------------------------------------------------------------
  --  Constants used for both compression and decompression.  --
  --  BZ_* names are as found in bzlib_private.h.             --
  --------------------------------------------------------------

  --  The run_a and run_b symbols are used to encode
  --  the run-lengths in the 2nd RLE phase (the
  --  encoding of MTF indices).

  run_a : constant := 0;  --  BZ_RUNA
  run_b : constant := 1;  --  BZ_RUNB

  --  If all byte values are used, the MTF & RLE_2 encoding
  --  needs this amount of symbols:
  --     *   2  for run_a, run_b
  --     * 255  for non-zero MTF indices
  --     *   1  for EOB.
  --
  max_alphabet_size        : constant := 258;  --  BZ_MAX_ALPHA_SIZE

  max_code_len_max         : constant := 23;   --  BZ_MAX_CODE_LEN
  max_code_len_bzip2_1_0_2 : constant := 20;   --  Actual maximum in bzip2 1.0.2
  max_code_len_bzip2_1_0_3 : constant := 17;   --  Actual maximum in bzip2 1.0.3+
  --
  --  ^ See comments in huffman.c and the hardcoded limit in decompress.c.
  --    Longer codes will trigger a BZ_DATA_ERROR in the latter.
  --    NB: the related C-compiled bzip2 executable shows:
  --         "data integrity (CRC) error in data"
  --    for all kinds of data errors, most of them unrelated to CRC checks!

  --  Each group of data comprises 50 symbols and can use one of up
  --  to 6 different entropy coders (for BZip2: Huffman tables).

  min_entropy_coders : constant := 2;    --  Magic number found in a check in decompress.c ...
  max_entropy_coders : constant := 6;    --  BZ_N_GROUPS (this name is very confusing!)
  group_size         : constant := 50;   --  BZ_G_SIZE

  --  Constants used to calibrate the main memory pool.

  max_block_size : constant := 9;
  sub_block_size : constant := 100_000;
  max_selectors  : constant := 2 + ((max_block_size * sub_block_size) / group_size);  --  BZ_MAX_SELECTORS

  subtype Natural_32  is Interfaces.Integer_32 range 0 .. Interfaces.Integer_32'Last;
  subtype Positive_32 is Interfaces.Integer_32 range 1 .. Interfaces.Integer_32'Last;

  stream_header_magic : constant String := "BZh0";

  stream_footer_magic  : constant String :=
    Character'Val (16#17#) & "rE8P" & Character'Val (16#90#);
  --  ^ sqrt (pi) "decimal" digits, visible in hexadecimal representation!

  block_header_magic : constant String := "1AY&SY";
  --  ^ pi "decimal" digits, visible in hexadecimal representation!

  ---------------------------------------------------------------------------
  --  Cyclic redundancy check to verify uncompressed block data integrity  --
  ---------------------------------------------------------------------------

  package CRC is
    use Interfaces;
    --
    procedure Init (CRC_Value : out Unsigned_32);
    function  Final (CRC_Value : Unsigned_32) return Unsigned_32;
    procedure Update (CRC_Value : in out Unsigned_32; val : Byte);
      pragma Inline (Update);
  end CRC;

end BZip2;
