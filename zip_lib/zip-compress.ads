--   ________  ___   ______       ______      ___
--  /___..._/  |.|   |.___.\     /. __ .\   __|.|   ____
--     /../    |.|   |.____/     |.|__|.|  /....|  __\..\
--   _/../___  |.|   |.|    ===  |..__..| |. = .| | = ..|
--  /_______/  |_|  /__|        /__|  |_|  \__\_|  \__\_|

--  Zip.Compress
----------------
--
--  Created 9-Dec-2007
--
--  This package facilitates the storage or compression of data.
--
--  Note that unlike decompression where the decoding is unique,
--  there is a quasi indefinite number of ways of compressing data into
--  most Zip-supported formats, including LZW (Shrink), Reduce, Deflate, or LZMA.
--  As a result, you may want to use your own way for compressing data.
--  This package is a portable one and doesn't claim to be the "best".
--  The term "best" is relative to the needs, since there are at least
--  two criteria that usually go in opposite directions: speed and
--  compression ratio, a bit like risk and return in finance.

--  Legal licensing note:

--  Copyright (c) 2007 .. 2020 Gautier de Montmollin
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

---------------------------------------------------------------------------------

with Zip_Streams, Zip.CRC_Crypto;

package Zip.Compress is

  --  Compression_Method is actually reflecting the way of compressing
  --  data, not only the final compression format called "method" in
  --  Zip specifications.

  type Compression_Method is
   ( --  No compression:
     Store,
     --  Shrink = LZW algorithm, as in GIF pictures:
     Shrink,
     --  Reduce combines LZ and a Markov predictor; 4 strengths available:
     Reduce_1,
     Reduce_2,
     Reduce_3,
     Reduce_4,
     --  Deflate combines LZ and Huffman encoding:
     Deflate_Fixed,
     Deflate_0,        --  0: No LZ77, only Huffman.
     Deflate_1,
     Deflate_2,
     Deflate_3,
     Deflate_R,
     --  LZMA:
     LZMA_0,           --  0: No LZ77, only "MA" part.
     LZMA_1,
     LZMA_2,
     LZMA_3,           --  NB: LZMA_3 can be very slow on large data
     --  LZMA with non-default parameters, targeted for specific data types:
     LZMA_2_for_Zip_in_Zip,
     LZMA_3_for_Zip_in_Zip,
     LZMA_2_for_Source,
     LZMA_3_for_Source,
     LZMA_for_JPEG,
     LZMA_for_ARW,   --  Raw camera picture
     LZMA_for_ORF,   --  Raw camera picture
     LZMA_for_MP3,
     LZMA_for_MP4,
     LZMA_for_PGM,  --  TBD: photo vs drawing (expect much LZ redundancy in the latter)
     LZMA_for_PPM,  --  TBD: photo vs drawing (expect much LZ redundancy in the latter)
     LZMA_for_PNG,
     LZMA_for_GIF,
     LZMA_for_WAV,
     LZMA_for_AU,
     --  Multi-method:
     --    Preselection: select a method depending on hints, like the uncompressed size
     Preselection_1,  --  Not too slow; selects Deflate_3 or LZMA_2*
     Preselection_2   --  Can be very slow on large data; selects Deflate_3, LZMA_2* or LZMA_3*
    );

  type Method_to_Format_type is array (Compression_Method) of PKZip_method;
  Method_to_Format : constant Method_to_Format_type;

  subtype Reduction_Method is Compression_Method range Reduce_1 .. Reduce_4;

  --  Deflate_Fixed compresses the data into a single block and with predefined
  --  ("fixed") compression structures. The data are basically LZ-compressed
  --  only, since the Huffman code sets are flat and not tailored for the data.
  subtype Deflation_Method is Compression_Method range Deflate_Fixed .. Deflate_R;

  --  The multi-block Deflate methods use refined techniques to decide when to
  --  start a new block and what sort of block to put next.
  subtype Taillaule_Deflation_Method is Compression_Method range Deflate_0 .. Deflation_Method'Last;

  subtype LZMA_Method is Compression_Method range LZMA_0 .. LZMA_for_AU;

  subtype Multi_Method is Compression_Method range Preselection_1 .. Preselection_2;

  subtype Preselection_Method is Compression_Method range Preselection_1 .. Preselection_2;

  subtype Single_Method is Compression_Method
    range Compression_Method'First .. Compression_Method'Pred (Multi_Method'First);

  User_abort : exception;

  type Data_content_type is (
    Neutral,
    Source_code,
    JPEG,
    ARW_RW2,     --  Raw digital camera image
    ORF_CR2,     --  Raw digital camera image
    Zip_in_Zip,
    GIF, PNG, PGM, PPM,
    WAV,
    AU,          --  Audacity .au raw sound file
    MP3, MP4
  );

  --  Compress data from an input stream to an output stream until
  --  End_Of_File(input) = True, or number of input bytes = input_size .
  --  If password /= "", an encryption header is written.

  procedure Compress_data (
    input,
    output           : in out Zip_Streams.Root_Zipstream_Type'Class;
    input_size_known : Boolean;
    input_size       : Zip_32_Data_Size_Type; -- ignored if input_size_known = False
    method           : Compression_Method;
    feedback         : Feedback_proc;
    password         : String;
    content_hint     : Data_content_type;
    CRC              : out Interfaces.Unsigned_32;
    output_size      : out Zip_32_Data_Size_Type;
    zip_type         : out Interfaces.Unsigned_16
    --  ^ code corresponding to the compression method actually used
  );

  function Guess_type_from_name (name : String) return Data_content_type;

private

  feedback_steps : constant := 100;

  Method_to_Format : constant Method_to_Format_type :=
    (Store               => store,
     Shrink              => shrink,
     Reduce_1            => reduce_1,
     Reduce_2            => reduce_2,
     Reduce_3            => reduce_3,
     Reduce_4            => reduce_4,
     Deflation_Method    => deflate,
     LZMA_Method         => lzma_meth,
     Multi_Method        => unknown
    );

  -----------------------------------
  --  I/O buffers for compression  --
  -----------------------------------

  type IO_Buffers_Type is record
    InBuf  : p_Byte_Buffer := null;  --  I/O buffers
    OutBuf : p_Byte_Buffer := null;
    --
    InBufIdx  : Positive;       --  Points to next char in buffer to be read
    OutBufIdx : Positive := 1;  --  Points to next free space in output buffer
    --
    MaxInBufIdx : Natural;      --  Count of valid chars in input buffer
    InputEoF    : Boolean;      --  End of file indicator
  end record;

  procedure Allocate_Buffers (
    b                : in out IO_Buffers_Type;
    input_size_known :        Boolean;
    input_size       :        Zip_32_Data_Size_Type
  );

  procedure Deallocate_Buffers (b : in out IO_Buffers_Type);

  procedure Read_Block (
    b     : in out IO_Buffers_Type;
    input : in out Zip_Streams.Root_Zipstream_Type'Class
  );

  procedure Write_Block (
    b                : in out IO_Buffers_Type;
    input_size_known :        Boolean;
    input_size       :        Zip_32_Data_Size_Type;
    output           : in out Zip_Streams.Root_Zipstream_Type'Class;
    output_size      : in out Zip_32_Data_Size_Type;
    crypto           : in out Zip.CRC_Crypto.Crypto_pack
  );

  --  Exception for the case where compression works but produces
  --  a bigger file than the file to be compressed (data is too "random").
  Compression_inefficient : exception;

end Zip.Compress;
