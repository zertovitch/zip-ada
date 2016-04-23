--  ________  ___   ______       ______     ___
-- /___..._/  |.|   |.___.\     /. __ .\  __|.|   ____
--    /../    |.|   |.____/     |.|__|.| /....|  __\..\
--  _/../___  |.|   |.|    ===  |..__..||. = .| | = ..|
-- /_______/  |_|  /__|        /__|  |_| \__\_|  \__\_|

-- Zip.Compress
---------------
--
--  Created 9-Dec-2007
--
--  This package facilitates the storage or compression of data.
--
--  Note that unlike decompression where the decoding is unique,
--  there is a quasi indefinite number of ways of compressing data into
--  most Zip-supported formats, including LZW (Shrink), Reduce, or Deflate.
--  As a result, you may want to use your own way for compressing data.
--  This package is a portable one and doesn't claim to be the "best".
--  The term "best" is relative to the needs, since there are at least
--  two criteria that usually go in opposite directions: speed and
--  compression ratio, a bit like risk and return in finance.

with Zip_Streams;

package Zip.Compress is

  --  Compression_Method is actually reflecting the way of compressing
  --  data, not only the final compression format called "method" in
  --  Zip specifications. In future versions, there might be other
  --  enumeration items like "PPMd_variant_x" or "LZMA_variant_x"...

  type Compression_Method is
    (--  No compression
     Store,
     --  Shrink = LZW algorithm, as in GIF pictures:
     Shrink,
     --  Reduce combines LZ and a Markov predictor; 4 strengths available:
     Reduce_1,
     Reduce_2,
     Reduce_3,
     Reduce_4,
     --  Deflate combines LZ and Huffman encoding; 4 strengths available:
     Deflate_Fixed,
     Deflate_1,
     Deflate_2,
     Deflate_3
    );

  type Method_to_Format_type is array(Compression_Method) of PKZip_method;
  Method_to_Format: constant Method_to_Format_type;

  subtype Reduction_Method is Compression_Method range Reduce_1 .. Reduce_4;

  --  Deflate_Fixed compresses the data into a single block and with predefined
  --  ("fixed") compression structures. The data are basically LZ-Compressed
  --  only, since the Huffman code sets are flat and not tailored for the data.
  subtype Deflation_Method is Compression_Method range Deflate_Fixed .. Deflate_3;

  --  The multi-block Deflate methods use refined techniques to decide when to
  --  start a new block and what sort of block to put next.
  subtype Taillaule_Deflation_Method is Compression_Method range Deflate_1 .. Deflate_3;

  User_abort: exception;

  --  Compress data from an input stream to an output stream until
  --  End_Of_File(input) = True, or number of input bytes = input_size .
  --  If password /= "", an encryption header is written.

  procedure Compress_data(
    input,
    output          : in out Zip_Streams.Root_Zipstream_Type'Class;
    input_size_known: Boolean;
    input_size      : File_size_type; -- ignored if input_size_known = False
    method          : Compression_Method;
    feedback        : Feedback_proc;
    password        : String;
    CRC             : out Interfaces.Unsigned_32;
    output_size     : out File_size_type;
    zip_type        : out Interfaces.Unsigned_16
    -- ^ code corresponding to the compression method actually used
  );

private

  buffer_size: constant:= 1024 * 1024; -- 1 MB

  Method_to_Format: constant Method_to_Format_type :=
    (Store               => store,
     Shrink              => shrink,
     Reduce_1            => reduce_1,
     Reduce_2            => reduce_2,
     Reduce_3            => reduce_3,
     Reduce_4            => reduce_4,
     Deflation_Method    => deflate
    );

end Zip.Compress;
