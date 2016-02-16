--  ________  ___   ______       ______     ___
-- /___..._/  |.|   |.___.\     /. __ .\  __|.|   ____
--    /../    |.|   |.____/     |.|__|.| /....|  __\..\
--  _/../___  |.|   |.|    ===  |..__..||. = .| | = ..|
-- /_______/  |_|  /__|        /__|  |_| \__\_|  \__\_|

-- Zip.Compress
---------------
--
-- Created 9-Dec-2007
--
-- This package facilitates the storage or compression of data.
--
-- Note that unlike decompression where the decoding is unique,
-- there are an undefinite number of ways of compressing data into
-- formats which include compression structures, like Deflate.
-- As a result, you may want to use your own way for compressing data,
-- e.g. interfacing with zlib.
-- This package is only a portable one, and doesn't claim
-- to be the best or the fastest.

with Zip_Streams;

package Zip.Compress is

  -- Compression_Method is really reflecting the way of compressing
  -- data, not only the final compression format called "method" in
  -- Zip specifications. In future versions, there might be
  -- enumeration items like "Deflate_zlib_like",
  -- "Deflate_64KB_brute_force", "PPMd_variant_x", or "LZMA_variant_x"...

  type Compression_Method is
    (Store,
     Shrink,
     Reduce_1, Reduce_2, Reduce_3, Reduce_4,
     Deflate_Fixed,  --  Compress the data in one block and with
                     --  predefined ("fixed") compression structures.
     Deflate_Dynamic_1
        --  NB: new method, in development. "There be bugs" !
        --  Better wait for a release for a "prod" usage...
    );

  type Method_to_Format_type is array(Compression_Method) of PKZip_method;
  Method_to_Format: constant Method_to_Format_type;

  subtype Reduction_Method is Compression_Method range Reduce_1 .. Reduce_4;

  subtype Deflation_Method is Compression_Method range Deflate_Fixed .. Deflate_Dynamic_1;

  User_abort: exception;

  -- Compress data from an input stream to an output stream until
  -- End_Of_File(input) = True, or number of input bytes = input_size .
  -- If password /= "", an encryption header is written.

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
     Deflate_Fixed       => deflate,
     Deflate_Dynamic_1   => deflate
    );

end Zip.Compress;
