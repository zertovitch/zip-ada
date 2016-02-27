--  The "Deflate" method combines the LZ77 compression
--  method with some Huffman encoding gymnastics.
--
--  See body for details.
--
--  Author: G. de Montmollin, February 2011 and later
--

with Zip.CRC_Crypto;                    use Zip.CRC_Crypto;

private procedure Zip.Compress.Deflate(
  input,
  output          : in out Zip_Streams.Root_Zipstream_Type'Class;
  input_size_known: Boolean;
  input_size      : File_size_type; -- ignored if unknown
  feedback        : Feedback_proc;
  method          : Deflation_Method;
  CRC             : in out Interfaces.Unsigned_32; -- only updated here
  crypto          : in out Crypto_pack;
  output_size     : out File_size_type;
  compression_ok  : out Boolean -- indicates when compressed <= uncompressed
);
