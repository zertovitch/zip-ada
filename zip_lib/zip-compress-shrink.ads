private procedure Zip.Compress.Shrink(
  input,
  output          : Zip_Streams.Zipstream_Class;
  input_size_known: Boolean;
  input_size      : File_size_type; -- ignored if unknown
  feedback        : Feedback_proc;
  CRC             : in out Interfaces.Unsigned_32; -- only updated here
  output_size     : out File_size_type;
  compression_ok  : out Boolean -- indicates compressed <= uncompressed
);
