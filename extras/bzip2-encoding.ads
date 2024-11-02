package BZip2.Encoding is

  --  Low level: faster but weaker compression
  --  High level: slower but stronger compression
  --
  type Compression_Level is range 1 .. 9;

  generic
    --  Input of data:
    with function  Read_Byte return Byte;
    with function  More_Bytes return Boolean;
    --  Output of LZMA-compressed data:
    with procedure Write_Byte (b : Byte);
    --
  procedure Encode (level : Compression_Level := 1);

end BZip2.Encoding;
