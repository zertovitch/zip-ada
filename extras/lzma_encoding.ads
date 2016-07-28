--  LZMA_Encoding - a standalone LZMA encoder.

with Interfaces;

package LZMA_Encoding is

  subtype Byte is Interfaces.Unsigned_8;

  type LZMA_Level is (
    Level_1,  --  Faster, weaker compression
    Level_2   --  Slower, stronger compression
  );

  generic
    level: LZMA_Level;
    -- Input of data:
    with function  Read_byte return Byte;
    with function  More_bytes return Boolean;
    -- Output of LZMA-compressed data:
    with procedure Write_byte (b: Byte);
    --
  procedure Encode;

end LZMA_Encoding;
