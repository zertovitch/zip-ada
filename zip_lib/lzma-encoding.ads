--  LZMA_Encoding - a standalone LZMA encoder.
--  See body for credits and other informations.

with Interfaces;

package LZMA.Encoding is

  subtype Byte is Interfaces.Unsigned_8;

  type LZMA_compression_level is (
    Level_1,  --  Faster but weaker compression
    Level_2   --  Slower but stronger compression
  );

  generic
    -- Input of data:
    with function  Read_byte return Byte;
    with function  More_bytes return Boolean;
    -- Output of LZMA-compressed data:
    with procedure Write_byte (b: Byte);
    --
  procedure Encode(
    level                 : LZMA_compression_level      := Level_1;
    literal_context_bits  : Literal_context_bits_range  := 3;  --  "Literal context" bits
    literal_position_bits : Literal_position_bits_range := 0;  --  "Literal position" bits
    position_bits         : Position_bits_range         := 2;  --  "Position" bits
    end_marker            : Boolean:= True  --  Produce an End-Of-Stream marker ?
  );

end LZMA.Encoding;
