-- This package is only needed by LZMA_Dec, and for Ada 95 compatibility.
-- In Ada 2005+ you can instanciate LZMA_Decoding inside a procedure.

with LZMA_Decoding;

with Ada.Streams.Stream_IO;             use Ada.Streams.Stream_IO;
with Interfaces;                        use Interfaces;

package LZMA_Dec_Pkg is

  subtype Byte is Unsigned_8;
  
  f_in, f_out: Ada.Streams.Stream_IO.File_Type;

  function Read_Byte return Byte;
  procedure Write_Byte(b: Byte);

  package My_LZMA_Decoding is new LZMA_Decoding(Read_Byte, Write_Byte);

end LZMA_Dec_Pkg;
