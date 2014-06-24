-- This package is only needed by LZMA_Dec, for Ada 95 compatibility.
-- In Ada 2005+ you can instanciate LZMA_Decoding in a procedure.

package body LZMA_Dec_Pkg is

  function Read_Byte return Byte is
    b: Byte;
  begin
    Byte'Read(Stream(f_in), b);
    return b;
  end;

  procedure Write_Byte(b: Byte) is
  begin
    Byte'Write(Stream(f_out), b);
  end;

end LZMA_Dec_Pkg;
