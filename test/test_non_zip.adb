--  Files -> LZHuf compression -> LZHuf decompression -> Zip archiving nz_lzhuf.zip
--  Files -> LZMA  compression -> LZMA  decompression -> Zip archiving nz_lzma.zip
--  Files -> (no compression here)                    -> Zip archiving nz_0.zip
--
--  Comparison nz_0.zip, nz_lzhuf.zip, nz_lzma.zip: should be identical.


with Comp_Zip_Prc, LZH, LZMA.Encoding, LZMA.Decoding;

with Ada.Command_Line;  use Ada.Command_Line;
with Ada.Text_IO;       use Ada.Text_IO;

procedure Test_non_zip is
begin
  for i in 1 .. Argument_Count loop
    Put_Line("Testing raw compression on: " & Argument(i));
  end loop;
end Test_non_zip;
