--  Files -> LZHuf compression -> LZHuf decompression -> Zip archiving nz_lzhuf.zip
--  Files -> LZMA  compression -> LZMA  decompression -> Zip archiving nz_lzma.zip
--  Files -> (no compression here)                    -> Zip archiving nz_0.zip
--
--  Comparison nz_0.zip, nz_lzhuf.zip, nz_lzma.zip: should be identical.

with Comp_Zip_Prc, LZH, LZMA.Encoding, LZMA.Decoding;

with Zip_Streams;                       use Zip_Streams;
with Zip.Create;                        use Zip.Create;

with Ada.Command_Line;                  use Ada.Command_Line;
with Ada.Text_IO;                       use Ada.Text_IO;

procedure Test_non_zip is

  type Raw_scheme is (plain, lzhuf, lzma);

  nz : array (Raw_scheme) of aliased File_Zipstream;
  info : array (Raw_scheme) of Zip_Create_info;

  procedure Test_LZHuf (fn: String) is
  begin
    null;
  end Test_LZHuf;

begin
  for r in Raw_scheme loop
    Create (info (r), nz (r)'Unchecked_Access, "$nz" & Raw_scheme'Image (r) & ".zip");
  end loop;
  --
  for i in 1 .. Argument_Count loop
    Put_Line ("Testing raw compression on: " & Argument(i));
    --  Plain :
    Add_File (info (plain), Argument (i));
    --  LZHuf :
    Test_LZHuf (Argument (i));
    --  LZMA  :
  end loop;
  --
  for r in Raw_scheme loop
    Finish (info (r));
  end loop;
end Test_non_zip;
