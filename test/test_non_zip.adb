--  Testing stream compressors. Not directly related to Zip.
--
--  Files -> LZHuf compression -> LZHuf decompression -> Zip archiving nz_lzhuf.zip
--  Files -> LZMA  compression -> LZMA  decompression -> Zip archiving nz_lzma.zip
--  Files ->           (no compression here)          -> Zip archiving nz_0.zip
--
--  Comparison nz_0.zip, nz_lzhuf.zip, nz_lzma.zip: should be identical.

with Comp_Zip_Prc, LZH, LZMA.Encoding, LZMA.Decoding;

with Zip.Create;                        use Zip.Create;

with Ada.Command_Line;                  use Ada.Command_Line;
with Ada.Sequential_IO;
with Ada.Text_IO;                       use Ada.Text_IO;
with Interfaces;

procedure Test_non_zip is

  type Raw_scheme is (no_compression, lzhuf_scheme, lzma_scheme);

  nz   : array (Raw_scheme) of aliased Zip_File_Stream;
  info : array (Raw_scheme) of Zip_Create_Info;
  zi   : array (Raw_scheme) of Zip.Zip_Info;

  subtype Byte is Interfaces.Unsigned_8;

  --  NB: The Byte I/O below is not buffered, so it is very slow.
  --  You need to implement a circular buffer of type Stream_Element_Array for a fast I/O.
  --  For instance, see the BlockRead in the Zip package for how to do it.

  package Byte_IO is new Ada.Sequential_IO (Byte);

  Infile, Outfile : Byte_IO.File_Type;

  function TNZ_Read_Byte return Byte is
    B : Byte;
  begin
    Byte_IO.Read (Infile, B);
    return B;
  end TNZ_Read_Byte;

  function TNZ_More_bytes return Boolean is
  begin
    return not Byte_IO.End_Of_File (Infile);
  end TNZ_More_bytes;

  procedure TNZ_Write_Byte (B : Byte) is
  begin
    Byte_IO.Write (Outfile, B);
  end TNZ_Write_Byte;

  procedure Test_LZHuf (fn : String) is
    package File_LZH is
      new LZH (
        Read_byte  => TNZ_Read_Byte,
        More_bytes => TNZ_More_bytes,
        Write_byte => TNZ_Write_Byte
      );
    temp_encoded : constant String := "lzhuf.tmp";
    temp_decoded : constant String := "lzhuf_decoded.tmp";
  begin
    Byte_IO.Open (Infile,  Byte_IO.In_File, Name => fn);
    Byte_IO.Create (Outfile, Name => temp_encoded);
    File_LZH.Encode;
    Byte_IO.Close (Infile);
    Byte_IO.Close (Outfile);
    --
    Byte_IO.Open (Infile,  Byte_IO.In_File, Name => temp_encoded);
    Byte_IO.Create (Outfile, Name => temp_decoded);
    File_LZH.Decode;
    Byte_IO.Close (Infile);
    Byte_IO.Close (Outfile);
    Add_File (info (lzhuf_scheme), temp_decoded, Name_in_archive => fn);
  end Test_LZHuf;

  procedure Test_LZMA (fn : String) is
    procedure File_LZMA_Encode is new LZMA.Encoding.Encode (TNZ_Read_Byte, TNZ_More_bytes, TNZ_Write_Byte);
    package File_LZMA_Decoding is new LZMA.Decoding (TNZ_Read_Byte, TNZ_Write_Byte);
    use File_LZMA_Decoding;
    default_hints : constant LZMA_Hints :=
       (has_size               => True,
        given_size             => dummy_size,
        marker_expected        => False,
        fail_on_bad_range_code => False);
    lzma_decoder : LZMA_Decoder_Info;
    res : LZMA_Result;
    --
    temp_encoded : constant String := "lzma.tmp";
    temp_decoded : constant String := "lzma_decoded.tmp";
  begin
    Byte_IO.Open (Infile,  Byte_IO.In_File, Name => fn);
    Byte_IO.Create (Outfile, Name => temp_encoded);
    File_LZMA_Encode (uncompressed_size_info => True);
    Byte_IO.Close (Infile);
    Byte_IO.Close (Outfile);
    --
    Byte_IO.Open (Infile,  Byte_IO.In_File, Name => temp_encoded);
    Byte_IO.Create (Outfile, Name => temp_decoded);
    File_LZMA_Decoding.Decode (lzma_decoder, default_hints, res);
    Byte_IO.Close (Infile);
    Byte_IO.Close (Outfile);
    Add_File (info (lzma_scheme), temp_decoded, Name_in_archive => fn);
  end Test_LZMA;

  total_differences : Natural;

begin
  if Argument_Count = 0 then
    Put_Line ("Test_non_zip: test raw compression schemes on many files");
    Put_Line ("  Each file is encoded, then decoded, then added to a Zip archive (one Zip per scheme).");
    Put_Line ("  When all files are processed, the resulting Zip archive is compared to original.");
    New_Line;
    Put_Line ("Syntax: test_non_zip [files]");
    Put_Line ("NB: you can use wildcards");
    return;
  end if;
  for r in Raw_scheme loop
    Create_Archive (info (r), nz (r)'Unchecked_Access, "$nz" & Raw_scheme'Image (r) & ".zip");
  end loop;
  --
  for i in 1 .. Argument_Count loop
    Put_Line ("Testing raw compression on: " & Argument (i));
    --  Plain :
    Put_Line ("  - Plain - no compression before zipping");
    Add_File (info (no_compression), Argument (i));
    --  LZHuf :
    Put_Line ("  - LZHuf");
    Test_LZHuf (Argument (i));
    --  LZMA  :
    Put_Line ("  - LZMA");
    Test_LZMA (Argument (i));
  end loop;
  --
  for r in Raw_scheme loop
    Finish (info (r));
    Zip.Load (zi (r), "$nz" & Raw_scheme'Image (r) & ".zip");
    if r > no_compression then
      Comp_Zip_Prc (zi (no_compression), zi (r), quiet => 0, total_differences => total_differences);
    end if;
  end loop;
  --
end Test_non_zip;
