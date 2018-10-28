--  This implementation uses the LZMA encoder-decoder
--  that is found in the Zip-Ada project's zip_lib directory.

with LZMA.Encoding, LZMA.Decoding;

package body Trained_Compression is

  ------------
  -- Encode --
  ------------

  procedure Encode (Train_Uncompressed, Skip_Compressed : Data_Bytes_Count) is
    total_uncompressed : Data_Bytes_Count := 0;
    total_compressed   : Data_Bytes_Count := 0;

    --  Call-back subprograms for the untrained encoder.
    function Read_Byte return Byte is
    begin
      if total_uncompressed < Train_Uncompressed then
        total_uncompressed := total_uncompressed + 1;
        return Read_Uncompressed_Training;
      else
        return Read_Uncompressed_Data;
      end if;
    end Read_Byte;

    function More_Bytes return Boolean is
    begin
      if total_uncompressed < Train_Uncompressed then
        return True;
      else
        return More_Uncompressed_Data_Bytes;
      end if;
    end More_Bytes;

    procedure Write_Byte (B: Byte) is
    begin
      if total_compressed < Skip_Compressed then
        total_compressed := total_compressed + 1;
        --  Discard B.
      else
        Write_Compressed_Byte (B);
      end if;
    end Write_Byte;

    procedure Specific_Encode is
      new LZMA.Encoding.Encode (Read_Byte, More_Bytes, Write_Byte);

  begin
    Specific_Encode (
      level           => LZMA.Encoding.Level_3,
      dictionary_size => 2 ** 16
    );
  end Encode;

  ------------
  -- Decode --
  ------------

  procedure Decode (Train_Compressed, Skip_Decompressed : Data_Bytes_Count)
  is
    total_compressed   : Data_Bytes_Count := 0;
    total_decompressed : Data_Bytes_Count := 0;

    --  Call-back subprograms for the untrained encoder.
    function Read_Byte return Byte is
    begin
      if total_compressed < Train_Compressed then
        total_compressed := total_compressed + 1;
        return Read_Compressed_Training;
      else
        return Read_Compressed_Data;
      end if;
    end Read_Byte;

    procedure Write_Byte (B: Byte) is
    begin
      if total_decompressed < Skip_Decompressed then
        total_decompressed := total_decompressed + 1;
        --  Discard B.
      else
        Write_Decompressed_Byte (B);
      end if;
    end Write_Byte;

    package Specific_Decoding is
      new LZMA.Decoding (Read_Byte, Write_Byte);

  begin
    Specific_Decoding.Decompress (
      hints =>
        ( has_size               => False,
          given_size             => Specific_Decoding.dummy_size,
          marker_expected        => True,
          fail_on_bad_range_code => False
        )
    );
  end Decode;

end Trained_Compression;
