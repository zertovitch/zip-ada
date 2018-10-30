--  Universal Trained Compression - body - an implementation with LZMA
----------------------------------------------------------------------
--  This implementation uses the LZMA encoder-decoder
--  that is found in the Zip-Ada project's zip_lib directory.
--
--  Legal licensing note:
--
--  Copyright (c) 2018 Gautier de Montmollin
--  SWITZERLAND
--
--  Permission is hereby granted, free of charge, to any person obtaining a copy
--  of this software and associated documentation files (the "Software"), to deal
--  in the Software without restriction, including without limitation the rights
--  to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
--  copies of the Software, and to permit persons to whom the Software is
--  furnished to do so, subject to the following conditions:
--
--  The above copyright notice and this permission notice shall be included in
--  all copies or substantial portions of the Software.
--
--  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
--  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
--  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
--  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
--  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
--  OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
--  THE SOFTWARE.
--
--  NB: this is the MIT License, as found 21-Aug-2016 on the site
--  http://www.opensource.org/licenses/mit-license.php

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
        --  Discard B. We could also output the bytes to the stub file.
      else
        Write_Compressed_Byte (B);
      end if;
    end Write_Byte;

    procedure Specific_Encode is
      new LZMA.Encoding.Encode (Read_Byte, More_Bytes, Write_Byte);

  begin
    Specific_Encode (
      level           => LZMA.Encoding.Level_3,
      dictionary_size => 2 ** 19  --  512 KB
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
