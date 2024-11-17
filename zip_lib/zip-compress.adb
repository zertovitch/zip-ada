--  Legal licensing note:

--  Copyright (c) 2007 .. 2024 Gautier de Montmollin
--  SWITZERLAND

--  Permission is hereby granted, free of charge, to any person obtaining a copy
--  of this software and associated documentation files (the "Software"), to deal
--  in the Software without restriction, including without limitation the rights
--  to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
--  copies of the Software, and to permit persons to whom the Software is
--  furnished to do so, subject to the following conditions:

--  The above copyright notice and this permission notice shall be included in
--  all copies or substantial portions of the Software.

--  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
--  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
--  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
--  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
--  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
--  OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
--  THE SOFTWARE.

--  NB: this is the MIT License, as found on the site
--  http://www.opensource.org/licenses/mit-license.php

with Zip.Create,
     Zip.Compress.Shrink,
     Zip.Compress.Reduce,
     Zip.Compress.Deflate,
     Zip.Compress.BZip2_E,
     Zip.Compress.LZMA_E;

with Ada.Characters.Handling,
     Ada.Numerics.Discrete_Random,
     Ada.Strings.Fixed,
     Ada.Unchecked_Deallocation;

package body Zip.Compress is

  use Zip_Streams, Zip.CRC_Crypto;

  --  The following procedure's purpose is to detect size overflows
  --  for Zip data. Even when the input size is known, we can have
  --  the situation where data is random and the compressed output size
  --  overflows.

  procedure Increment
    (out_size : in out Zip_64_Data_Size_Type;
     by       : in     Natural)
  is
    temp_by : constant ZS_Size_Type := ZS_Size_Type (by);
    use type Zip_64_Data_Size_Type, ZS_Size_Type;
  begin
    if temp_by > Create.max_size then
      raise Create.Zip_Capacity_Exceeded with
        "Compressed data too large: size is 2 EiB (Exbibytes) or more.";
    end if;
    out_size := out_size + Zip_64_Data_Size_Type (by);
  end Increment;

  default_byte_IO_buffer_size : constant := 1024 * 1024;  --  1 MiB

  -------------------
  -- Compress_data --
  -------------------

  procedure Compress_Data
    (input,
     output           : in out Zip_Streams.Root_Zipstream_Type'Class;
     input_size_known : in     Boolean;
     input_size       : in     Zip_64_Data_Size_Type;  --  ignored if input_size_known = False
     method           : in     Compression_Method;
     feedback         : in     Feedback_Proc;
     password         : in     String;
     content_hint     : in     Data_Content_Type;
     CRC              :    out Interfaces.Unsigned_32;
     output_size      :    out Zip_64_Data_Size_Type;
     zip_type         :    out Interfaces.Unsigned_16)
  is
    use Interfaces;
    user_aborting : Boolean;
    idx_in :  constant ZS_Index_Type := Index (input);
    idx_out : constant ZS_Index_Type := Index (output);
    compression_ok : Boolean;
    first_feedback : Boolean := True;
    --
    is_encrypted : constant Boolean := password /= "";
    encrypt_pack, mem_encrypt_pack : Crypto_pack;
    encrypt_header : Byte_Buffer (1 .. 12);
    package Byte_soup is new Ada.Numerics.Discrete_Random (Byte);
    use Byte_soup;
    cg : Byte_soup.Generator;
    --
    --  Store data as is, or, if do_write = False, just compute CRC (this is for encryption).
    --
    procedure Store_data (do_write : Boolean) is
      Buffer      : Byte_Buffer (1 .. default_byte_IO_buffer_size);
      Last_Read   : Natural;
      counted     : Zip_64_Data_Size_Type := 0;
    begin
      zip_type := Compression_format_code.store_code;
      while not End_Of_Stream (input) loop
        if input_size_known and then counted >= input_size then
          exit;
        end if;
        --  Copy data
        Block_Read (input, Buffer, Last_Read);
        Increment (counted, Last_Read);
        Update (CRC, Buffer (1 .. Last_Read));
        if do_write then
          Encode (encrypt_pack, Buffer (1 .. Last_Read));
          Block_Write (output, Buffer (1 .. Last_Read));
        end if;
        --  Feedback
        if feedback /= null and then
          (first_feedback or (counted mod (2**16) = 0) or
          (input_size_known and then counted = input_size))
        then
          if input_size_known then
            feedback (
              percents_done =>
                Natural ((100.0 * Float (counted)) / Float (input_size)),
              entry_skipped => False,
              user_abort    => user_aborting);
          else
            feedback (
              percents_done => 0,
              entry_skipped => False,
              user_abort    => user_aborting);
          end if;
          first_feedback := False;
          if user_aborting then
            raise User_abort;
          end if;
        end if;
      end loop;
      output_size := counted;
      compression_ok := True;
    end Store_data;
    --
    procedure Compress_data_single_method (actual_method : Single_Method) is
    begin
      Init (CRC);
      if is_encrypted then
        Init_Keys (encrypt_pack, password);
        Set_Mode (encrypt_pack, encrypted);
        --  A bit dumb from Zip spec: we need to know the final CRC in order to set up
        --  the last byte of the encryption header, that allows for detecting if a password
        --  is OK - this, with 255/256 probability of correct detection of a wrong password!
        --  Result: 1st scan of the whole input stream with CRC calculation:
        Store_data (do_write => False);
        Reset (cg);
        for i in 1 .. 11 loop
          encrypt_header (i) := Random (cg);
        end loop;
        encrypt_header (12) := Byte (Shift_Right (Final (CRC), 24));
        Set_Index (input, idx_in);
        Init (CRC);
        Encode (encrypt_pack, encrypt_header);
        Block_Write (output, encrypt_header);
        --
        --  We need to remember at this point the encryption keys in case we need
        --  to rewrite from here (compression failed, store data).
        --
        mem_encrypt_pack := encrypt_pack;
      else
        Set_Mode (encrypt_pack, clear);
      end if;
      --
      --  Dispatch the work to child procedures doing the stream compression
      --  in different formats, depending on the actual compression method.
      --  For example, for methods LZMA_for_JPEG, LZMA_for_WAV, or LZMA_3, we
      --  logically call Zip.Compress.LZMA_E for the job.
      --
      case actual_method is

        when Store =>
          Store_data (do_write => True);

        when Shrink =>
          Zip.Compress.Shrink
            (input, output, input_size_known, input_size, feedback,
             CRC, encrypt_pack, output_size, compression_ok);
          zip_type := Compression_format_code.shrink_code;

        when Reduction_Method =>
          Zip.Compress.Reduce
            (input, output, input_size_known, input_size, feedback,
             actual_method,
             CRC, encrypt_pack, output_size, compression_ok);
          zip_type := Compression_format_code.reduce_code +
            Unsigned_16
              (Compression_Method'Pos (actual_method) -
               Compression_Method'Pos (Reduce_1));

        when Deflation_Method =>
          Zip.Compress.Deflate
            (input, output, input_size_known, input_size, feedback,
             actual_method,
             CRC, encrypt_pack, output_size, compression_ok);
          zip_type := Compression_format_code.deflate_code;

        when BZip2_Method =>
          Zip.Compress.BZip2_E
            (input, output, input_size_known, input_size, feedback,
             actual_method,
             CRC, encrypt_pack, output_size, compression_ok);
          zip_type := Compression_format_code.bzip2_code;

        when LZMA_Method =>
          Zip.Compress.LZMA_E
            (input, output, input_size_known, input_size, feedback,
             actual_method,
             CRC, encrypt_pack, output_size, compression_ok);
          zip_type := Compression_format_code.lzma_code;
      end case;
      CRC := Final (CRC);
      --
      --  Handle case where compression has been unefficient:
      --  data to be compressed is too "random"; then compressed data
      --  happen to be larger than uncompressed data
      --
      if not compression_ok then
        --  Go back to the beginning and just store the data
        Set_Index (input, idx_in);
        if is_encrypted then
          Set_Index (output, idx_out + 12);
          --  Restore the encryption keys to their state just after the encryption header:
          encrypt_pack := mem_encrypt_pack;
        else
          Set_Index (output, idx_out);
        end if;
        Init (CRC);
        Store_data (do_write => True);
        CRC := Final (CRC);
      end if;
      if is_encrypted then
        output_size := output_size + 12;
      end if;
    end Compress_data_single_method;

    fast_presel_threshold : constant := 10_000;
    bzip2_threshold       : constant := 15_000;

    fast_presel : constant Boolean :=
      method = Preselection_1 or (input_size_known and then input_size < fast_presel_threshold);

    data_type_to_LZMA_method : constant array (Data_Content_Type) of LZMA_Method :=
      (JPEG    => LZMA_for_JPEG,
       ARW_RW2 => LZMA_for_ARW,
       ORF_CR2 => LZMA_for_ORF,
       MP3     => LZMA_for_MP3,
       MP4     => LZMA_for_MP4,
       PGM     => LZMA_for_PGM,
       PPM     => LZMA_for_PPM,
       PNG     => LZMA_for_PNG,
       WAV     => LZMA_for_WAV,
       AU      => LZMA_for_AU,
       others  => LZMA_1);  --  Fake, should be unused as such.

  begin
    case method is
      --
      when Single_Method =>
        Compress_data_single_method (method);
      --
      when Preselection_Method =>
        case content_hint is
          when neutral | text_data =>
            if input_size_known and then input_size < 9_000 then
              Compress_data_single_method (Deflate_3);  --  Deflate
            elsif fast_presel then
              --  See: Optimum, LZ77 sheet in za_work.xls
              --       or l2_vs_l3.xls with a larger data set.
              Compress_data_single_method (LZMA_2);                 --  LZMA with IZ_10 match finder
            else
              Compress_data_single_method (LZMA_3);                 --  LZMA with BT4 match finder
            end if;

          when ARW_RW2 | ORF_CR2 | MP3 | MP4 | JPEG | PGM | PPM | PNG | WAV | AU =>
            if input_size_known and then input_size < 2_250 then
              Compress_data_single_method (Deflate_3);  --  Deflate
            else
              Compress_data_single_method (data_type_to_LZMA_method (content_hint));
            end if;

          when GIF =>
            if input_size_known and then input_size < 350 then
              Compress_data_single_method (Deflate_1);
            else
              Compress_data_single_method (LZMA_for_GIF);
            end if;

          when Zip_in_Zip =>
            if input_size_known and then input_size < 1_000 then
              Compress_data_single_method (Deflate_3);  --  Deflate
            elsif fast_presel then
              Compress_data_single_method (LZMA_2_for_Zip_in_Zip);
            else
              Compress_data_single_method (LZMA_3_for_Zip_in_Zip);
            end if;

          when source_code =>
            if input_size_known and then input_size < 8_000 then
              Compress_data_single_method (Deflate_3);  --  Deflate
            elsif fast_presel then
              Compress_data_single_method (LZMA_2_for_Source);
            elsif input_size_known and then input_size < bzip2_threshold then
              Compress_data_single_method (LZMA_3_for_Source);
            else
              Compress_data_single_method (BZip2_Method'Last);
            end if;

          when text_formatted_text_or_dna =>
            if input_size_known and then input_size < 9_000 then
              Compress_data_single_method (Deflate_3);
            elsif fast_presel then
              Compress_data_single_method (LZMA_2);
            elsif input_size_known and then input_size < bzip2_threshold then
              Compress_data_single_method (LZMA_3);
            else
              Compress_data_single_method (BZip2_Method'Last);
            end if;

        end case;
    end case;
  end Compress_Data;

  function Guess_Type_from_Name (name : String) return Data_Content_Type is
    use Ada.Characters.Handling, Ada.Strings, Ada.Strings.Fixed;
    up : constant String := To_Upper (name);
    dot : constant Natural := Index (up, ".", Backward);
  begin
    if dot = 0 then
      return neutral;
    end if;
    declare
      ext : constant String := up (dot + 1 .. up'Last);
    begin
      if ext in "JPG" | "JPEG" then
        return JPEG;
      end if;
      if ext in
        "A"    | "ADA"  | "ADS" | "ADB" |     --  Ada
        "PRC"  | "PKG"  | "HAC" | "GPR" |
        "F"    | "FOR"  |                     --  Fortran
        "C"    | "H"    | "CPP" | "HPP" |     --  C/C++
        "DEF"  | "ASM"  |                     --  Assembler
        "JAVA" | "CS"   |
        "PAS"  | "INC"  | "LPR" | "PP" |      --  Pascal
        "M"    |                              --  Matlab
        "M4"   | "MAK"  | "IN"  |             --  Macro assembler
        "SH"   | "BAT"  | "CMD" |             --  Operating System Script
        "PO"   |                              --  GNU PO
        "XML"  | "XSL"  |
        "SGML" |
        "AUP"  |                              --  Audacity project (XML)
        "HTM"  | "HTML" |
        "JS"   | "LSP"  | "SCM" |
        "SQL"  | "PDB"  | "PL"
      then
        return source_code;
      end if;
      if ext in "CFG" | "INI" | "CSV" | "SVG" | "JSON" then
        return text_data;
      end if;
      if ext in "TXT" | "RTF" | "HTM" | "HTML" | "GB" | "FASTA" then
        return text_formatted_text_or_dna;
      end if;
      --  Zip archives happen to be zipped...
      if ext in
        "EPUB" |  --  EPUB: e-book reader format
        "ZIP"  |
        "JAR"  |
        "ODB"  | "ODS"  | "ODT" | "OTR" | "OTS" | "OTT" |
        "CRX"  | "NTH"  |
        "DOCX" | "PPTX" | "XLSX" | "XLSB" | "XLSM"
      then
        return Zip_in_Zip;
      end if;
      --  Some raw camera picture data
      if ext in "ORF" |  --  Raw Olympus
                "CR2" |  --  Raw Canon
                "RAF" |  --  Raw Fujifilm
                "SRW"    --  Raw Samsung
      then
        return ORF_CR2;
      end if;
      if ext in "ARW" |  --  Raw Sony
                "RW2" |  --  Raw Panasonic
                "NEF" |  --  Raw Nikon
                "DNG" |  --  Raw Leica, Pentax
                "X3F"    --  Raw Sigma
      then
        return ARW_RW2;
      end if;
      if ext = "PGM" then
        return PGM;
      end if;
      if ext = "PPM" then
        return PPM;
      end if;
      if ext = "MP3" then
        return MP3;
      end if;
      if ext in "MTS" | "MP4" | "M4A" | "M4P" then
        return MP4;
      end if;
      if ext = "PNG" then
        return PNG;
      end if;
      if ext = "GIF" then
        return GIF;
      end if;
      if ext in "WAV" | "UAX" then
        return WAV;
      end if;
      if ext = "AU" then  --  Audacity raw data
        return AU;
      end if;
    end;
    return neutral;
  end Guess_Type_from_Name;

  -----------------------------------
  --  I/O buffers for compression  --
  -----------------------------------

  procedure Allocate_Buffers
    (b                : in out IO_Buffers_Type;
     input_size_known :        Boolean;
     input_size       :        Zip_64_Data_Size_Type)
  is
    calibration : Zip_64_Data_Size_Type := default_byte_IO_buffer_size;
  begin
    if input_size_known then
      calibration :=
        Zip_64_Data_Size_Type'Min
          (default_byte_IO_buffer_size,
           Zip_64_Data_Size_Type'Max (8, input_size));
    end if;
    b.InBuf  := new Byte_Buffer (1 .. Integer (calibration));
    b.OutBuf := new Byte_Buffer (1 .. default_byte_IO_buffer_size);
  end Allocate_Buffers;

  procedure Deallocate_Buffers (b : in out IO_Buffers_Type) is
    procedure Dispose_Buffer is
      new Ada.Unchecked_Deallocation (Byte_Buffer, p_Byte_Buffer);
  begin
    Dispose_Buffer (b.InBuf);
    Dispose_Buffer (b.OutBuf);
  end Deallocate_Buffers;

  procedure Read_Block
    (b     : in out IO_Buffers_Type;
     input : in out Zip_Streams.Root_Zipstream_Type'Class)
  is
  begin
    Zip.Block_Read
      (stream        => input,
       buffer        => b.InBuf.all,
       actually_read => b.MaxInBufIdx);
    b.InputEoF := b.MaxInBufIdx = 0;
    b.InBufIdx := 1;
  end Read_Block;

  procedure Write_Block
    (b                : in out IO_Buffers_Type;
     input_size_known :        Boolean;
     input_size       :        Zip_64_Data_Size_Type;
     output           : in out Zip_Streams.Root_Zipstream_Type'Class;
     output_size      : in out Zip_64_Data_Size_Type;
     crypto           : in out Zip.CRC_Crypto.Crypto_pack)
  is
    amount : constant Integer := b.OutBufIdx - 1;
    use type Zip_64_Data_Size_Type;
  begin
    Increment (output_size, Integer'Max (0, amount));
    if input_size_known and then output_size >= input_size then
      --  The compression so far is obviously inefficient for that file.
      --  Useless to go further.
      --  Stop immediately before growing the file more than the
      --  uncompressed size.
      raise Compression_inefficient;
    end if;
    Encode (crypto, b.OutBuf (1 .. amount));
    Zip.Block_Write (output, b.OutBuf (1 .. amount));
    b.OutBufIdx := 1;
  end Write_Block;

end Zip.Compress;
