--  ________  ___   ______       ______     ___
-- /___..._/  |.|   |.___.\     /. __ .\  __|.|   ____
--    /../    |.|   |.____/     |.|__|.| /....|  __\..\
--  _/../___  |.|   |.|    ===  |..__..||. = .| | = ..|
-- /_______/  |_|  /__|        /__|  |_| \__\_|  \__\_|

-- Zip.Compress
---------------
--
-- Created 9-Dec-2007
--
-- This package facilitates the storage or compression of data.
--
-- Note that unlike decompression where the decoding is unique,
-- there are an indefinite number of ways of compressing data into
-- formats which include compression structures, like Deflate.
-- As a result, you may want to use your own way (e.g. interfacing
-- with zlib).
-- This package is only a portable one, and doesn't claim
-- to be the best or the fastest

with Zip.CRC_Crypto,
     Zip.Compress.Shrink,
     Zip.Compress.Reduce,
     Zip.Compress.Deflate,
     Zip.Compress.LZMA_E;

with Ada.Characters.Handling;           use Ada.Characters.Handling;
with Ada.Numerics.Discrete_Random;
with Ada.Strings.Fixed;                 use Ada.Strings.Fixed;

package body Zip.Compress is

  use Zip_Streams, Zip.CRC_Crypto;

  -------------------
  -- Compress_data --
  -------------------

  procedure Compress_data
   (input,
    output          : in out Zip_Streams.Root_Zipstream_Type'Class;
    input_size_known: Boolean;
    input_size      : File_size_type;
    method          : Compression_Method;
    feedback        : Feedback_proc;
    password        : String;
    content_hint    : Data_content_type;
    CRC             : out Interfaces.Unsigned_32;
    output_size     : out File_size_type;
    zip_type        : out Interfaces.Unsigned_16
   )
  is
    use Interfaces;
    counted: File_size_type;
    user_aborting: Boolean;
    idx_in:  constant ZS_Index_Type:= Index(input);
    idx_out: constant ZS_Index_Type:= Index(output);
    compression_ok: Boolean;
    first_feedback: Boolean:= True;
    --
    is_encrypted: constant Boolean:= password /= "";
    encrypt_pack, mem_encrypt_pack: Crypto_pack;
    encrypt_header: Byte_Buffer(1..12);
    package Byte_soup is new Ada.Numerics.Discrete_Random(Byte);
    use Byte_soup;
    cg: Byte_soup.Generator;
    --
    --  Store data as is, or, if do_write = False, juste compute CRC (this is for encryption).
    --
    procedure Store_data(do_write: Boolean) is
      Buffer      : Byte_Buffer (1 .. buffer_size);
      Last_Read   : Natural;
    begin
      zip_type:= 0; -- "Store" method
      counted:= 0;
      while not End_Of_Stream(input) loop
        if input_size_known and counted >= input_size then
          exit;
        end if;
        -- Copy data
        BlockRead (input, Buffer, Last_Read);
        counted:= counted + File_size_type (Last_Read);
        Update(CRC, Buffer (1 .. Last_Read));
        if do_write then
          Encode(encrypt_pack, Buffer (1 .. Last_Read));
          BlockWrite(output, Buffer (1 .. Last_Read));
        end if;
        -- Feedback
        if feedback /= null and then
          (first_feedback or (counted mod (2**16)=0) or
          (input_size_known and counted = input_size))
        then
          if input_size_known then
            feedback(
              percents_done =>
                Natural( (100.0 * Float(counted)) / Float(input_size) ),
              entry_skipped => False,
              user_abort    => user_aborting );
          else
            feedback(
              percents_done => 0,
              entry_skipped => False,
              user_abort    => user_aborting );
          end if;
          first_feedback:= False;
          if user_aborting then
            raise User_abort;
          end if;
        end if;
      end loop;
      output_size:= counted;
      compression_ok:= True;
    end Store_data;
    --
    procedure Compress_data_single_method(actual_method: Single_Method) is
    begin
      Init(CRC);
      if is_encrypted then
        Init_keys(encrypt_pack, password);
        Set_mode(encrypt_pack, encrypted);
        --  A bit dumb from Zip spec: we need to know the final CRC in order to set up
        --  the last byte of the encryption header, that allows for detecting if a password
        --  is OK - this, with 255/256 probability of correct detection of a wrong password!
        --  Result: 1st scan of the whole input stream with CRC calculation:
        Store_data(do_write => False);
        Reset(cg);
        for i in 1..11 loop
          encrypt_header(i):= Random(cg);
        end loop;
        encrypt_header(12):= Byte(Shift_Right( Final(CRC), 24 ));
        Set_Index(input, idx_in);
        Init(CRC);
        Encode(encrypt_pack, encrypt_header);
        BlockWrite(output, encrypt_header);
        --
        --  We need to remember at this point the encryption keys in case we need
        --  to rewrite from here (compression failed, store data).
        --
        mem_encrypt_pack:= encrypt_pack;
      else
        Set_mode(encrypt_pack, clear);
      end if;
      case actual_method is
        --
        when Store =>
          Store_data(do_write => True);
        --
        when Shrink =>
          Zip.Compress.Shrink(
            input, output, input_size_known, input_size, feedback,
            CRC, encrypt_pack, output_size, compression_ok
          );
          zip_type:= 1; -- code for "Shrink" method
        --
        when Reduction_Method =>
          Zip.Compress.Reduce(
            input, output, input_size_known, input_size, feedback,
            actual_method,
            CRC, encrypt_pack, output_size, compression_ok
          );
          zip_type:= 2 + Unsigned_16(
            Compression_Method'Pos(actual_method) -
            Compression_Method'Pos(Reduce_1)
          );
        when Deflation_Method =>
          Zip.Compress.Deflate(
            input, output, input_size_known, input_size, feedback,
            actual_method,
            CRC, encrypt_pack, output_size, compression_ok
          );
          zip_type:= 8;
        when LZMA_Method =>
          Zip.Compress.LZMA_E(
            input, output, input_size_known, input_size, feedback,
            actual_method,
            CRC, encrypt_pack, output_size, compression_ok
          );
          zip_type:= 14;
      end case;
      CRC:= Final(CRC);
      --
      -- Handle case where compression has been unefficient:
      -- data to be compressed is too "random"; then compressed data
      -- happen to be larger than uncompressed data
      --
      if not compression_ok then
        -- Go back to the beginning and just store the data
        Set_Index(input, idx_in);
        if is_encrypted then
          Set_Index(output, idx_out + 12);
          encrypt_pack:= mem_encrypt_pack;
          -- ^ Restore the encryption keys to their state just after the encryption header
        else
          Set_Index(output, idx_out);
        end if;
        Init(CRC);
        Store_data(do_write => True);
        CRC:= Final(CRC);
      end if;
      if is_encrypted then
        output_size:= output_size + 12;
      end if;
    end Compress_data_single_method;

    fast_presel: constant Boolean:=
      method = Preselection_1 or (input_size_known and input_size < 22_805);

  begin
    case method is
      --
      when Single_Method =>
        Compress_data_single_method(method);
      --
      when Preselection_Method =>
        case content_hint is
          when Neutral =>  --  No clue about what kind of data
            if input_size_known and input_size < 9_000 then
              Compress_data_single_method(Deflation_Method'Last);  --  Deflate
            elsif fast_presel then
              --  See: Optimum, LZ77 sheet in za_work.xls
              --       or l2_vs_l3.xls with a larger data set.
              Compress_data_single_method(LZMA_2);                 --  LZMA with IZ_10 match finder
            else
              Compress_data_single_method(LZMA_3);                 --  LZMA with BT4 match finder
            end if;
          when JPEG_and_Co =>
            Compress_data_single_method(LZMA_for_JPEG);
          when ARW_RW2 =>
            Compress_data_single_method(LZMA_for_ARW);
          when PNG =>
            Compress_data_single_method(LZMA_for_PNG);
          when GIF =>
            if input_size_known and input_size < 350 then
              Compress_data_single_method(Deflate_1);
            else
              Compress_data_single_method(LZMA_for_GIF);
            end if;
          when Zip_in_Zip =>
            if fast_presel then
              Compress_data_single_method(LZMA_2_for_Zip_in_Zip);
            else
              Compress_data_single_method(LZMA_3_for_Zip_in_Zip);
            end if;
        end case;
    end case;
  end Compress_data;

  function Guess_type_from_name(name: String) return Data_content_type is
    up: constant String:= To_Upper(name);
    ext_3: constant String:= Tail(up, 4);
    ext_4: constant String:= Tail(up, 5);
  begin
    if ext_3 = ".JPG" or else ext_4 = ".JPEG"
      --  Not related to JPEG, but are compressed as well better in "pure Markov" mode with LZMA:
      or else ext_3 = ".ORF" or else ext_3 = ".CR2"  --  Raw camera files: Olympus, Canon
    then
      return JPEG_and_Co;
    end if;
    --  Zip archives happen to be zipped...
    if ext_4 = ".EPUB"  --  EPUB: e-book reader format
      or else ext_3 = ".JAR" or else ext_3 = ".ZIP"
      or else ext_3 = ".ODB" or else ext_3 = ".ODS" or else ext_3 = ".ODT"
      or else ext_3 = ".OTR" or else ext_3 = ".OTS" or else ext_3 = ".OTT"
      or else ext_4 = ".XLSX"
    then
      return Zip_in_Zip;
    end if;
    if ext_3 = ".ARW" or else ext_3 = ".RW2"
      or else ext_3 = ".MTS" or else ext_3 = ".MP3"
      or else ext_3 = ".MP4" or else ext_3 = ".M4A" or else ext_3 = ".M4P"
    then
      return ARW_RW2;
    end if;
    if ext_3 = ".PNG" then
      return PNG;
    end if;
    if ext_3 = ".GIF" then
      return GIF;
    end if;
    return Neutral;
  end Guess_type_from_name;

end Zip.Compress;
