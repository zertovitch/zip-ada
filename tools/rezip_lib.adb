------------------------------------------------------------------------------
--  File:            rezip_lib.adb
--  Description:     Recompression tool to make archives smaller.
--                   Core moved from Rezip (main). Still Q&D !
--  Author:          Gautier de Montmollin
------------------------------------------------------------------------------
--
--  To do:
--    * In order to facilitate customization, ReZip could have a config file
--      ( http://sf.net/projects/ini-files/ ) to store external packer
--      program names. See ZipMax as an example...
--
--  External programs used (feel free to customize/add/remove):
--    7-Zip, KZip, Zip (info-zip), AdvZip, DeflOpt
--    Web URL's: see Zipper_specification below or run ReZip without arguments.

with Zip.Headers, Zip.Compress, UnZip;
with Zip.Create;                        use Zip.Create;
with Zip_Streams;                       use Zip_Streams;

with Flexible_temp_files;
with Zip_Console_IO;

with Ada.Calendar;                      use Ada.Calendar;
with Ada.Characters.Handling;           use Ada.Characters.Handling;
with Ada.Directories;                   use Ada.Directories;
with Ada.Float_Text_IO;                 use Ada.Float_Text_IO;
with Ada.Integer_Text_IO;               use Ada.Integer_Text_IO;
with Ada.IO_Exceptions;
with Ada.Numerics.Discrete_Random;
with Ada.Numerics.Elementary_Functions;
with Ada.Numerics.Float_Random;
with Ada.Streams.Stream_IO;             use Ada.Streams.Stream_IO;
with Ada.Strings.Fixed;                 use Ada.Strings.Fixed, Ada.Strings;
with Ada.Strings.Unbounded;             use Ada.Strings.Unbounded;
with Ada.Text_IO;                       use Ada.Text_IO;
with Ada.Unchecked_Deallocation;

with Dual_IO;

with Interfaces;                        use Interfaces;

with GNAT.OS_Lib;

package body Rezip_lib is

  function S (Source : Ada.Strings.Unbounded.Unbounded_String) return String
    renames Ada.Strings.Unbounded.To_String;
  function U (Source : String) return Ada.Strings.Unbounded.Unbounded_String
    renames Ada.Strings.Unbounded.To_Unbounded_String;

  --  This might be better read from a config file...
  --
  type Zipper_specification is record
    name, title, URL, options : Unbounded_String;
    expanded_options    : Unbounded_String;
    --  options with dynamically expanded tokens
    made_by_version     : Unsigned_16;
    pkzm                : Zip.PKZip_method;
    limit               : Zip.Zip_64_Data_Size_Type;
    --  Compression is considered too slow or unefficient beyond limit
    --  E.g., kzip's algorithm might be O(N^2) or worse; on large files,
    --   deflate_e or other methods are better anyway
    randomized          : Boolean;
  end record;

  NN : constant Unbounded_String := Null_Unbounded_String;

  --  Give up recompression above a certain data size for some external packers like KZip
  --  or Zopfli.
  --
  kzip_zopfli_limit : constant := 2_000_000;

  type Approach is (
    original,
    shrink,
    reduce_4,
    deflate_3,
    deflate_r,
    lzma_2, lzma_3,
    presel_1, presel_2,
    external_1, external_2, external_3, external_4,
    external_5, external_6, external_7, external_8,
    external_9, external_10, external_11, external_12,
    external_13
  );

  subtype Internal is Approach
    range Approach'Succ (Approach'First) .. Approach'Pred (external_1);
  subtype External is Approach
    range external_1 .. Approach'Last;

  ext : array (External) of Zipper_specification :=
    ( --  Zip 2.32 or later:
      (U ("zip"), U ("Zip"), U ("http://info-zip.org/"),
         U ("-9"), NN, 20, Zip.deflate, 0, False),
      --  7-Zip 4.64 or later; Deflate:
      (U ("7z"),
         U ("7-Zip"), U ("http://7-zip.org/"),
         U ("a -tzip -mm=deflate -mfb=258 -mpass=#RAND#(7,15) -mmc=10000"),
         NN, 20, Zip.deflate, 0, True),
      (U ("7z"),
         U ("7-Zip"), NN,
         U ("a -tzip -mm=deflate64 -mfb=257 -mpass=15 -mmc=10000"),
         NN, 21, Zip.deflate_e, 0, False),
      --  KZip:
      (U ("kzip"), U ("KZIP"), U ("http://www.advsys.net/ken/utils.htm"),
         U ("/rn /b0"), NN, 20, Zip.deflate, kzip_zopfli_limit, True),
      (U ("kzip"), U ("KZIP"), NN,
         U ("/rn /b#RAND_EXP#(1,2048)"), NN, 20, Zip.deflate, kzip_zopfli_limit, True),
      --  Zip 3.0 or later; BZip2:
      (U ("zip"), U ("Zip"), NN,
         U ("-#RAND#(1,9) -Z bzip2"), NN, 46, Zip.bzip2, 0, True),
      --  7z:
      (U ("7z"), U ("7-Zip"), NN,
         U ("a -tzip -mm=BZip2:d=#RAND#(1,9)00k:pass=7"), NN, 46, Zip.bzip2, 0, True),
      --  7-Zip 9.20 or later; LZMA:
      (U ("7z"), U ("7-Zip"), NN,
         U ("a -tzip -mm=LZMA -mx=9"), NN, 63, Zip.lzma_meth, 0, False),
      (U ("7z"), U ("7-Zip"), NN, --  LZ77: BT3 or BT4, dictionary size 2**19 = 512 KiB
         U ("a -tzip -mm=LZMA:a=2:d=19:mf=bt#RAND#(3,5):fb=273:lc=0:lp=2"), NN, 63, Zip.lzma_meth, 0, False),
      (U ("7z"), U ("7-Zip"), NN, --  LZ77: BT3 or BT4, dictionary size 2**25 = 32 MiB
         U ("a -tzip -mm=LZMA:a=2:d=25:mf=bt#RAND#(3,5):fb=273:lc=7"), NN, 63, Zip.lzma_meth, 0, False),
      (U ("7z"), U ("7-Zip"), NN, --  LZ77: BT3 or BT4, dictionary size 2**26 = 64 MiB
         U ("a -tzip -mm=LZMA:a=2:d=26:mf=bt#RAND#(3,5):fb=273:lc=8:lp0:pb1"), NN, 63, Zip.lzma_meth, 0, False),
      (U ("7z"), U ("7-Zip"), NN, --  Super-randomized version
         U ("a -tzip -mm=LZMA:a=2:d=#RAND_EXP#(1,65535)k:mf=bt#RAND#(2,5):fb=#RAND#(128,273):lc=#RAND#(0,8):lp#RAND#(0,4):pb#RAND#(0,4)"),
         NN, 63, Zip.lzma_meth, 0, True),
      --  AdvZip: advancecomp v1.19+ interesting for the Zopfli algorithm
      (U ("advzip"), U ("AdvZip"), U ("http://advancemame.sf.net/comp-readme.html"),
         U ("-a -4"), NN, 20, Zip.deflate, kzip_zopfli_limit, False)
    );

  defl_opt : constant Zipper_specification :=
    (U ("deflopt"), U ("DeflOpt"), U ("http://www.walbeehm.com/download/"),
     NN, NN, 0, Zip.deflate, 0, False);

  procedure Rezip (
    from_zip_file      : String;
    to_zip_file        : String;
    format_choice      : Zip_format_set := all_formats;  --  force output into selected format set
    touch              : Boolean        := False;        --  set time stamps to now
    lower              : Boolean        := False;        --  set full file names to lower case
    delete_comment     : Boolean        := False;        --  delete zip comment
    randomized_stable  : Positive       := 1;
    log_file           : String         := "";
    html_report        : String         := "";
    alt_tmp_file_radix : String         := "";           --  e.g. "X:\temp\rz_"
    internal_only      : Boolean        := False         --  Zip-Ada algorithms only, no ext. call
  )
  is

    package DFIO is new Dual_IO.Float_IO (Float);

    procedure Rip_data (
      archive      : Zip.Zip_info; -- from this archive...
      input        : in out Root_Zipstream_Type'Class;
      data_name    : String;       -- extract this data
      rip_rename   : String;       -- to this file (compressed)
      unzip_rename : String;       -- and this one (uncompressed)
      header       : out Zip.Headers.Local_File_Header
    )
    is
      file_index     : Zip_Streams.ZS_Index_Type;
      comp_size      : Zip.Zip_64_Data_Size_Type;
      uncomp_size    : Zip.Zip_64_Data_Size_Type;
      file_out       : Ada.Streams.Stream_IO.File_Type;
      dummy_encoding : Zip.Zip_name_encoding;
      dummy_crc      : Unsigned_32;
      mem            : Zip_Streams.ZS_Index_Type;
      head_extra     : Zip.Headers.Local_File_Header_Extension;
      dummy_offset   : Unsigned_64 := 0;  --  Initialized for avoiding random value = 16#FFFF_FFFF#
      use UnZip;
    begin
      Zip.Find_offset (
        info           => archive,
        name           => data_name,
        name_encoding  => dummy_encoding,
        file_index     => file_index,
        comp_size      => comp_size,
        uncomp_size    => uncomp_size,
        crc_32         => dummy_crc
      );
      Set_Index (input, file_index);
      Zip.Headers.Read_and_check (input, header);
      --  Skip name
      Set_Index (input,
        Index (input) + Zip_Streams.ZS_Size_Type (header.filename_length)
      );
      mem := Index (input);
      if header.extra_field_length >= 4 then
        Zip.Headers.Read_and_check (input, head_extra);
        Zip.Headers.Interpret
          (head_extra,
           header.dd.uncompressed_size,
           header.dd.compressed_size,
           dummy_offset);
      end if;
      --  Skip extra field
      Set_Index (input, mem + Zip_Streams.ZS_Size_Type (header.extra_field_length));
      --  * Get the data, compressed
      Ada.Streams.Stream_IO.Create (file_out, Out_File, rip_rename);
      Zip.Copy_chunk (input, Stream (file_out).all, Integer (comp_size));
      Close (file_out);
      if unzip_rename /= "" then
        --  * Get the data, uncompressed
        Extract (
          from    => archive,
          what    => data_name,
          rename  => unzip_rename,
          options =>
             (test_only => False,
              junk_directories => False,
              case_sensitive_match => True,
              extract_as_text => False
          )
        );
      end if;
    end Rip_data;

    Approach_to_Method : constant array (Internal) of Zip.Compress.Compression_Method :=
      (shrink    => Zip.Compress.Shrink,
       reduce_4  => Zip.Compress.Reduce_4,
       deflate_3 => Zip.Compress.Deflate_3,
       deflate_r => Zip.Compress.Deflate_R,
       lzma_2    => Zip.Compress.LZMA_2,
       lzma_3    => Zip.Compress.LZMA_3,
       presel_1  => Zip.Compress.Preselection_1,
       presel_2  => Zip.Compress.Preselection_2
      );

    type Packer_info is record
      size             : Zip.Zip_64_Data_Size_Type;
      zfm              : Unsigned_16;
      count            : Natural;
      saved            : Integer_64;  --  Number of bytes saved by chosen method
      --  NB: can be negative if -defl chosen: suboptimal recompression,
      --      but compatible method.
      saved_ex_aequo   : Integer_64;  --  Number of bytes saved if method is as good as
                                      --  the winning method.
      uncomp_size      : Unsigned_64;
      --  summed uncompressed sizes might be more than 2**32
      expanded_options : Unbounded_String;
      iter             : Positive; -- iterations needed
      LZMA_EOS         : Boolean;
    end record;

    type Packer_info_array is array (Approach) of Packer_info;

    type Dir_entry;
    type p_Dir_entry is access Dir_entry;
    --
    type Dir_entry is record
      head : Zip.Headers.Central_File_Header;
      name : Unbounded_String;
      next : p_Dir_entry := null;
      chosen_approach : Approach := original;
      info : Packer_info_array;
    end record;

    function Radix return String is
    begin
      if alt_tmp_file_radix = "" then
        return Flexible_temp_files.Radix;
      else
        return alt_tmp_file_radix;
      end if;
    end Radix;

    function Temp_name (
      is_compressed : Boolean;
      appr          : Approach
    )
      return String
    is
      initial : constant array (Boolean) of Character := ('u', 'c');
    begin
      return
        Radix &
        "_!" & initial (is_compressed) &
        '!' & Trim (Integer'Image (Approach'Pos (appr)), Left) &
        "!_.tmp";
    end Temp_name;

    function Img (a : Approach; html : Boolean) return String is
      function Repl (s : String) return String is
        t : String := s;
      begin
        for i in t'Range loop
          if html and t (i) = ':' then t (i) := ' '; end if;  --  Break too long texts within a cell.
        end loop;
        return t;
      end Repl;
    begin
      if a in External then
        return "External: " & S (ext (a).title) & ", " & Repl (S (ext (a).expanded_options));
      else
        declare
          s : constant String := Approach'Image (a);
        begin
          return s (s'First) & To_Lower (s (s'First + 1 .. s'Last) & (Approach'Width - s'Length + 1) * ' ');
        end;
      end if;
    end Img;

    --  From AZip_Common...
    function Image_1000 (r : Zip.Zip_64_Data_Size_Type; separator : Character := ''') return String is
      s : constant String := Zip.Zip_64_Data_Size_Type'Image (r);
      t : String (s'First .. s'First + (s'Length * 4) / 3);
      j, c : Natural;
    begin
      --  For signed integers
      --  if r < 0 then
      --    return '-' & Image_1000(abs r, separator);
      --  end if;
      --
      --  We build result string t from right to left
      j := t'Last + 1;
      c := 0;
      for i in reverse s'First .. s'Last loop
        exit when s (i) = ' ' or s (i) = '-';
        if c > 0 and then c mod 3 = 0 then
          j := j - 1;
          t (j) := separator;
        end if;
        j := j - 1;
        t (j) := s (i);
        c := c + 1;
      end loop;
      return t (j .. t'Last);
    end Image_1000;

    function Image_1000 (r : Integer_64; separator : Character := ''') return String is
      s : constant String := Integer_64'Image (r);
      t : String (s'First .. s'First + (s'Length * 4) / 3);
      j, c : Natural;
    begin
      --  For signed integers
      if r < 0 then
        return '-' & Image_1000 (abs r, separator);
      end if;
      --  We build result string t from right to left
      j := t'Last + 1;
      c := 0;
      for i in reverse s'First .. s'Last loop
        exit when s (i) = ' ' or s (i) = '-';
        if c > 0 and then c mod 3 = 0 then
          j := j - 1;
          t (j) := separator;
        end if;
        j := j - 1;
        t (j) := s (i);
        c := c + 1;
      end loop;
      return t (j .. t'Last);
    end Image_1000;

    procedure Call_external (
      packer : String;
      args   : String
    )
    is
      use GNAT.OS_Lib;
      procedure Dispose is
        new Ada.Unchecked_Deallocation (Argument_List, Argument_List_Access);
      list : Argument_List_Access;
      ok : Boolean;
    begin
      Dual_IO.Put_Line (packer & " [" & args & ']');
      list := Argument_String_To_List (args);
      GNAT.OS_Lib.Spawn (packer, list.all, ok);
      Dispose (list);
      if not ok then
        Dual_IO.Put_Line (
          "ReZip warning: cannot call " & packer &
          ", or it has returned an error. Is it callable through the ""path"" ?"
        );
      end if;
    end Call_external;

    seed_iterator : Natural;

    procedure Call_external_expanded (
      packer     :        String;
      options    :        String;
      other_args :        String;
      expand     : in out Unbounded_String  --  expanded arguments
    )
    is
      type Token is (rand, rand_exp);
    begin
      expand := U (options);
      for t in Token loop
        --  Replace all tokens:  #<t>#(a,b)
        loop
          declare
            tok : constant String := '#' & Token'Image (t) & '#';
            idx : constant Natural := Index (expand, tok);
            par : Natural;
            replace_by : Unbounded_String;
          begin
            --  put_line("Token: " & Token'Image(t) & "   " & S(expand));
            exit when idx = 0;  --  No more of token t to replace
            par := Index (expand, ")", idx);
            declare
              opt : constant String := S (expand);  --  partially processed option string
              curr : constant String := opt (idx + 1 .. opt'Last);  --  current option
              par_a : constant Natural := Index (curr, "(");
              par_z : constant Natural := Index (curr, ")");
              comma : constant Natural := Index (curr, ",");
              n1, n2, n : Integer;
            begin
              n1 := Integer'Value (curr (par_a + 1 .. comma - 1));
              n2 := Integer'Value (curr (comma + 1 .. par_z - 1));
              case t is
                when rand =>
                  --  Replace #RAND#(n1,n2) by a number between n1 and n2.
                  --  Uniform distribution: U(n1,n2).
                  declare
                    subtype rng is Integer range n1 .. n2;
                    package Rnd is new Ada.Numerics.Discrete_Random (rng);
                    gen : Rnd.Generator;
                  begin
                    Rnd.Reset (gen, seed_iterator);  --  seed_iterator is itself randomized.
                    seed_iterator := seed_iterator + 1;
                    n := Rnd.Random (gen);
                  end;
                  replace_by := U (Trim (Integer'Image (n), Left));
                when rand_exp =>
                  --  Replace #RAND_EXP#(n1,n2) by a number between n1 and n2.
                  --  Strong bias towards small numbers (rather close to n1 than to n2).
                  --
                  --  Example (k=1, n1=1, n2=100): P(X in [1;10]) = 1/2; P(X in [10;100]) = 1/2.
                  --
                  --  The CDF is:  F(x) = ((log x - log n1) / (log n2 - log n1)) ^ (1/k).
                  --
                  declare
                    use Ada.Numerics.Float_Random, Ada.Numerics.Elementary_Functions;
                    gen : Generator;
                    l1, l2, l, u : Float;
                    k : constant := 2;
                  begin
                    Reset (gen, seed_iterator);  --  seed_iterator is itself randomized.
                    seed_iterator := seed_iterator + 1;
                    u := Random (gen);  --  u is Uniform in [0;1]
                    l1 := Log (Float (n1));
                    l2 := Log (Float (n2));
                    l := l1 + (l2 - l1) * (u ** k);
                    n := Integer (Exp (l));
                  end;
                  replace_by := U (Trim (Integer'Image (n), Left));
              end case;
              Replace_Slice (expand, idx, par, S (replace_by));
            end;
          end;
        end loop;
      end loop;
      Call_external (packer, S (expand) & ' ' & other_args);
    end Call_external_expanded;

    function Temp_Zip_Name return String is
    begin
      return Simple_Name (Radix) & "_$temp$.zip";
    end Temp_Zip_Name;

    procedure Try_deleting_Temp_Zip_File is
    begin
      if Exists (Temp_Zip_Name) then
        Delete_File (Temp_Zip_Name);
      end if;
    exception
      when Ada.IO_Exceptions.Use_Error =>
        null;
    end Try_deleting_Temp_Zip_File;

    procedure Process_External (
      packer     : String;
      options    : String;
      out_name   : String;
      is_rand    : Boolean;
      is_deflate : Boolean;
      info       : out Packer_info
    )
    is
      rand_winner : constant String := Simple_Name (Radix) & "_$rand$.tmp";
      options_winner : Unbounded_String;
      data_name : constant String := Simple_Name (Temp_name (False, original));
      header : Zip.Headers.Local_File_Header;
      MyStream   : aliased File_Zipstream;
      cur_dir : constant String := Current_Directory;
      size_memory : array (1 .. randomized_stable) of Zip.Zip_64_Data_Size_Type := (others => 0);
      current_size : Zip.Zip_64_Data_Size_Type := 0;
      zfm : Unsigned_16;
      attempt : Positive := 1;
      dummy_exp_opt : Unbounded_String;
      zi_ext : Zip.Zip_info;
    begin
      --  We jump into the TEMP directory, to avoid putting pathes into the
      --  temporary zip file.
      Set_Directory (Containing_Directory (Radix));
      loop
        Try_deleting_Temp_Zip_File;  --  remove (eventually broken) zip
        Call_external_expanded (
          packer,
          options,
          Temp_Zip_Name & ' ' & data_name,
          info.expanded_options
        );
        if (not Exists (Temp_Zip_Name)) and then Ada.Directories.Size (data_name) = 0 then
          --  ADVZip 1.19 doesn't create a zip file for a 0-size entry; we call Zip instead...
          Call_external_expanded ("zip", "", Temp_Zip_Name & ' ' & data_name, dummy_exp_opt);
        end if;
        if is_deflate then
          --  Post processing of "deflated" entry with DeflOpt:
          Call_external (S (defl_opt.name), Temp_Zip_Name);
        end if;
        --  Now, rip
        Set_Name (MyStream, Temp_Zip_Name);
        Open (MyStream, In_File);
        Zip.Load (zi_ext, MyStream, True);
        Rip_data (
          archive      => zi_ext,
          input        => MyStream,
          data_name    => data_name,
          rip_rename   => out_name,
          unzip_rename => "",
          header       => header
        );
        Close (MyStream);
        Try_deleting_Temp_Zip_File;
        --
        if randomized_stable = 1 or not is_rand then  --  normal behaviour (1 attempt)
          current_size := header.dd.compressed_size;
          zfm := header.zip_type;
          info.iter := 1;
          exit;
        end if;
        --
        --  Here, we process the cases where compressed sizes need
        --  to be reduced and we expect a stable size over n=randomized_stable
        --  attempts.
        --
        if attempt = 1 or else
          header.dd.compressed_size < current_size  --  better size
        then
          current_size := header.dd.compressed_size;
          zfm := header.zip_type;
          if Exists (rand_winner) then
            Delete_File (rand_winner);
          end if;
          Rename (out_name, rand_winner);
          options_winner := info.expanded_options;
        end if;
        --
        --  Manage the array of last n=randomized_stable sizes
        --
        if attempt > size_memory'Last then
          for i in size_memory'First + 1 .. size_memory'Last loop
            size_memory (i - 1) := size_memory (i);
          end loop;
          size_memory (size_memory'Last) := current_size;
        else
          size_memory (attempt) := current_size;
        end if;
        --
        --  Check stability over n=randomized_stable attempts
        --
        if attempt >= randomized_stable then
          if size_memory (randomized_stable) = size_memory (1) then
            if Exists (out_name) then
              Delete_File (out_name);
            end if;
            Rename (rand_winner, out_name);
            info.expanded_options := options_winner;
            info.iter := attempt;
            exit;
          end if;
        end if;
        attempt := attempt + 1;
      end loop;
      info.size        := current_size;
      info.uncomp_size := header.dd.uncompressed_size;
      --  uncomp_size should not matter (always the same).
      info.zfm        := zfm;
      info.LZMA_EOS   := (zfm = 14) and (header.bit_flag and Zip.Headers.LZMA_EOS_Flag_Bit) /= 0;
      --  We jump back to the startup directory.
      Set_Directory (cur_dir);
    end Process_External;

    --  Compress data as raw compressed data
    procedure Process_Internal_Raw (a : Approach; e : in out Dir_entry) is
      File_in       : aliased File_Zipstream;
      File_out      : aliased File_Zipstream;
    begin
      Set_Name (File_in, Temp_name (False, original));
      Open (File_in, In_File);
      Set_Name (File_out, Temp_name (True, a));
      Create (File_out, Out_File);
      Zip.Compress.Compress_data
      (
        input            => File_in,
        output           => File_out,
        input_size_known => True,
        input_size       => e.head.short_info.dd.uncompressed_size,
        method           => Approach_to_Method (a),
        feedback         => Zip_Console_IO.My_feedback'Access,
        password         => "",
        content_hint     => Zip.Compress.Guess_type_from_name (S (e.name)),
        CRC              => e.head.short_info.dd.crc_32,
        --  we take the occasion to compute the CRC if not
        --  yet available (e.g. JAR)
        output_size      => e.info (a).size,
        zip_type         => e.info (a).zfm
      );
      e.info (a).LZMA_EOS := e.info (a).zfm = 14;
      Close (File_in);
      Close (File_out);
    end Process_Internal_Raw;

    --  Compress data as a Zip archive (like external methods), then call post-processing
    procedure Process_Internal_as_Zip (a : Approach; e : in out Dir_entry) is
      zip_file : aliased File_Zipstream;
      archive : Zip_Create_Info;
      data_name : constant String := Simple_Name (Temp_name (False, original));
      zi_ext : Zip.Zip_info;
      header : Zip.Headers.Local_File_Header;
      MyStream   : aliased File_Zipstream;
      cur_dir : constant String := Current_Directory;
    begin
      Set_Directory (Containing_Directory (Radix));
      Create_Archive (archive, zip_file'Unchecked_Access, Temp_Zip_Name);
      Set (archive, Approach_to_Method (a));
      Add_File (archive, data_name);
      Finish (archive);
      --  Post processing of "deflated" entry with DeflOpt:
      Call_external (S (defl_opt.name), Temp_Zip_Name);
      --  Now, rip
      Set_Name (MyStream, Temp_Zip_Name);
      Open (MyStream, In_File);
      Zip.Load (zi_ext, MyStream, True);
      Rip_data (
        archive      => zi_ext,
        input        => MyStream,
        data_name    => data_name,
        rip_rename   => Temp_name (True, a),
        unzip_rename => "",
        header       => header
      );
      e.info (a).size := header.dd.compressed_size;
      e.info (a).zfm := header.zip_type;
      e.info (a).LZMA_EOS :=
        (header.zip_type = 14) and (header.bit_flag and Zip.Headers.LZMA_EOS_Flag_Bit) /= 0;
      Close (MyStream);
      Try_deleting_Temp_Zip_File;
      Set_Directory (cur_dir);
    end Process_Internal_as_Zip;

    time_0 : constant Ada.Calendar.Time := Clock;

    procedure Repack_contents (orig_name, repacked_name, html_report_name : String)
    is
      zi : Zip.Zip_info;
      MyStream   : aliased File_Zipstream;

      list, e, curr : p_Dir_entry := null;
      repacked_zip_file   : aliased File_Zipstream;
      null_packer_info : constant Packer_info := (0, 0, 0, 0, 0, 0, NN, 1, False);
      total : Packer_info_array := (others => null_packer_info);
      --  total(a).count counts the files where approach 'a' was optimal
      --  total(a).saved counts the saved bytes when approach 'a' was optimal
      total_choice : Packer_info := null_packer_info;
      summary : Ada.Text_IO.File_Type;
      T0, T1 : Ada.Calendar.Time;
      repack_duration : Duration;
      --
      type Approach_Filtering is array (Approach) of Boolean;
      consider_a_priori : Approach_Filtering;
      --
      lightred : constant String := "#f43048";

      procedure Process_one (unique_name : String) is
        comp_size   :  Zip.Zip_64_Data_Size_Type;
        uncomp_size :  Zip.Zip_64_Data_Size_Type;
        choice : Approach := original;
        deco : constant String := "-->-->-->" & (20 + unique_name'Length) * '-';
        mth : Zip.PKZip_method;
        consider : Approach_Filtering;
        gain, gain_a : Integer_64;
        --
        procedure Winner_color is
        begin
          if e.info (choice).size < e.info (original).size then
            Put (summary, "<td bgcolor=lightgreen><b>");
            --  We were able to reduce the size. :-)
          elsif e.info (choice).size = e.info (original).size then
            Put (summary, "<td><b>");
            --  Original was already the best.
          else
            Put (summary, "<td bgcolor=" & lightred & "><b>");
            --  Forced to a format with a less efficient compression. :-(
          end if;
        end Winner_color;
        --
        use Zip;
        needs_zip64 : Boolean;
        fh_extra : Zip.Headers.Local_File_Header_Extension;
      begin
        --  Start with the set of approaches that has been decided for all entries.
        consider := consider_a_priori;
        if unique_name = "" or else
             (unique_name (unique_name'Last) = '\'
           or unique_name (unique_name'Last) = '/'
          )
        then
          return; -- directories are useless entries!
        end if;
        total_choice.count := total_choice.count + 1;
        Dual_IO.Close_and_Append_Log; -- have an up to date copy on file system
        Dual_IO.Put_Line (deco);
        Dual_IO.Put_Line (
          ' ' &
          Integer'Image ((100 * total_choice.count) / Zip.Entries (zi)) &
          "% - Processing " &
          unique_name & ',' &
          Integer'Image (total_choice.count) &
          " of" &
          Integer'Image (Zip.Entries (zi))
        );
        Dual_IO.Put_Line (deco);
        Dual_IO.New_Line;
        --
        e := new Dir_entry;
        if curr = null then
          curr := e;
          list := e;
        else
          curr.next := e;
          curr := e;
        end if;
        e.name := U (unique_name);
        e.head.made_by_version     := 20; -- version 2.0
        e.head.comment_length      := 0;
        e.head.disk_number_start   := 0;
        e.head.internal_attributes := 0; -- 0: seems binary; 1, text
        e.head.external_attributes := 0;
        --
        Dual_IO.Put ("    Phase 1:  dump & unzip -");
        Rip_data (
          archive      => zi,
          input        => MyStream,
          data_name    => unique_name,
          rip_rename   => Temp_name (True, original),
          unzip_rename => Temp_name (False, original),
          header       => e.head.short_info
        );
        --
        if touch then
          e.head.short_info.file_timedate := Zip.Convert (time_0);
        end if;
        if lower then
          e.name := U (To_Lower (S (e.name)));
        end if;
        --  Get reliable data from zi
        Zip.Get_sizes (
          info           => zi,
          name           => unique_name,
          comp_size      => comp_size,
          uncomp_size    => uncomp_size
        );
        Dual_IO.Put_Line (" done");
        --
        --  Apply limitations: skip some methods if certain conditions are met.
        --  For instance:
        --    Shrink may in rare cases be better, but only for tiny files.
        --    KZip and Zopfli are excellent but really too slow on large files.
        --
        for a in Approach loop
          case a is
            when original =>
              null;
            when shrink =>
              consider (a) := consider (a) and uncomp_size <= 6000;
            when reduce_4 =>
              consider (a) := consider (a) and uncomp_size <= 9000;
            when External =>
              consider (a) := consider (a) and (ext (a).limit = 0 or uncomp_size <= ext (a).limit);
            when others =>
              null;
          end case;
        end loop;
        Dual_IO.Put_Line ("    Phase 2:  try different tactics...");
        --
        Try_all_approaches :
        --
        for a in Approach loop
          if consider (a) then
            Dual_IO.Put ("              -o-> " & Img (a, html => False));
            e.info (a).iter := 1;
            case a is
              --
              when original =>
                --  This is from the original .zip - just record size and method
                e.info (a).size := comp_size;
                e.info (a).zfm  := e.head.short_info.zip_type;
                e.info (a).LZMA_EOS :=
                  (e.info (a).zfm = 14) and
                  (e.head.short_info.bit_flag and Zip.Headers.LZMA_EOS_Flag_Bit) /= 0;
                mth := Zip.Method_from_code (e.info (a).zfm);
                --
              when Internal =>
                if Approach_to_Method (a) in Zip.Compress.Deflation_Method
                  and not internal_only
                then
                  --  We will post-process our internal Deflate with DeflOpt.
                  Process_Internal_as_Zip (a, e.all);
                else
                  Process_Internal_Raw (a, e.all);
                end if;
              when External =>
                Dual_IO.New_Line;
                Process_External (
                  S (ext (a).name),
                  S (ext (a).options),
                  Temp_name (True, a),
                  ext (a).randomized,
                  ext (a).pkzm = Zip.deflate,
                  e.info (a)
                );
                e.head.made_by_version := ext (a).made_by_version;
                ext (a).expanded_options := e.info (a).expanded_options;
                --
            end case;
            total (a).size := total (a).size + e.info (a).size;
            if e.info (a).size < e.info (choice).size then
              --  Hurra, we found a smaller size than previous choice!
              choice := a;
            end if;
            if choice = original and not format_choice (mth) then
              --  This occurs if we want to make an archive with only a certain set of formats,
              --  for instance deflate_or_store, which is the most compatible.
              --  Since approach _a_ uses a format in the desired set, the choice will be
              --  forced out of original, even with a worse size.
              choice := a;
            end if;
            Dual_IO.New_Line;
          end if;
        end loop Try_all_approaches;
        --
        total_choice.size := total_choice.size + e.info (choice).size;
        total (choice).count := total (choice).count + 1;
        total_choice.uncomp_size :=
          total_choice.uncomp_size + Unsigned_64 (uncomp_size);
        gain := Integer_64 (e.info (original).size) - Integer_64 (e.info (choice).size);
        total (choice).saved := total (choice).saved + gain;
        --  We award now the ex-aequo's. Caution: multiple counting if you take the sum of totals
        --  over all approachs, but it is good for knowing the strength of an individual approach.
        for a in Approach loop
          if consider (a) then
            gain_a := Integer_64 (e.info (original).size) - Integer_64 (e.info (a).size);
            if gain_a = gain then
              total (a).saved_ex_aequo := total (a).saved_ex_aequo + gain;
            end if;
          end if;
        end loop;
        total_choice.saved := total_choice.saved + gain;
        --
        Dual_IO.New_Line;
        Dual_IO.Put (
          "    Phase 3:  Winner is " & Img (choice, html => False) &
          "; gain in bytes:" & Integer_64'Image (gain) &
          "; writing data -"
        );
        --  * Summary outputs
        Put (summary,
          "<tr><td>" &
          Trim (Integer'Image (total_choice.count), Left) &
          --  '/' &
          --  Trim(Integer'Image(Zip.Entries(zi)),Left) &
          "</td>" &
          "<td bgcolor=lightgrey><tt>" & unique_name & "</tt>, " &
          Image_1000 (uncomp_size) & "</td>");
        for a in Approach loop
          if consider_a_priori (a) then
            if not consider (a) then
              Put (summary, "<td bgcolor=lightgray>skipped");
            elsif a = choice then
              Winner_color;
            elsif e.info (a).size = e.info (choice).size then -- ex aequo
              Put (summary, "<td bgcolor=lightblue><b>");
            else
              Put (summary, "<td>");
            end if;
            if consider (a) then
              Put (summary, Image_1000 (e.info (a).size));
            end if;
            if choice = a then
              Put (summary, "</b>");
            end if;
            Put (summary, "</td>");
          end if;
        end loop;
        --  Recall winner approach, method and size:
        Put (summary, "<td>" & Img (choice, html => True) & "</td>");
        Put (summary,
          "<td bgcolor=#fafa64>" &
          Zip.Image (Zip.Method_from_code (e.info (choice).zfm)) &
          "</td>"
        );
        Put (summary,
          "<td>" &
          Zip.Image (Zip.Method_from_code (e.info (original).zfm)) &
          "</td>"
        );
        Winner_color;
        Put (summary, Image_1000 (e.info (choice).size));
        Put (summary, "</b></td><td>");
        if e.info (original).size > 0 then
          Put (
            summary,
            100.0 * Float (e.info (choice).size) / Float (e.info (original).size),
            3, 2, 0
          );
          Put (summary, "%");
        end if;
        Put (summary, "</td><td>");
        if uncomp_size > 0 then
          Put (
            summary,
            100.0 * Float (e.info (choice).size) / Float (uncomp_size),
            3, 2, 0
          );
          Put (summary, "%");
        end if;
        Put (summary, "</td><td>");
        Put (summary, Image_1000 (uncomp_size));
        Put (summary, "</td><td>");
        Put (summary, Integer'Image (e.info (choice).iter));
        Put_Line (summary, "</td></tr>");
        --
        --  Write winning data:
        --
        e.head.short_info.extra_field_length := 0;  --  We choose to ignore it...
        --  No data descriptor after data (bit 3); no EOS for LZMA (bit 1):
        e.head.short_info.bit_flag :=
          e.head.short_info.bit_flag and (2#1111_1111_1111_0101#);
        --  Set the LZMA EOS flag if present in winner entry (checked by 7-Zip v.17.01):
        if e.info (choice).LZMA_EOS then
          e.head.short_info.bit_flag := e.head.short_info.bit_flag or Zip.Headers.LZMA_EOS_Flag_Bit;
        end if;
        --  Set or adjust the pre-data data descriptor:
        --  NB: even if missing pre-data, CRC will have been computed
        --     at least with one internal method
        e.head.short_info.dd.uncompressed_size := uncomp_size;
        --  Put the winning size and method
        e.head.short_info.dd.compressed_size := e.info (choice).size;
        e.head.short_info.zip_type := e.info (choice).zfm;
        e.head.local_header_offset := Unsigned_64 (Index (repacked_zip_file)) - 1;
        needs_zip64 :=
          Zip.Headers.Needs_Local_Zip_64_Header_Extension
            (e.head.short_info, e.head.local_header_offset);
        Zip.Headers.Write
          (repacked_zip_file, e.head.short_info,
           (if needs_zip64 then Zip.Headers.force_zip_64 else Zip.Headers.force_empty));
        String'Write (repacked_zip_file'Access, S (e.name));
        if needs_zip64 then
          fh_extra.tag  := 1;
          fh_extra.size := Zip.Headers.local_header_extension_short_length - 4;
          fh_extra.value_64 (1) := e.head.short_info.dd.uncompressed_size;
          fh_extra.value_64 (2) := e.head.short_info.dd.compressed_size;
          fh_extra.value_64 (3) := e.head.local_header_offset;  --  Not actually written.
          Zip.Headers.Write (repacked_zip_file, fh_extra, True);
        end if;
        --  Copy the compressed data
        Zip.Copy_file (Temp_name (True, choice), repacked_zip_file);
        Dual_IO.Put_Line (" done");
        Dual_IO.New_Line;
      end Process_one;

      procedure Process_all is new Zip.Traverse (Process_one);

      ed : Zip.Headers.End_of_Central_Dir;

      function Webcolor (a : Approach) return String is
        v : Float;
        sr, sg, sb : String (1 .. 10);
      begin
        if total_choice.saved > 0 and
          --  with options like -defl ot -fast_dec, we may have
          --  negative values or other strange things:
           total (a).saved >= 0
        then
          v := Float (total (a).saved) / Float (total_choice.saved);
          --   ^ contribution of approach 'a'
        else
          v := 0.0;
        end if;
        Put (sr, 512 + Integer (144.0 + 111.0 * (1.0 - v)), 16);
        sb := sr;
        Put (sg, 512 + Integer (238.0 + 17.0 * (1.0 - v)), 16);
        return
          sr (sr'Last - 2 .. sr'Last - 1) &
          sg (sg'Last - 2 .. sg'Last - 1) &
          sb (sb'Last - 2 .. sb'Last - 1);
      end Webcolor;

      meth : Zip.Compress.Compression_Method;

    begin  --  Repack_contents
      T0 := Clock;
      for a in Approach loop
        case a is
          when original =>
            consider_a_priori (a) := True;
          when Internal =>
            meth := Approach_to_Method (a);
            case meth is
              when Zip.Compress.Single_Method =>
                consider_a_priori (a) := format_choice (Zip.Compress.Method_to_Format (meth));
              when Zip.Compress.Multi_Method =>
                --  For the sake of simplicity, we consider the Multi_Method's
                --  only when all formats are admitted.
                consider_a_priori (a) := format_choice = all_formats;
            end case;
          when External =>
            consider_a_priori (a) := format_choice (ext (a).pkzm) and not internal_only;
        end case;
      end loop;
      Set_Name (MyStream, orig_name);
      Open (MyStream, In_File);
      Zip.Load (zi, MyStream, True);

      Set_Name (repacked_zip_file, repacked_name);
      Create (repacked_zip_file, Out_File);
      Create (summary, Out_File, html_report_name);
      --
      --  HTML Report begins here.
      --
      Put_Line (summary,
        "<html><head><title>ReZip summary for file "
         & orig_name & "</title></head>"
      );
      Put_Line (summary, "<style>.container { overflow-y: auto; height: 87%; }");
      Put_Line (summary, "td_approach { width:115px; }");
      Put_Line (summary, "</style><body>");
      Put_Line (summary, "<font face=""Calibri, Arial, Tahoma""> <!-- Set font for the whole page !-->");
      Put_Line (summary,
        "<h2><a target=_blank href=" & Zip.web &
        ">ReZip</a> summary for file " & orig_name & "</h2>"
      );
      Put_Line (summary,
        "ReZip - Zip-Ada Library version " & Zip.version & " dated " & Zip.reference
      );
      if format_choice /= all_formats then
        Put_Line (summary,
          "<br><table border=0 cellpadding=0 cellspacing=0>" &
          "<tr bgcolor=" & lightred &
          "><td><b>An option that filters methods is on, " &
          "result(s) may be sub-optimal - see details at bottom.</b></td></tr></table><br>"
        );
      end if;
      Put_Line (summary, "<div class=""container""><table border=1 cellpadding=1 cellspacing=1>");
      Put (summary,
        "<tr bgcolor=lightyellow><td></td>" &
        "<td align=right valign=top><b>Approach:</b></td>"
      );
      for a in Approach loop
        if consider_a_priori (a) then
          if a in External then
            ext (a).expanded_options := ext (a).options;
          end if;
          Put (summary, "<td valign=top class=""td_approach"">" & Img (a, html => True) & "</td>");
        end if;
      end loop;
      Put_Line (summary, "</tr>");
      Put (summary,
        "<tr bgcolor=lightyellow><td></td>" &
        "<td bgcolor=lightgrey valign=bottom><b>File name, uncompressed size:</b></td>"
      );
      --  Additionally, we show a row with the Approach's Compression_Method's output format (the
      --  Zip.PKZip_method). If it is not unique, we mention it.
      for a in Approach loop
        if consider_a_priori (a) then
          case a is
            when original =>
              Put (summary, "<td align=right bgcolor=#dddd00 class=""td_approach"">Approach's<br>format &rarr;</td>");
            when Internal =>
              Put (summary, "<td bgcolor=#fafa64>");
              meth := Approach_to_Method (a);
              case meth is
                when Zip.Compress.Single_Method =>
                  Put (summary, Zip.Image (Zip.Compress.Method_to_Format (meth)));
                when Zip.Compress.Multi_Method =>
                  Put (summary, "(Various formats)");
              end case;
              Put (summary, "</td>");
            when External =>
              Put (summary, "<td bgcolor=#fafa64>" & Zip.Image (ext (a).pkzm) & "</td>");
          end case;
        end if;
      end loop;
      Put_Line (summary,
        "<td><b>Choice</b></td>" &
        "<td bgcolor=#dddd00>Choice's<br>method/<br>format</td>" &
        "<td>Original<br>method/<br>format</td>" &
        "<td>Smallest<br>size</td>" &
        "<td>% of<br>original</td>" &
        "<td>% of<br>uncompressed</td>" &
        "<td>Uncompressed<br>size</td>" &
        "<td>Iterations</td></tr>"
      );
      --
      --  1/ Recompress each file into the new archive:
      --
      Process_all (zi);
      --
      --  2/ Almost done - write Central Directory:
      --
      ed.central_dir_offset := Unsigned_64 (Index (repacked_zip_file)) - 1;
      ed.total_entries := 0;
      ed.central_dir_size := 0;
      ed.main_comment_length := 0;
      declare
        comment : constant String := Zip.Zip_comment (zi);
        needs_64, needs_local_zip64 : Boolean;
        fh_extra : Zip.Headers.Local_File_Header_Extension;
        ed64l    : Zip.Headers.Zip64_End_of_Central_Dir_Locator;
        ed64     : Zip.Headers.Zip64_End_of_Central_Dir;
      begin
        if not delete_comment then
          ed.main_comment_length := comment'Length;
        end if;
        --  Restart at the beginning of the list
        e := list;
        needs_64 := False;
        while e /= null loop
          ed.total_entries := ed.total_entries + 1;
          needs_local_zip64 :=
            Zip.Headers.Needs_Local_Zip_64_Header_Extension
              (e.head.short_info, e.head.local_header_offset);
          if needs_local_zip64 then
            e.head.short_info.extra_field_length := Zip.Headers.local_header_extension_length;
            fh_extra.tag  := 1;
            fh_extra.size := Zip.Headers.local_header_extension_length - 4;
            fh_extra.value_64 (1) := e.head.short_info.dd.uncompressed_size;
            fh_extra.value_64 (2) := e.head.short_info.dd.compressed_size;
            fh_extra.value_64 (3) := e.head.local_header_offset;
            e.head.short_info.dd.uncompressed_size := 16#FFFF_FFFF#;
            e.head.short_info.dd.compressed_size   := 16#FFFF_FFFF#;
            e.head.local_header_offset             := 16#FFFF_FFFF#;
            needs_64 := True;
          end if;
          Zip.Headers.Write (repacked_zip_file, e.head);
          String'Write (repacked_zip_file'Access, S (e.name));
          if needs_local_zip64 then
            Zip.Headers.Write (repacked_zip_file, fh_extra, False);
          end if;
          ed.central_dir_size :=
            ed.central_dir_size +
            Zip.Headers.central_header_length +
            Unsigned_64 (e.head.short_info.filename_length) +
            Unsigned_64 (e.head.short_info.extra_field_length);
          e := e.next;
        end loop;
        ed.disknum := 0;
        ed.disknum_with_start := 0;
        ed.disk_total_entries := ed.total_entries;
        if needs_64 then
          ed64l.number_of_the_disk_with_the_start_of_the_zip64_end_of_central_dir := 0;
          ed64l.relative_offset_of_the_zip64_end_of_central_dir_record :=
            Unsigned_64 (Index (repacked_zip_file) - 1);
          ed64l.total_number_of_disks := 1;
          --
          ed64.size := 44;
          ed64.version_made_by           := 16#2D#;
          ed64.version_needed_to_extract := 16#2D#;
          ed64.number_of_this_disk                                        := ed.disknum;
          ed64.number_of_the_disk_with_the_start_of_the_central_directory := ed.disknum_with_start;
          ed64.total_number_of_entries_in_the_central_directory_on_this_disk := ed.disk_total_entries;
          ed64.total_number_of_entries_in_the_central_directory              := ed.total_entries;
          ed64.size_of_the_central_directory        := ed.central_dir_size;
          ed64.offset_of_start_of_central_directory := ed.central_dir_offset;
          Zip.Headers.Write (repacked_zip_file, ed64);
          --
          Zip.Headers.Write (repacked_zip_file, ed64l);
          --
          ed.disk_total_entries := 16#FFFF#;
          ed.total_entries      := 16#FFFF#;
          ed.central_dir_size   := 16#FFFF_FFFF#;
          ed.central_dir_offset := 16#FFFF_FFFF#;
        end if;
        Zip.Headers.Write (repacked_zip_file, ed);
        if not delete_comment then
          String'Write (repacked_zip_file'Access, comment);
        end if;
      end;
      Close (repacked_zip_file);
      Close (MyStream);
      --
      --  Cleanup.
      --
      for a in Approach loop
        if consider_a_priori (a) then
          if Exists (Temp_name (True, a)) then
            Delete_File (Temp_name (True, a));
          end if;
          if a = original then -- also an uncompressed data file to delete
            Delete_File (Temp_name (False, a));
          end if;
        end if;
      end loop;
      --
      --  Report total files per approach.
      --
      Put (summary, "<tr><td></td><td><b>T<small>OTAL FILES (of chosen optimal approach)</small></b></td>");
      for a in Approach loop
        if consider_a_priori (a) then
          Put (summary,
            "<td bgcolor=#" & Webcolor (a) & ">" &
            Integer'Image (total (a).count) & "</td>"
          );
        end if;
      end loop;
      Put (summary,
        "<td></td><td></td><td></td><td bgcolor=lightgreen><b>" & Integer'Image (total_choice.count) &
        "</b></td>" &
        "<td>"
      );
      Put_Line (summary, "</td><td></td><td></td><td></td></tr>");
      --
      --  Report total compressed bytes.
      --
      Put (summary, "<tr><td></td><td><b>T<small>OTAL COMPRESSED BYTES</small></b></td>");
      for a in Approach loop
        if consider_a_priori (a) then
          Put (summary,
            "<td bgcolor=#" & Webcolor (a) & ">" &
            Image_1000 (total (a).size) & "</td>"
          );
        end if;
      end loop;
      Put (summary,
        "<td></td><td></td><td></td><td bgcolor=lightgreen><b>" & Image_1000 (total_choice.size) &
        "</b></td><td>"
      );
      if total (original).size > 0 then
        Put (summary,
          100.0 * Float (total_choice.size) / Float (total (original).size),
          3, 2, 0
        );
        Put (summary, "%");
      end if;
      Put (summary, "</td><td>");
      if total_choice.uncomp_size > 0 then
        Put (summary,
          100.0 * Float (total_choice.size) / Float (total_choice.uncomp_size),
          3, 2, 0
        );
        Put (summary, "%");
      end if;
      Put_Line (summary, "</td><td></td><td></td></tr>");
      --
      --  Report total saved bytes per approach.
      --
      Put (summary, "<tr><td></td><td><b>T<small>OTAL BYTES SAVED (by chosen optimal approach)</small></b></td>");
      for a in Approach loop
        if consider_a_priori (a) then
          Put (summary, "<td bgcolor=#" & Webcolor (a) & ">" & Image_1000 (total (a).saved) & "</td>");
        end if;
      end loop;
      Put (summary,
        "<td></td><td></td><td></td><td bgcolor=lightgreen><b>" &
        Image_1000 (total_choice.saved) & "</b></td>" &
        "<td>"
      );
      if total (original).size > 0 then
        Put (summary,
          100.0 * Float (total_choice.saved) / Float (total (original).size),
          3, 2, 0
        );
        Put (summary, "%");
      end if;
      Put (summary, "</td><td>");
      if total_choice.uncomp_size > 0 then
        Put (summary,
          100.0 * Float (total_choice.saved) / Float (total_choice.uncomp_size),
          3, 2, 0
        );
        Put (summary, "%");
      end if;
      Put_Line (summary, "</td><td></td><td></td></tr>");
      --
      --  Report total saved bytes per approach, *including ex-aequos*.
      --
      Put (summary, "<tr><td></td><td><b>T<small>OTAL BYTES SAVED (by chosen or ex-aequo optimal approach)</small></b></td>");
      for a in Approach loop
        if consider_a_priori (a) then
          Put (summary, "<td bgcolor=#" & Webcolor (a) & ">" & Image_1000 (total (a).saved_ex_aequo) & "</td>");
        end if;
      end loop;
      Put (summary, "<td></td><td></td><td></td><td></td><td></td><td>");
      Put_Line (summary, "</td><td></td><td></td></tr>");
      Put_Line (summary, "</table></div><br><br>");
      Put_Line (summary, "<dt>Options used for ReZip</dt>");
      Put_Line (summary, "<dd>Randomized_stable =" & Integer'Image (randomized_stable) & "<br>");
      Put_Line (summary, "    Formats allowed:<br><table border=1 cellpadding=1 cellspacing=1>");
      for f in format_choice'Range loop
        Put_Line (summary,
          "      <tr><td>" & Zip.Image (f) & "</td><td>" &
          Boolean'Image (format_choice (f)) & "</td></tr>");
      end loop;
      Put_Line (summary, "    </table>");
      Put_Line (summary, "</dd>");
      T1 := Clock;
      repack_duration := T1 - T0;
      Put (summary, "Time elapsed : ");
      Put (summary,  Float (repack_duration), 4, 2, 0);
      Put (summary,  " seconds, or");
      Put (summary,  Float (repack_duration) / 60.0, 4, 2, 0);
      Put (summary,  " minutes, or");
      Put (summary,  Float (repack_duration) / 3600.0, 4, 2, 0);
      Put_Line (summary,  " hours.</font></body></html>");
      Close (summary);
      Dual_IO.Put ("Time elapsed : ");
      DFIO.Put (Float (repack_duration), 4, 2, 0);
      Dual_IO.Put_Line (" sec");
      Dual_IO.Put_Line ("All details for " & orig_name & " in " & html_report_name);
    end Repack_contents;

    --  This is for randomizing the above seed_iterator.
    --  On GNAT the clock-based Reset is too coarse: it gives many times
    --  the same seed when called with small time intervals.
    --
    subtype Seed_Range is Integer range 1 .. 1_000_000;
    package Rnd_seed is new Ada.Numerics.Discrete_Random (Seed_Range);
    gen_seed : Rnd_seed.Generator;

  begin
    Rnd_seed.Reset (gen_seed);  --  1x clock-based randomization
    seed_iterator := Rnd_seed.Random (gen_seed);
    if alt_tmp_file_radix = "" then
      Flexible_temp_files.Initialize;
    end if;
    Dual_IO.Create_Log (log_file);
    Repack_contents (from_zip_file, to_zip_file, html_report);
    Dual_IO.Close_Log;
    if alt_tmp_file_radix = "" then
      Flexible_temp_files.Finalize;
    end if;
  end Rezip;

  procedure Show_external_packer_list is
    procedure Display (p : Zipper_specification) is
      fix : String (1 .. 8) := (others => ' ');
    begin
      Insert (fix, fix'First, S (p.title));
      Ada.Text_IO.Put ("  " & fix);
      fix := (others => ' ');
      Insert (fix, fix'First, S (p.name));
      Ada.Text_IO.Put_Line (" Executable: " & fix & " URL: " & S (p.URL));
    end Display;
    name_is_new : Boolean;
  begin
    for e in External loop
      name_is_new := True;
      for ee in External'First .. External'Pred (e) loop
        name_is_new := name_is_new and ext (e).name /= ext (ee).name;
      end loop;
      if name_is_new then
        Display (ext (e));
      end if;
    end loop;
    Display (defl_opt);
  end Show_external_packer_list;

end Rezip_lib;
