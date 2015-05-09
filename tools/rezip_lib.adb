------------------------------------------------------------------------------
--  File:            Rezip_Prc.adb
--  Description:     Recompression tool to make archives smaller.
--                   Core moved from Rezip (main)
--  Author:          Gautier de Montmollin
------------------------------------------------------------------------------
--
-- To do:
--  * In order to facilitate customization, ReZip could have a config file (
--    http://sf.net/projects/ini-files/ ) to store external packer program
--    names. See ZipMax as an example...
--
-- External programs used (feel free to customize/add/remove):
--   7-Zip, KZip, Zip (info-zip), DeflOpt
--   Web URL's: see Zipper_specification below or run ReZip without arguments.

with Zip.Headers, Zip.Compress, UnZip;
with Zip_Streams;                       use Zip_Streams;

with My_feedback, Flexible_temp_files;

with Ada.Calendar;                      use Ada.Calendar;
with Ada.Directories;
with Ada.Text_IO;                       use Ada.Text_IO;
with Dual_IO;                           use Dual_IO;
-- NB about 'use': no worry, Ada detects all conflicts
-- between Dual_IO and Text_IO
with Ada.Integer_Text_IO;               use Ada.Integer_Text_IO;
with Ada.Float_Text_IO;                 use Ada.Float_Text_IO;
with Ada.Streams.Stream_IO;             use Ada.Streams.Stream_IO;
with Ada.Strings.Fixed;                 use Ada.Strings.Fixed, Ada.Strings;
with Ada.Strings.Unbounded;             use Ada.Strings.Unbounded;
with Ada.Characters.Handling;           use Ada.Characters.Handling;
with Ada.Unchecked_Deallocation;
with Ada.Numerics.Discrete_Random;

with Interfaces;                        use Interfaces;

with GNAT.OS_Lib;

package body Rezip_lib is

  function S (Source : Ada.Strings.Unbounded.Unbounded_String) return String
    renames Ada.Strings.Unbounded.To_String;
  function U (Source : String) return Ada.Strings.Unbounded.Unbounded_String
    renames Ada.Strings.Unbounded.To_Unbounded_String;

  -- This might be better read from a config file...
  --
  type Zipper_specification is record
    name, title, URL, options: Unbounded_String;
    expanded_options    : Unbounded_String;
    -- options with dynamically expanded tokens
    made_by_version     : Unsigned_16;
    pkzm                : Zip.PKZip_method;
    limit               : Zip.File_size_type;
    -- Compression is considered too slow or unefficient beyond limit
    -- E.g., kzip's algorithm might be O(N^2) or worse; on large files,
    --   deflate_e or other methods are better anyway
    randomized          : Boolean;
  end record;

  NN: constant Unbounded_String:= Null_Unbounded_String;

  kzip_limit: constant:= 500_000;

  type Approach is (
    original,
    shrink,
    reduce_1, reduce_2, reduce_3, reduce_4,
    deflate_f,
    external_1, external_2, external_3, external_4,
    external_5, external_6, external_7, external_8,
    external_9, external_10, external_11, external_12,
    external_13
  );

  subtype Internal is Approach
    range Approach'Succ(Approach'First) .. Approach'Pred(external_1);
  subtype External is Approach
    range external_1 .. Approach'Last;

  ext: array(External) of Zipper_specification:=
    ( -- Zip 2.32 or later:
      (U("zip"), U("Zip"), U("http://info-zip.org/"),
         U("-9"), NN, 20, Zip.deflate, 0, False),
      -- 7-Zip 4.64 or later; Deflate:
      (U("7z"),
         U("7-Zip"), U("http://7-zip.org/"),
         U("a -tzip -mm=deflate -mfb=258 -mpass=#RAND#(7,15) -mmc=10000"),
         NN, 20, Zip.deflate, 0, True),
      (U("7z"),
         U("7-Zip"), NN,
         U("a -tzip -mm=deflate64 -mfb=257 -mpass=15 -mmc=10000"),
         NN, 21, Zip.deflate_e, 0, False),
      -- KZip:
      (U("kzip"),U("KZIP"),U("http://www.advsys.net/ken/utils.htm"),
         U("/rn /b0"), NN, 20, Zip.deflate, kzip_limit, True),
      (U("kzip"),U("KZIP"),NN,
         U("/rn /b#RAND#(0,128)"), NN, 20, Zip.deflate, kzip_limit, True),
      (U("kzip"),U("KZIP"),NN,
         U("/rn /b#RAND#(128,2048)"), NN, 20, Zip.deflate, kzip_limit, True),
      -- Zip 3.0 or later; BZip2:
      (U("zip"), U("Zip"), U("http://info-zip.org/"),
         U("-#RAND#(1,9) -Z bzip2"), NN, 46, Zip.bzip2, 0, True),
      -- 7-Zip 9.20 or later; LZMA:
      (U("7z"), U("7-Zip"), NN,
         U("a -tzip -mm=LZMA -mx=9"), NN, 63, Zip.lzma, 0, False),
      (U("7z"), U("7-Zip"), NN, -- dictionary size 2**19 = 512 KB
         U("a -tzip -mm=LZMA:a=2:d=19:mf=bt3:fb=128:lc=0:lp=2"), NN, 63, Zip.lzma, 0, False),
      (U("7z"), U("7-Zip"), NN,
         U("a -tzip -mm=LZMA:a=2:d=#RAND#(3200,3700)k:mf=bt4:fb=#RAND#(255,273):lc=2:lp0:pb0"),
         NN, 63, Zip.lzma, 0, True),
      (U("7z"), U("7-Zip"), NN, -- dictionary size 2**25 = 32 MB
         U("a -tzip -mm=LZMA:a=2:d=25:mf=bt3:fb=255:lc=7"), NN, 63, Zip.lzma, 0, False),
      (U("7z"), U("7-Zip"), NN, -- dictionary size 2**26 = 64 MB
         U("a -tzip -mm=LZMA:a=2:d=26:mf=bt3:fb=222:lc=8:lp0:pb1"), NN, 63, Zip.lzma, 0, False),
      -- AdvZip: advancecomp v1.19+ interesting for the Zopfli algorithm
      (U("advzip"), U("AdvZip"), U("http://advancemame.sourceforge.net/comp-readme.html"),
         U("-a -4"), NN, 20, Zip.deflate, 0, False)
    );

  defl_opt: constant Zipper_specification:=
    (U("deflopt"), U("DeflOpt"), U("http://www.walbeehm.com/download/"),
     NN, NN, 0, Zip.deflate, 0, False);

  procedure Rezip(
    from_zip_file      : String;
    to_zip_file        : String;
    format_choice      : Zip_format_set := all_formats;
    touch              : Boolean        := False;      --  set time stamps to now
    lower              : Boolean        := False;      --  set full file names to lower case
    delete_comment     : Boolean        := False;      --  delete zip comment
    randomized_stable  : Positive       := 1;
    log_file           : String         := "";
    html_report        : String         := "")
  is

    package DFIO is new Dual_IO.Float_IO(Float);

    procedure Rip_data(
      archive      : Zip.Zip_info; -- from this archive...
      input        : in out Root_Zipstream_Type'Class;
      data_name    : String;       -- extract this data
      rip_rename   : String;       -- to this file (compressed)
      unzip_rename : String;       -- and this one (uncompressed)
      header       : out Zip.Headers.Local_File_Header
    )
    is
      file_index     : Zip_Streams.ZS_Index_Type;
      comp_size      : Zip.File_size_type;
      uncomp_size    : Zip.File_size_type;
      file_out       : Ada.Streams.Stream_IO.File_Type;
      dummy_encoding : Zip.Zip_name_encoding;
      dummy_crc      : Unsigned_32;
      use UnZip;
    begin
      Zip.Find_Offset(
        info           => archive,
        name           => data_name,
        name_encoding  => dummy_encoding,
        file_index     => file_index,
        comp_size      => comp_size,
        uncomp_size    => uncomp_size,
        crc_32         => dummy_crc
      );
      Set_Index(input, file_index);
      Zip.Headers.Read_and_check(input, header);
      -- Skip name and extra field
      Set_Index(input,
        Index(input) +
          Zip_Streams.ZS_Size_Type
           (header.extra_field_length +
            header.filename_length)
      );
      -- * Get the data, compressed
      Ada.Streams.Stream_IO.Create(file_out, Out_File, rip_rename);
      Zip.Copy_Chunk(input, Stream(file_out).all, Integer(comp_size));
      Close(file_out);
      if unzip_rename /= "" then
        -- * Get the data, uncompressed
        Extract(
          from    => archive,
          what    => data_name,
          rename  => unzip_rename,
          options =>
          (   test_only => False,
              junk_directories => False,
              case_sensitive_match => True,
              extract_as_text => False
          )
        );
      end if;
    end Rip_data;

    Approach_to_Method: constant array(Internal) of Zip.Compress.Compression_Method:=
      (shrink    => Zip.Compress.shrink,
       reduce_1  => Zip.Compress.reduce_1,
       reduce_2  => Zip.Compress.reduce_2,
       reduce_3  => Zip.Compress.reduce_3,
       reduce_4  => Zip.Compress.reduce_4,
       deflate_f => Zip.Compress.deflate_fixed
      );

    type Packer_info is record
      size            : Zip.File_size_type;
      zfm             : Unsigned_16;
      count           : Natural;
      saved           : Integer_64;
      -- can be negative if -defl chosen: suboptimal recompression,
      -- but compatible method
      uncomp_size     : Unsigned_64;
      -- summed uncompressed sizes might be more than 2**32
      expanded_options: Unbounded_String;
      iter            : Positive; -- iterations needed
    end record;

    type Packer_info_array is array(Approach) of Packer_info;

    type Dir_entry;
    type p_Dir_entry is access Dir_entry;
    --
    type Dir_entry is record
      head: Zip.Headers.Central_File_Header;
      name: Unbounded_String;
      next: p_Dir_entry:= null;
      chosen_approach: Approach:= original;
      info: Packer_info_array;
    end record;

    function Temp_name(
      compressed: Boolean;
      appr      : Approach
    )
      return String
    is
      initial: constant array(Boolean) of Character:= ('u','c');
    begin
      return
        Flexible_temp_files.Radix &
        "_!" & initial(compressed) &
        '!' & Trim(Integer'Image(Approach'Pos(appr)), Left) &
        "!_.tmp";
    end Temp_name;

    function Img(a: Approach; html: Boolean) return String is
      function Repl(s: String) return String is
        t: String:= s;
      begin
        for i in t'Range loop
          if html and t(i) = ':' then t(i):= ' '; end if; -- break too long lines
        end loop;
        return t;
      end;
    begin
      if a in External then
        return "External: " & S(ext(a).title) & ", " & Repl(S(ext(a).expanded_options));
      else
        declare
          s: constant String:= Approach'Image(a);
        begin
          return s(s'First) & To_Lower(s(s'First+1..s'Last) & (Approach'Width-s'Length+1) * ' ');
        end;
      end if;
    end Img;

    procedure Call_external(
      packer:        String;
      args  :        String
    )
    is
      use GNAT.OS_Lib;
      procedure Dispose is
        new Ada.Unchecked_Deallocation(Argument_List, Argument_List_Access);
      list: Argument_List_Access;
      ok: Boolean;
      Cannot_call_external: exception;
    begin
      Dual_IO.Put_Line(packer & " [" & args & ']');
      list:= Argument_String_To_List(args);
      GNAT.OS_Lib.Spawn(packer, list.all, ok);
      Dispose(list);
      if not ok then
        raise Cannot_call_external;
      end if;
    end Call_external;

    seed_iterator: Natural;

    procedure Call_external_expanded(
      packer    :        String;
      options   :        String;
      other_args:        String;
      expand    : in out Unbounded_String -- expanded arguments
    )
    is
      type Token is (rand);
    begin
      expand:= U(options);
      for t in Token loop
        loop
          declare
            tok: constant String:= '#' & Token'Image(t) & '#';
            idx: constant Natural:= Index(expand, tok);
            par: constant Natural:= Index(expand, ")");
            replace: Unbounded_String;
          begin
            exit when idx = 0;  --  No more of token t to replace
            declare
              opt: constant String:= S(expand); -- partially processed option string
              curr: constant String:= opt(idx+1..opt'Last); -- current option
              par_a: constant Natural:= Index(curr, "(");
              par_z: constant Natural:= Index(curr, ")");
              comma: constant Natural:= Index(curr, ",");
              n1, n2, n: Integer;
            begin
              case t is
                when rand =>
                  n1:= Integer'Value(curr(par_a+1..comma-1));
                  n2:= Integer'Value(curr(comma+1..par_z-1));
                  declare
                    subtype rng is Integer range n1..n2;
                    package Rnd is new Ada.Numerics.Discrete_Random(rng);
                    gen: Rnd.Generator;
                  begin
                    Rnd.Reset(gen, seed_iterator);
                    -- On GNAT the clock-based Reset is too coarse
                    -- (gives many times the same seed when called with small
                    -- time intervals).
                    seed_iterator:= seed_iterator + 1;
                    n:= Rnd.Random(gen);
                  end;
                  replace:= U(Trim(Integer'Image(n),Left));
              end case;
              Replace_Slice(expand, idx, par, S(replace));
            end;
          end;
        end loop;
      end loop;
      Call_external(packer, S(expand) & ' ' & other_args);
    end Call_external_expanded;

    procedure Process_External(
      packer  : String;
      options : String;
      out_name: String;
      is_rand : Boolean;
      info    : out Packer_info
    )
    is
      use Ada.Directories;
      temp_zip: constant String:= Simple_Name(Flexible_temp_files.Radix) & "_$temp$.zip";
      rand_win: constant String:= Simple_Name(Flexible_temp_files.Radix) & "_$rand$.tmp";
      options_winner: Unbounded_String;
      data_name: constant String:= Simple_Name(Temp_name(False,original));
      zi_ext: Zip.Zip_info;
      header: Zip.Headers.Local_File_Header;
      MyStream   : aliased File_Zipstream;
      cur_dir: constant String:= Current_Directory;
      size_memory: array(1..randomized_stable) of Zip.File_size_type:= (others => 0);
      size: Zip.File_size_type:= 0;
      zfm: Unsigned_16;
      attempt: Positive:= 1;
    begin
      -- We jump into the TEMP directory, to avoid putting pathes into the
      -- temporary zip file.
      Set_Directory(Containing_Directory(Flexible_temp_files.Radix));
      loop
        if Exists(temp_zip) then -- remove (eventually broken) zip
          Delete_File(temp_zip);
        end if;
        Call_external_expanded(
          packer,
          options,
          temp_zip & ' ' & data_name,
          info.expanded_options
        );
        -- Post processing of "deflated" entries with DeflOpt:
        Call_external(S(defl_opt.name), temp_zip);
        -- Now, rip
        Set_Name (MyStream, temp_zip);
        Open (MyStream, In_File);
        Zip.Load( zi_ext, MyStream, True );
        Rip_data(
          archive      => zi_ext,
          input        => MyStream,
          data_name    => data_name,
          rip_rename   => out_name,
          unzip_rename => "",
          header       => header
        );
        Close(MyStream);
        Delete_File(temp_zip);
        Zip.Delete(zi_ext);
        --
        if randomized_stable = 1 or not is_rand then -- normal behaviour (1 attempts)
          size:= header.dd.compressed_size;
          zfm := header.zip_type;
          info.iter:= 1;
          exit;
        end if;
        --
        -- Here, we process the cases where compressed sizes need
        -- to be reduced and we expect a stable size over n=randomized_stable
        -- attempts.
        --
        if attempt = 1 or else
          header.dd.compressed_size < size -- better size
        then
          size:= header.dd.compressed_size;
          zfm := header.zip_type;
          if Exists(rand_win) then
            Delete_file(rand_win);
          end if;
          Rename(out_name, rand_win);
          options_winner:= info.expanded_options;
        end if;
        --
        -- Manage the array of last n=randomized_stable sizes
        --
        if attempt > size_memory'Last then
          for i in size_memory'First+1..size_memory'Last loop
            size_memory(i-1):= size_memory(i);
          end loop;
          size_memory(size_memory'Last):= size;
        else
          size_memory(attempt):= size;
        end if;
        --
        -- Check stability over n=randomized_stable attempts
        --
        if attempt >= randomized_stable then
          if size_memory(randomized_stable) = size_memory(1) then
            if Exists(out_name) then
              Delete_file(out_name);
            end if;
            Rename(rand_win, out_name);
            info.expanded_options:= options_winner;
            info.iter:= attempt;
            exit;
          end if;
        end if;
        attempt:= attempt + 1;
      end loop;
      info.size       := size;
      info.uncomp_size:= Unsigned_64(header.dd.uncompressed_size);
      -- uncomp_size should not matter (always the same).
      info.zfm        := zfm;
      -- We jump back to the startup directory.
      Set_Directory(cur_dir);
    end Process_external;

    time_0      : constant Ada.Calendar.Time:= Clock;

    procedure Repack_contents(orig_name, repacked_name, html_report_name: String)
    is
      use type Zip.PKZip_method;
      zi: Zip.Zip_info;
      MyStream   : aliased File_Zipstream;

      list, e, curr: p_Dir_entry:= null;
      repacked_zip_file   : aliased File_Zipstream;
      total: Packer_info_array:= (others => (0,0,0,0,0,NN,1));
      -- total(a).count counts the files where approach 'a' was optimal
      -- total(a).saved counts the saved bytes when approach 'a' was optimal
      total_choice: Packer_info:= (0,0,0,0,0,NN,1);
      summary: Ada.Text_IO.File_Type;
      T0, T1 : Ada.Calendar.Time;
      seconds: Duration;
      --
      type Approach_Filtering is array(Approach) of Boolean;
      consider_a_priori: Approach_Filtering;
      --
      lightred: constant String:= "#f43048";

      procedure Process_one(unique_name: String) is
        comp_size  :  Zip.File_size_type;
        uncomp_size:  Zip.File_size_type;
        File_in       : aliased File_Zipstream;
        File_out      : aliased File_Zipstream;
        choice: Approach:= original;
        deco: constant String:= "-->-->-->" & (20+unique_name'Length) * '-';
        mth: Zip.PKZip_method;
        consider: Approach_Filtering;
        gain: Integer_64;
        --
        procedure Winner_color is
        begin
          if e.info(choice).size < e.info(original).size then
            Put(summary,"<td bgcolor=lightgreen><b>");
            -- We were able to reduce the size
          elsif e.info(choice).size = e.info(original).size then
            Put(summary,"<td><b>");
            -- Original was the best, alas...
          else
            Put(summary,"<td bgcolor=" & lightred & "><b>");
            -- Forced method with less efficient compression
          end if;
        end Winner_color;
        --
      begin
        consider:= consider_a_priori;
        if unique_name = "" or else
          (   unique_name(unique_name'Last)='\'
           or unique_name(unique_name'Last)='/'
          )
        then
          return; -- directories are useless entries!
        end if;
        total_choice.count:= total_choice.count + 1;
        Dual_IO.Close_and_Append_Log; -- have an up to date copy on file system
        Dual_IO.Put_Line(deco);
        Dual_IO.Put_Line(
          ' ' &
          Integer'Image((100 * total_choice.count) / Zip.Entries(zi)) &
          "% - Processing " &
          unique_name & ',' &
          Integer'Image(total_choice.count) &
          " of" &
          Integer'Image(Zip.Entries(zi))
        );
        Dual_IO.Put_Line(deco);
        Dual_IO.New_Line;
        --
        e:= new Dir_entry;
        if curr = null then
          curr:= e;
          list:= e;
        else
          curr.next:= e;
          curr:= e;
        end if;
        e.name:= U(unique_name);
        e.head.made_by_version     := 20; -- version 2.0
        e.head.comment_length      := 0;
        e.head.disk_number_start   := 0;
        e.head.internal_attributes := 0; -- 0: seems binary; 1, text
        e.head.external_attributes := 0;
        --
        Dual_IO.Put("    Phase 1:  dump & unzip -");
        Rip_data(
          archive      => zi,
          input        => MyStream,
          data_name    => unique_name,
          rip_rename   => Temp_name(True,original),
          unzip_rename => Temp_name(False,original),
          header       => e.head.short_info
        );
        --
        if touch then
          e.head.short_info.file_timedate:= Zip.Convert(time_0);
        end if;
        if lower then
          e.name:= U(To_Lower(S(e.name)));
        end if;
        -- Get reliable data from zi
        Zip.Get_sizes(
          info           => zi,
          name           => unique_name,
          comp_size      => comp_size,
          uncomp_size    => uncomp_size
        );
        Dual_IO.Put_Line(" done");
        --
        --  Apply limitations: skip some methods if certain conditions are met.
        --  For instance:
        --    Shrink may in rare cases be better for tiny files;
        --    KZip is excellent but really too slow on large files
        --
        for a in Approach loop
          case a is
            when original =>
              null;
            when shrink =>
              consider(a):= consider(a) and uncomp_size <= 5000;
            when reduce_1 .. reduce_4 =>
              consider(a):= consider(a) and uncomp_size <= 8000;
            when deflate_f =>
              consider(a):= consider(a) and uncomp_size <= 2000;
            when External =>
              consider(a):= consider(a) and (ext(a).limit = 0 or uncomp_size <= ext(a).limit);
          end case;
        end loop;
        Dual_IO.Put_Line("    Phase 2:  try different tactics...");
        --
        Try_all_approaches:
        --
        for a in Approach loop
          if consider(a) then
            Dual_IO.Put("              -o-> " & Img(a, html => False));
            e.info(a).iter:= 1;
            case a is
              --
              when original =>
                -- This is from the original .zip - just record size and method
                e.info(a).size:= comp_size;
                e.info(a).zfm := e.head.short_info.zip_type;
                mth:= Zip.Method_from_code(e.info(a).zfm);
                --
              when Internal =>
                Set_Name (File_in, Temp_name(False,original));
                Open (File_in, In_File);
                Set_Name (File_out, Temp_name(True,a));
                Create (File_out, Out_File);
                Zip.Compress.Compress_data
                (
                  input            => File_in,
                  output           => File_out,
                  input_size_known => True,
                  input_size       => e.head.short_info.dd.uncompressed_size,
                  method           => Approach_to_Method(a),
                  feedback         => My_Feedback'Access,
                  password         => "",
                  CRC              => e.head.short_info.dd.crc_32,
                  -- we take the occasion to compute the CRC if not
                  -- yet available (e.g. JAR)
                  output_size      => e.info(a).size,
                  zip_type         => e.info(a).zfm
                );
                Close(File_in);
                Close(File_out);
                --
              when External =>
                Dual_IO.New_Line;
                Process_External(
                  S(ext(a).name),
                  S(ext(a).options),
                  Temp_name(True,a),
                  ext(a).randomized,
                  e.info(a)
                );
                e.head.made_by_version:= ext(a).made_by_version;
                ext(a).expanded_options:= e.info(a).expanded_options;
                --
            end case;
            total(a).size:= total(a).size + e.info(a).size;
            if e.info(a).size < e.info(choice).size then
              -- Hurra, we found a smaller size!
              choice:= a;
            end if;
            if choice = original and not format_choice(mth) then
              --  This occurs if we want to make an archive with only a certain set of formats,
              --  for instance deflate_or_store, which is the most compatible.
              --  Since approach _a_ uses a format in the desired set, the choice will be
              --  forced out of original, even with a worse size.
              choice:= a;
            end if;
            Dual_IO.New_Line;
          end if;
        end loop Try_all_approaches;
        --
        total_choice.size:= total_choice.size + e.info(choice).size;
        total(choice).count:= total(choice).count + 1;
        total_choice.uncomp_size:=
          total_choice.uncomp_size + Unsigned_64(uncomp_size);
        gain:= Integer_64(e.info(original).size) - Integer_64(e.info(choice).size);
        total(choice).saved:= total(choice).saved + gain;
        total_choice.saved:= total_choice.saved + gain;
        --
        Dual_IO.New_Line;
        Dual_IO.Put(
          "    Phase 3:  Winner is " & Img(choice, html => False) &
          "; gain in bytes:" & Integer_64'Image(gain) &
          "; writing data -"
        );
        -- * Summary outputs
        Put(summary,
          "<tr><td>" &
          Trim(Integer'Image(total_choice.count),Left) &
          -- '/' &
          -- Trim(Integer'Image(Zip.Entries(zi)),Left) &
          "</td>" &
          "<td bgcolor=lightgrey><tt>" & unique_name & "</tt></td>");
        for a in Approach loop
          if consider_a_priori(a) then
            if not consider(a) then
              Put(summary,"<td bgcolor=lightgray>skipped");
            elsif a = choice then
              Winner_color;
            elsif e.info(a).size = e.info(choice).size then -- ex aequo
              Put(summary,"<td bgcolor=lightblue><b>");
            else
              Put(summary,"<td>");
            end if;
            if consider(a) then
              Put(summary, Zip.File_size_type'Image(e.info(a).size));
            end if;
            if choice = a then
              Put(summary,"</b>");
            end if;
            Put(summary,"</td>");
          end if;
        end loop;
        -- Recall winner approach, method and size:
        Put(summary,"<td>" & Img(choice, HTML => True) & "</td>");
        Put(summary,
          "<td bgcolor=#fafa64>" &
          To_Lower(Zip.PKZip_method'Image(Zip.Method_from_code(e.info(choice).zfm))) &
          "</td>"
        );
        Put(summary,
          "<td>" &
          To_Lower(Zip.PKZip_method'Image(Zip.Method_from_code(e.info(original).zfm))) &
          "</td>"
        );
        Winner_color;
        Put(summary, Zip.File_size_type'Image(e.info(choice).size));
        Put(summary,"</b></td><td>");
        if e.info(original).size > 0 then
          Put(
            summary,
            100.0 * Float(e.info(choice).size) / Float(e.info(original).size),
            3,2,0
          );
          Put(summary,"%");
        end if;
        Put(summary,"</td><td>");
        if uncomp_size > 0 then
          Put(
            summary,
            100.0 * Float(e.info(choice).size) / Float(uncomp_size),
            3,2,0
          );
          Put(summary,"%");
        end if;
        Put(summary,"</td><td>");
        Put(summary,Integer'Image(e.info(choice).iter));
        Put_Line(summary,"</td></tr>");
        --
        -- Write winning data:
        --
        e.head.short_info.extra_field_length:= 0; -- We choose to ignore it...
        -- No data descriptor after data (bit 3); no EOS for LZMA (bit 1):
        e.head.short_info.bit_flag:=
          e.head.short_info.bit_flag and (2#1111_1111_1111_0101#);
        -- Set or adjust the pre-data data descriptor:
        -- NB: even if missing pre-data, CRC will have been computed
        --     at least with one internal method
        e.head.short_info.dd.uncompressed_size:= uncomp_size;
        -- Put the winning size and method
        e.head.short_info.dd.compressed_size:= e.info(choice).size;
        e.head.short_info.zip_type:= e.info(choice).zfm;
        e.head.local_header_offset:= Unsigned_32(Index(repacked_zip_file))-1;
        Zip.Headers.Write(repacked_zip_file, e.head.short_info);
        String'Write(repacked_zip_file'Access, S(e.name));
        -- Copy the compressed data
        Zip.Copy_file( Temp_name(True,choice), repacked_zip_file );
        Dual_IO.Put_Line(" done");
        Dual_IO.New_Line;
      end Process_one;

      procedure Process_all is new Zip.Traverse(Process_one);

      ed: Zip.Headers.End_of_Central_Dir;

      function Webcolor(a: Approach) return String is
        v: Float;
        sr,sg,sb: String(1..10);
      begin
        if total_choice.saved > 0 and
          -- with options like -defl ot -fast_dec, we may have
          -- negative values or other strange things:
           total(a).saved >= 0
        then
          v:= Float(total(a).saved) / Float(total_choice.saved);
          -- ^ contribution of approach 'a'
        else
          v:= 0.0;
        end if;
        Put(sr, 512 + Integer(144.0 + 111.0 * (1.0 - v)), 16);
        sb:= sr;
        Put(sg, 512 + Integer(238.0 + 17.0 * (1.0 - v)), 16);
        return
          sr(sr'Last-2..sr'Last-1) &
          sg(sg'Last-2..sg'Last-1) &
          sb(sb'Last-2..sb'Last-1);
      end Webcolor;

      use Ada.Directories;

    begin -- Repack_contents
      T0:= Clock;
      for a in Approach loop
        case a is
          when original =>
            consider_a_priori(a):= True;
          when Internal =>
            consider_a_priori(a):= format_choice(Zip.Compress.Method_to_Format(Approach_to_Method(a)))
              and a not in reduce_1..reduce_3;
          when External =>
            consider_a_priori(a):= format_choice(ext(a).pkzm);
        end case;
      end loop;
      Set_Name (MyStream, orig_name);
      Open (MyStream, In_File);
      Zip.Load( zi, MyStream, True );

      Set_Name (repacked_zip_file, repacked_name);
      Create(repacked_zip_file, Out_File);
      Create(summary, Out_File, html_report_name);
      Put_Line(summary,
        "<html><head><title>ReZip summary for file "
         & orig_name & "</title></head>"
      );
      Put_Line(summary, "<style>.container { overflow-y: auto; height: 87%; }");
      Put_Line(summary, "td_approach { width:115px; }");
      Put_Line(summary, "</style><body>");
      Put_Line(summary,
        "<h2><a target=_blank href=" & Zip.web &
        ">ReZip</a> summary for file " & orig_name & "</h2>"
      );
      Put_Line(summary,
        "Library version " & Zip.version & " dated " & Zip.reference
      );
      if format_choice /= all_formats then
        Put_Line(summary,
          "<br><table border=0 cellpadding=0 cellspacing=0>" &
          "<tr bgcolor=" & lightred &
          "><td><b>An option that filters methods is on, " &
          "result(s) may be sub-optimal.</b></td></tr></table><br>"
        );
      end if;
      Put_Line(summary, "<div class=""container""><table border=1 cellpadding=1 cellspacing=1>");
      Put(summary,
        "<tr bgcolor=lightyellow><td></td>"&
        "<td align=right valign=top><b>Approach:</b></td>"
      );
      for a in Approach loop
        if consider_a_priori(a) then
          if a in External then
            ext(a).expanded_options:= ext(a).options;
          end if;
          Put(summary, "<td valign=top class=""td_approach"">" & Img(a, html => True) & "</td>");
        end if;
      end loop;
      Put_Line(summary, "</tr>");
      Put(summary,
        "<tr bgcolor=lightyellow><td></td>"&
        "<td bgcolor=lightgrey valign=bottom><b>File name:</b></td>"
      );
      for a in Approach loop
        if consider_a_priori(a) then
          case a is
            when original =>
              Put(summary, "<td align=right bgcolor=#dddd00 class=""td_approach"">Approach's<br>method/<br>format &rarr;</td>");
            when Internal =>
              Put(summary, "<td bgcolor=#fafa64>" & To_Lower( Zip.Compress.Compression_Method'Image(Approach_to_Method(a))) & "</td>");
              -- better: the Zip.PKZip_method, in case 2 Compression_Method's produce the same sub-format
            when External =>
              Put(summary, "<td bgcolor=#fafa64>" & To_Lower(Zip.PKZip_method'Image(ext(a).pkzm)) & "</td>");
          end case;
        end if;
      end loop;
      Put_Line(summary,
        "<td><b>Choice</b></td>"&
        "<td bgcolor=#dddd00>Choice's<br>method/<br>format</td>"&
        "<td>Original<br>method/<br>format</td>"&
        "<td>Smallest<br>size</td>" &
        "<td>% of<br>original</td><td>% of<br>uncompressed</td><td>Iterations</td></tr>"
      );
      --
      -- 1/ Recompress each file into the new archive:
      --
      Process_all(zi);
      --
      -- 2/ Almost done - write Central Directory:
      --
      ed.central_dir_offset:= Unsigned_32(Index(repacked_zip_file))-1;
      ed.total_entries:= 0;
      ed.central_dir_size:= 0;
      ed.main_comment_length:= 0;
      declare
        comment: constant String:= Zip.Zip_comment(zi);
      begin
        if not delete_comment then
          ed.main_comment_length:= comment'Length;
        end if;
        -- Restart at the beginning of the list
        e:= list;
        while e /= null loop
          ed.total_entries:= ed.total_entries + 1;
          Zip.Headers.Write(repacked_zip_file, e.head);
          String'Write(repacked_zip_file'Access, S(e.name));
          ed.central_dir_size:=
            ed.central_dir_size +
            Zip.Headers.central_header_length +
            Unsigned_32(e.head.short_info.filename_length);
          e:= e.next;
        end loop;
        ed.disknum:= 0;
        ed.disknum_with_start:= 0;
        ed.disk_total_entries:= ed.total_entries;
        Zip.Headers.Write(repacked_zip_file, ed);
        if not delete_comment then
          String'Write(repacked_zip_file'Access, comment);
        end if;
      end;
      Close(repacked_zip_file);
      Close(MyStream);
      --
      -- Cleanup
      --
      for a in Approach loop
        if consider_a_priori(a) then
          if Exists(Temp_name(True,a)) then
            Delete_File( Temp_name(True,a) );
          end if;
          if a = original then -- also an uncompressed data file to delete
            Delete_File( Temp_name(False,a) );
          end if;
        end if;
      end loop;
      -- Report total bytes
      Put(summary,"<tr><td></td><td><b>T<small>OTAL BYTES</small></b></td>");
      for a in Approach loop
        if consider_a_priori(a) then
          Put(summary,
            "<td bgcolor=#" & Webcolor(a) & ">" &
            Zip.File_size_type'Image(total(a).size) & "</td>"
          );
        end if;
      end loop;
      Put(summary,
        "<td></td><td></td><td></td><td bgcolor=lightgreen><b>" & Zip.File_size_type'Image(total_choice.size) &
        "</b></td><td>"
      );
      if total(original).size > 0 then
        Put(summary,
          100.0 * Float(total_choice.size) / Float(total(original).size),
          3,2,0
        );
        Put(summary,"%");
      end if;
      Put(summary, "</td><td>");
      if total_choice.uncomp_size > 0 then
        Put(summary,
          100.0 * Float(total_choice.size) / Float(total_choice.uncomp_size),
          3,2,0
        );
        Put(summary,"%");
      end if;
      Put_Line(summary, "</td></tr>");
      -- Report total files per approach
      Put(summary,"<tr><td></td><td><b>T<small>OTAL FILES</small></b></td>");
      for a in Approach loop
        if consider_a_priori(a) then
          Put(summary,
            "<td bgcolor=#" & Webcolor(a) & ">" &
            Integer'Image(total(a).count) & "</td>"
          );
        end if;
      end loop;
      Put(summary,
        "<td></td><td></td><td></td><td bgcolor=lightgreen><b>" & Integer'Image(total_choice.count) &
        "</b></td>" &
        "<td>"
      );
      Put_Line(summary, "</td></tr>");
      -- Report total saved bytes per approach
      Put(summary,"<tr><td></td><td><b>T<small>OTAL SAVED BYTES</small></b></td>");
      for a in Approach loop
        if consider_a_priori(a) then
          Put(summary,
            "<td bgcolor=#" & Webcolor(a) & ">" &
            Integer_64'Image(total(a).saved) & "</td>"
          );
        end if;
      end loop;
      Put(summary,
        "<td></td><td></td><td></td><td bgcolor=lightgreen><b>" & Integer_64'Image(total_choice.saved) &
        "</b></td>" &
        "<td>"
      );
      if total(original).size > 0 then
        Put(summary,
          100.0 * Float(total_choice.saved) / Float(total(original).size),
          3,2,0
        );
        Put(summary,"%");
      end if;
      Put(summary, "</td><td>");
      if total_choice.uncomp_size > 0 then
        Put(summary,
          100.0 * Float(total_choice.saved) / Float(total_choice.uncomp_size),
          3,2,0
        );
        Put(summary,"%");
      end if;
      Put_Line(summary, "</td></tr></table></div><br>");
      T1:= Clock;
      seconds:= T1-T0;
      Put(summary, "Time elapsed : ");
      Put(summary,  Float( seconds ), 4, 2, 0 );
      Put(summary,  " seconds, or");
      Put(summary,  Float( seconds ) / 60.0, 4, 2, 0 );
      Put(summary,  " minutes, or");
      Put(summary,  Float( seconds ) / 3600.0, 4, 2, 0 );
      Put_Line(summary,  " hours.</body></html>");
      Close(summary);
      Dual_IO.Put("Time elapsed : ");
      DFIO.Put( Float( seconds ), 4, 2, 0 );
      Dual_IO.Put_Line( " sec");
      Dual_IO.Put_Line("All details for " & orig_name & " in " & html_report_name);
    end Repack_contents;

    -- This is for randomizing the above seed_iterator.
    subtype Seed_Range is Integer range 1..1_000_000;
    package Rnd_seed is new Ada.Numerics.Discrete_Random(Seed_range);
    gen_seed: Rnd_seed.Generator;

  begin
    Rnd_seed.Reset(gen_seed); -- 1x clock-based randomization
    seed_iterator:= Rnd_seed.Random(gen_seed);
    Flexible_temp_files.Initialize;
    Dual_IO.Create_Log(log_file);
    Repack_contents(from_zip_file, to_zip_file, html_report);
    Dual_IO.Close_Log;
    Flexible_temp_files.Finalize;
  end Rezip;

  procedure Show_external_packer_list is
    procedure Display(p: Zipper_specification) is
      fix: String(1..8):= (others => ' ');
    begin
      Insert(fix,fix'First, S(p.title));
      Ada.Text_IO.Put("  " & fix);
      fix:= (others => ' ');
      Insert(fix,fix'First, S(p.name));
      Ada.Text_IO.Put_Line(" Executable:  " & fix & " URL: " & S(p.URL));
    end Display;
    name_is_new: Boolean;
  begin
    for e in External loop
      name_is_new:= True;
      for ee in External'First .. External'Pred(e) loop
        name_is_new:= name_is_new and ext(e).name /= ext(ee).name;
      end loop;
      if name_is_new then
        Display(ext(e));
      end if;
    end loop;
    Display(defl_opt);
  end;

end Rezip_lib;
