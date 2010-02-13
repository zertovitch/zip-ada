------------------------------------------------------------------------------
--  File:            ReZip.adb
--  Description:     Recompression tool to make archives smaller.
--                     Uses brute force and pick-and-choose among compression
--                     tools and methods. Typically the optimal archive will
--                     contain some entries compressed with the BZip2 format,
--                     and others with the Deflate one.
--                     Compression speed doesn't matter, only the final size.
--                     NB: due to random parameters to some compressors, only
--                     several runs of ReZip will result in the optimal size.
--                     See rezip_loop.cmd.
--
--  Date/version:    ... ; 11-Jan-2008
--  Author:          Gautier de Montmollin
------------------------------------------------------------------------------
--
-- To do:
--  * In order to facilitate customization, ReZip could have a config file (
--    http://sf.net/projects/ini-files/ ) to store external packer program names.
--
-- External programs used (feel free to customize/add/remove):
--   7-Zip, KZip, Zip (info-zip), DeflOpt
--   Web URL's: see Zipper_specification below or run ReZip without arguments.

with Zip.Headers, Zip.Compress, UnZip;
with Zip_Streams;                       use Zip_Streams;

with Comp_Zip_Prc;

with My_feedback, Flexible_temp_files;

with Ada.Calendar;                      use Ada.Calendar;
with Ada.Command_Line;                  use Ada.Command_Line;
with Ada.Directories;
with Ada.Text_IO;                       use Ada.Text_IO;
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

procedure ReZip is

  procedure Blurb is
  begin
    Put_Line("ReZip * Zip file recompression tool.");
    Put_Line("Author: Gautier de Montmollin");
    Put_Line("Library version " & Zip.version & " dated " & Zip.reference );
    Put_Line("URL: " & Zip.web);
    New_Line;
  end Blurb;

  -- Copy a chunk from a stream into another one:
  procedure Copy_chunk(from : Zipstream_Class;
                       into : Ada.Streams.Stream_IO.File_Type; bytes: Natural) is
    buf: Zip.Byte_Buffer(1..32768);
    actually_read, remains: Natural;
  begin
    remains:= bytes;
    loop
      exit when remains = 0;
      Zip.BlockRead(from,buf(1..Integer'Min(remains, buf'Last)),actually_read);
      if actually_read = 0 then -- premature end, unexpected
        raise Zip.Zip_File_Error;
      end if;
      remains:= remains - actually_read;
      Zip.BlockWrite(Stream(into).all, buf(1..actually_read));
    end loop;
  end Copy_chunk;

  -- Copy a whole file into a stream:
  procedure Copy(from: String; into: Zipstream_Class) is
    f: Ada.Streams.Stream_IO.File_Type;
    buf: Zip.Byte_Buffer(1..32768);
    actually_read: Natural;
  begin
    Open(f, In_File, from);
    loop
      Zip.BlockRead(f,buf,actually_read);
      exit when actually_read = 0; -- this is expected
      Zip.BlockWrite(into.all, buf(1..actually_read));
    end loop;
    Close(f);
  end Copy;

  function S (Source : Ada.Strings.Unbounded.Unbounded_String) return String
    renames Ada.Strings.Unbounded.To_String;
  function U (Source : String) return Ada.Strings.Unbounded.Unbounded_String
    renames Ada.Strings.Unbounded.To_Unbounded_String;

  procedure Rip_data(
    archive      : Zip.Zip_info; -- from this archive...
    InputStream  : in Zipstream_Class;
    data_name    : String;       -- extract this data
    rip_rename   : String;       -- to this file (compressed)
    unzip_rename : String;       -- and this one (uncompressed)
    case_match   : Boolean;
    header       : out Zip.Headers.Local_File_Header
  )
  is
    file_index :  Ada.Streams.Stream_IO.Positive_Count;
    comp_size  :  Zip.File_size_type;
    uncomp_size:  Zip.File_size_type;
    file_out   : Ada.Streams.Stream_IO.File_Type;
    use UnZip;
  begin
    Zip.Find_Offset(
      info           => archive,
      name           => data_name,
      case_sensitive => case_match,
      file_index     => file_index,
      comp_size      => comp_size,
      uncomp_size    => uncomp_size
    );
    Set_Index(InputStream, Positive(file_index));
    Zip.Headers.Read_and_check(InputStream, header);
    -- Skip name and extra field
    Set_Index(InputStream,
      Index(InputStream) +
        Positive (header.extra_field_length +
        header.filename_length)
    );
    -- * Get the data, compressed
    Create(file_out, Out_File, rip_rename);
    Copy_Chunk(InputStream, file_out, Integer(comp_size));
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
            case_sensitive_match => case_match,
            extract_as_text => False
        )
      );
    end if;
  end Rip_data;

  type Approach is (
    original,
    shrink,
    reduce_1, reduce_2, reduce_3, reduce_4,
    external_1, external_2, external_3, external_4,
    external_5, external_6, external_7
  );

  subtype Internal is Approach
    range Approach'Succ(Approach'First) .. Approach'Pred(external_1);
  subtype External is Approach
    range external_1 .. Approach'Last;

  Approach_to_Method: constant array(Internal) of Zip.Compress.Compression_Method:=
    (shrink   => Zip.Compress.shrink,
     reduce_1 => Zip.Compress.reduce_1,
     reduce_2 => Zip.Compress.reduce_2,
     reduce_3 => Zip.Compress.reduce_3,
     reduce_4 => Zip.Compress.reduce_4
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

  ext: array(External) of Zipper_specification:=
    (
      (U("zip.exe"), U("Zip"), U("http://info-zip.org/"),
         U("-9"), NN, 20, Zip.deflate, 0, False),
       -- Zip 2.32 or later
      (U("zip.exe"), U("Zip"), U("http://info-zip.org/"),
         U("-#RAND#(1,9) -Z bzip2"), NN, 46, Zip.bzip2, 0, True),
       -- Zip 3.0 or later
      (U("7z.exe"),                                      -- 7-Zip 4.64 or later
         U("7-Zip"), U("http://7-zip.org/"),
         U("a -tzip -mx9 -mm=deflate -mfb=258 -mpass=15 -mmc=10000"),
         NN, 20, Zip.deflate, 0, False),
      (U("7z.exe"),
         U("7-Zip"), NN,
         U("a -tzip -mx9 -mm=deflate64 -mfb=257 -mpass=15 -mmc=10000"),
         NN, 21, Zip.deflate_e, 0, False),
      (U("kzip.exe"),U("KZIP"),U("http://www.advsys.net/ken/utils.htm"),
         U("/rn /b0"), NN, 20, Zip.deflate, kzip_limit, True),
      (U("kzip.exe"),U("KZIP"),NN,
         U("/rn /b#RAND#(0,128)"), NN, 20, Zip.deflate, kzip_limit, True),
      (U("kzip.exe"),U("KZIP"),NN,
         U("/rn /b#RAND#(128,1024)"), NN, 20, Zip.deflate, kzip_limit, True)
    );

  defl_opt: constant Zipper_specification:=
    (U("deflopt.exe"), U("DeflOpt"), U("http://www.walbeehm.com/download/"),
     NN, NN, 0, Zip.deflate, 0, False);

  function Img(a: Approach) return String is
  begin
    if a in External then
      return "External: " & S(ext(a).title) & ", " & S(ext(a).expanded_options);
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
  begin
    Put_Line(packer & " [" & args & ']');
    list:= Argument_String_To_List(args);
    GNAT.OS_Lib.Spawn(packer, list.all, ok);
    Dispose(list);
    if not ok then
      raise Program_Error;
    end if;
  end Call_external;

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
      declare
        tok: constant String:= '#' & Token'Image(t) & '#';
        idx: constant Natural:= Index(expand, tok);
        par: constant Natural:= Index(expand, ")");
        replace: Unbounded_String;
      begin
        if idx > 0 then
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
                  Rnd.Reset(gen);
                  n:= Rnd.Random(gen);
                end;
                replace:= U(Trim(Integer'Image(n),Left));
            end case;
            Replace_Slice(expand, idx, par, S(replace));
          end;
        end if;
      end;
    end loop;
    Call_external(packer, S(expand) & ' ' & other_args);
  end Call_external_expanded;

  deflate_only: Boolean:= False;
  fast_decomp : Boolean:= False;
  compare     : Boolean:= False;
  lower       : Boolean:= False;
  touch       : Boolean:= False;
  del_comment : Boolean:= False;
  rand_stable : Positive:= 1;
  -- ^ we want to reach a stable size over n=rand_stable attempts
  --   on a method with randomized parameters.

  procedure Process_External(
    packer  : String;
    options : String;
    out_name: String;
    is_rand : Boolean;
    info    : out Packer_info
  ) is
    use Ada.Directories;
    temp_zip: constant String:= Simple_Name(Flexible_temp_files.Radix) & "_$temp$.zip";
    data_name: constant String:= Simple_Name(Temp_name(False,original));
    zi_ext: Zip.Zip_info;
    header: Zip.Headers.Local_File_Header;
    MyStream   : aliased ZipFile_Stream;
    StreamFile : constant Zipstream_Class := MyStream'Unchecked_Access;
    cur_dir: constant String:= Current_Directory;
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
      SetName (StreamFile, temp_zip);
      Open (MyStream, In_File);
      Zip.Load( zi_ext, StreamFile, False );
      Rip_data(
        archive      => zi_ext,
        InputStream  => StreamFile,
        data_name    => data_name,
        rip_rename   => out_name,
        unzip_rename => "",
        case_match   => False, -- external packers are DOS/Windows
        header       => header
      );
      Close (MyStream);
      Delete_File(temp_zip);
      exit when not is_rand or rand_stable = 1;
      --
      -- !! Here, process the cases where compressed sizes need
      -- to be reduced and we expect a stable size over n=rand_stable
      -- attempts.
      --
    end loop;
    info.size       := header.dd.compressed_size;
    info.uncomp_size:= Unsigned_64(header.dd.uncompressed_size);
    -- uncomp_size should not matter (always the same).
    info.zfm        := header.zip_type;
    -- We jump back to the startup directory.
    Set_Directory(cur_dir);
  end Process_external;

  time_0      : constant Ada.Calendar.Time:= Clock;

  procedure Repack_contents(orig_name, repacked_name, log_name: String)
  is
    use type Zip.PKZip_method;
    zi: Zip.Zip_info;
    MyStream   : aliased ZipFile_Stream;
    StreamFile : constant Zipstream_Class := MyStream'Unchecked_Access;

    list, e, curr: p_Dir_entry:= null;
    repacked_zip_file   : aliased ZipFile_Stream;
    Streamrepacked_zip_file : constant Zipstream_Class := repacked_zip_file'Unchecked_Access;
    total: Packer_info_array:= (others => (0,0,0,0,0,NN));
    -- total(a).count counts the files where approach 'a' was optimal
    -- total(a).saved counts the saved bytes when approach 'a' was optimal
    total_choice: Packer_info:= (0,0,0,0,0,NN);
    summary: Ada.Text_IO.File_Type;
    T0, T1 : Ada.Calendar.Time;
    seconds: Duration;
    --
    type Approach_Filtering is array(Approach) of Boolean;
    always_consider: Approach_Filtering;
    Is_fast_decomp_method: constant array(Zip.PKZip_method) of Boolean:=
      (Zip.store .. Zip.deflate_e => True,
       Zip.bzip2 => False,
       others => False);
    --
    lightred: constant String:= "#f43048";

    procedure Process_one(unique_name: String) is
      comp_size  :  Zip.File_size_type;
      uncomp_size:  Zip.File_size_type;
      File_in       : aliased ZipFile_Stream;
      StreamFile_in : constant Zipstream_Class := File_in'Unchecked_Access;
      File_out      : aliased ZipFile_Stream;
      StreamFile_out: constant Zipstream_Class := File_out'Unchecked_Access;
      choice: Approach:= original;
      deco: constant String:= "-->-->-->----" & unique_name'Length * '-';
      mth: Zip.PKZip_method;
      consider: Approach_Filtering;
      --
      procedure Winner_color is
      begin
        if e.info(choice).size <= e.info(original).size then -- normal case
          Put(summary,"<td bgcolor=lightgreen><b>");
        else
          Put(summary,"<td bgcolor=" & lightred & "><b>");
          -- Forced method with less efficient compression
        end if;
      end Winner_color;
      --
    begin
      consider:= always_consider;
      if unique_name = "" or else
        (   unique_name(unique_name'Last)='\'
         or unique_name(unique_name'Last)='/'
        ) then
        return; -- directories are useless entries!
      end if;
      total_choice.count:= total_choice.count + 1;
      Put_Line(deco);
      Put_Line(
        "  Processing " &
        unique_name & ',' &
        Integer'Image(total_choice.count) &
        " of" &
        Integer'Image(Zip.Entries(zi))
      );
      Put_Line(deco);
      New_Line;
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
      Put("    Phase 1:  dump & unzip -");
      Rip_data(
        archive      => zi,
        InputStream  => StreamFile,
        data_name    => unique_name,
        rip_rename   => Temp_name(True,original),
        unzip_rename => Temp_name(False,original),
        case_match   => True,
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
        case_sensitive => True,
        comp_size      => comp_size,
        uncomp_size    => uncomp_size
      );
      Put_Line(" done");
      for a in Approach loop
        if consider(a) then
          --
          -- Apply limitations
          --
          if a = shrink and then uncomp_size > 5000 then
            -- Shrink (LZW) is sometimes better for tiny files, but only them.
            consider(a):= False;
          elsif a in External and then
            ext(a).limit /= 0 and then
            uncomp_size > ext(a).limit
          then
            consider(a):= False;
          end if;
        end if;
      end loop;
      Put_Line("    Phase 2:  try different tactics...");
      --
      Try_all_approaches:
      --
      for a in Approach loop
        if consider(a) then
          Put("              -o-> " & Img(a));
          case a is
            --
            when original =>
              -- This is from the original .zip - just record size and method
              e.info(a).size:= comp_size;
              e.info(a).zfm := e.head.short_info.zip_type;
              mth:= Zip.Method_from_code(e.info(a).zfm);
              --
            when Internal =>
              SetName (StreamFile_in, Temp_name(False,original));
              Open (File_in, In_File);
              SetName (StreamFile_out, Temp_name(True,a));
              Create (File_out, Out_File);
              Zip.Compress.Compress_data
              (
                input            => StreamFile_in,
                output           => StreamFile_out,
                input_size_known => True,
                input_size       => e.head.short_info.dd.uncompressed_size,
                method           => Approach_to_Method(a),
                feedback         => My_Feedback'Access,
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
              New_Line;
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
          if choice = original and
            (
              (deflate_only and not (mth = Zip.store or mth = Zip.deflate)) or
              (fast_decomp and not Is_fast_decomp_method(mth))
            )
          then
            -- This occurs if we want to make a deflate/store only archive
            -- As soon as a /= original, the choice will be forced out of original if
            -- method is not "compatible", even with a worse size
            choice:= a;
          end if;
          New_Line;
        end if;
      end loop Try_all_approaches;
      --
      total_choice.size:= total_choice.size + e.info(choice).size;
      total(choice).count:= total(choice).count + 1;
      total_choice.uncomp_size:=
        total_choice.uncomp_size + Unsigned_64(uncomp_size);
      total(choice).saved:=
        total(choice).saved + Integer_64(e.info(original).size) - Integer_64(e.info(choice).size);
      total_choice.saved:=
        total_choice.saved + Integer_64(e.info(original).size) - Integer_64(e.info(choice).size);
      --
      New_Line;
      Put(
        "    Phase 3:  Winner is " & Img(choice) &
        "; writing data -"
      );
      -- * Summary outputs
      Put(summary,"<tr><td bgcolor=lightgrey><tt>" & unique_name & "</tt></td>");
      for a in Approach loop
        if always_consider(a) then
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
      Put(summary,"<td>" & Img(choice) & "</td>");
      Put(summary,
        "<td bgcolor=#fafa64>" &
        To_Lower(Zip.PKZip_method'Image(Zip.Method_from_code(e.info(choice).zfm))) &
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
      Put_Line(summary,"</td></tr>");
      --
      -- Write winning data:
      --
      e.head.short_info.extra_field_length:= 0; -- We choose to ignore it...
      -- No data descriptor after data:
      e.head.short_info.bit_flag:=
        e.head.short_info.bit_flag and (16#FFFF# - 8);
      -- Set or adjust the pre-data data descriptor:
      -- NB: even if missing pre-data, CRC will have been computed
      --     at least with one internal method
      e.head.short_info.dd.uncompressed_size:= uncomp_size;
      -- Put the winning size and method
      e.head.short_info.dd.compressed_size:= e.info(choice).size;
      e.head.short_info.zip_type:= e.info(choice).zfm;
      e.head.local_header_offset:= Unsigned_32(Index(Streamrepacked_zip_file))-1;
      Zip.Headers.Write(Streamrepacked_zip_file, e.head.short_info);
      String'Write(Streamrepacked_zip_file, S(e.name));
      -- Copy the compressed data
      Copy( Temp_name(True,choice), Streamrepacked_zip_file );
      Put_Line(" done");
      New_Line;
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

  begin -- Repack_contents
    T0:= Clock;
    for a in Approach loop
      if a = original then
        always_consider(a):= True;
      elsif deflate_only then
        always_consider(a):=
          a in External and then ext(a).pkzm = Zip.deflate;
      elsif fast_decomp then
        always_consider(a):=
          a = shrink or
          (a in External and then Is_fast_decomp_method(ext(a).pkzm));
      else
        always_consider(a):= a not in reduce_1..reduce_4;
      end if;
    end loop;
    SetName (StreamFile, orig_name);
    Open (MyStream, In_File);
    Zip.Load( zi, StreamFile, True );

    SetName (Streamrepacked_zip_file, repacked_name);
    Create(repacked_zip_file, Out_File);
    Create(summary, Out_File, log_name);
    Put_Line(summary,
      "<html><head><title>ReZip summary for file "
       & orig_name & "</title></head><body>"
    );
    Put_Line(summary,
      "<h2><a target=_blank href=" & Zip.web &
      ">ReZip</a> summary for file " & orig_name & "</h2>"
    );
    Put_Line(summary,
      "Library version " & Zip.version & " dated " & Zip.reference
    );
    if deflate_only or fast_decomp then
      Put_Line(summary,
        "<br><table border=0 cellpadding=0 cellspacing=0>" &
        "<tr bgcolor=" & lightred &
        "><td><b>An option that filters methods is on, " &
        "result(s) may be sub-optimal.</b></td></tr></table><br>"
      );
    end if;
    Put_Line(summary, "<table border=1 cellpadding=1 cellspacing=1>");
    Put(summary, "<tr bgcolor=lightyellow><td align=right valign=top><b>Approach:</b></td>");
    for a in Approach loop
      if always_consider(a) then
        if a in External then
          ext(a).expanded_options:= ext(a).options;
        end if;
        Put(summary, "<td valign=top>" & Img(a) & "</td>");
      end if;
    end loop;
    Put_Line(summary, "</tr>");
    Put(summary, "<tr bgcolor=lightyellow><td bgcolor=lightgrey valign=bottom><b>File name:</b></td>");
    for a in Approach loop
      if always_consider(a) then
        case a is
          when original =>
            Put(summary, "<td align=right bgcolor=#dddd00>Approach's<br>method/<br>format</td>");
          when Internal =>
            Put(summary, "<td bgcolor=#fafa64>" & To_Lower( Zip.Compress.Compression_Method'Image(Approach_to_Method(a))) & "</td>");
            -- better: the Zip.PKZip_method, in case 2 Compression_Method's produce the same sub-format
          when External =>
            Put(summary, "<td bgcolor=#fafa64>" & To_Lower(Zip.PKZip_method'Image(ext(a).pkzm)) & "</td>");
        end case;
      end if;
    end loop;
    Put_Line(summary,
      "<td><b>Choice</b></td><td bgcolor=#dddd00>Choice's<br>method/<br>format</td><td>Smallest<br>size</td>" &
      "<td>% of<br>original</td><td>% of<br>uncompressed</td></tr>"
    );
    --
    -- 1/ Recompress each file into the new archive:
    --
    Process_all(zi);
    --
    -- 2/ Almost done - write Central Directory:
    --
    ed.central_dir_offset:= Unsigned_32(Index(streamrepacked_zip_file))-1;
    ed.total_entries:= 0;
    ed.central_dir_size:= 0;
    ed.main_comment_length:= 0;
    declare
      comment: constant String:= Zip.Zip_comment(zi);
    begin
      if not del_comment then
        ed.main_comment_length:= comment'Length;
      end if;
      -- Restart at the beginning of the list
      e:= list;
      while e /= null loop
        ed.total_entries:= ed.total_entries + 1;
        Zip.Headers.Write(Streamrepacked_zip_file, e.head);
        String'Write(Streamrepacked_zip_file, S(e.name));
        ed.central_dir_size:=
          ed.central_dir_size +
          Zip.Headers.central_header_length +
          Unsigned_32(e.head.short_info.filename_length);
        e:= e.next;
      end loop;
      ed.disknum:= 0;
      ed.disknum_with_start:= 0;
      ed.disk_total_entries:= ed.total_entries;
      Zip.Headers.Write(Streamrepacked_zip_file, ed);
      if not del_comment then
        String'Write(Streamrepacked_zip_file, comment);
      end if;
    end;
    Close(repacked_zip_file);
    Close(MyStream);
    --
    -- Cleanup
    --
    for a in Approach loop
      if always_consider(a) then
        Ada.Directories.Delete_File( Temp_name(True,a) );
        if a = original then -- also an uncompressed data file to delete
          Ada.Directories.Delete_File( Temp_name(False,a) );
        end if;
      end if;
    end loop;
    -- Report total bytes
    Put(summary,"<tr><td><b>T<small>OTAL BYTES</small></b></td>");
    for a in Approach loop
      if always_consider(a) then
        Put(summary,
          "<td bgcolor=#" & Webcolor(a) & ">" &
          Zip.File_size_type'Image(total(a).size) & "</td>"
        );
      end if;
    end loop;
    Put(summary,
      "<td></td><td></td><td bgcolor=lightgreen><b>" & Zip.File_size_type'Image(total_choice.size) &
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
    Put(summary,"<tr><td><b>T<small>OTAL FILES</small></b></td>");
    for a in Approach loop
      if always_consider(a) then
        Put(summary,
          "<td bgcolor=#" & Webcolor(a) & ">" &
          Integer'Image(total(a).count) & "</td>"
        );
      end if;
    end loop;
    Put(summary,
      "<td></td><td></td><td bgcolor=lightgreen><b>" & Integer'Image(total_choice.count) &
      "</b></td>" &
      "<td>"
    );
    Put_Line(summary, "</td></tr>");
    -- Report total saved bytes per approach
    Put(summary,"<tr><td><b>T<small>OTAL SAVED BYTES</small></b></td>");
    for a in Approach loop
      if always_consider(a) then
        Put(summary,
          "<td bgcolor=#" & Webcolor(a) & ">" &
          Integer_64'Image(total(a).saved) & "</td>"
        );
      end if;
    end loop;
    Put(summary,
      "<td></td><td></td><td bgcolor=lightgreen><b>" & Integer_64'Image(total_choice.saved) &
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
    Put_Line(summary, "</td></tr></table><br>");
    T1:= Clock;
    seconds:= T1-T0;
    Put(summary, "Time elapsed : ");
    Put(summary,  Float( seconds ), 4, 2, 0 );
    Put_Line(summary,  " sec</body></html>");
    Close(summary);
    Put("Time elapsed : ");
    Put( Float( seconds ), 4, 2, 0 );
    Put_Line( " sec");
    Put_Line("All details for " & orig_name & " in " & log_name);
  end Repack_contents;

  function Add_zip_ext(s: String) return String is
  begin
    if Zip.Exists(s) then
      return s;
    else
      return s & ".zip";
      -- Maybe the file doesn't exist, but we tried our best...
    end if;
  end Add_zip_ext;

  function Get_ext(s: String) return String is
    dot: Integer:= s'Last;
  begin
    for i in reverse s'Range loop
      if s(i)='.' then
        dot:= i;
        exit;
      end if;
    end loop;
    if s="" or dot = s'Last then -- no extension in all cases:
      return "zip";              -- "", "xxx." or "xxx"
    else
      return s(dot+1..s'Last);
    end if;
  end Get_ext;

  function Remove_ext(s: String) return String is
    dot: Integer:= s'Last+1;
  begin
    if s = "" then
      return s;
    end if;
    for i in reverse s'Range loop
      if s(i)='.' then
        dot:= i;
        exit;
      end if;
    end loop;
    return s(s'First..dot-1);
    -- "xxx" returned in all cases: "xxx.ext", "xxx." or "xxx"
  end Remove_ext;

begin
  Blurb;
  if Argument_Count = 0 then
    Put_Line("Usage: rezip [options] archive(s)[.zip]");
    New_Line;
    Put_Line("options:  -defl:     repack archive only with Deflate method (most compatible)");
    Put_Line("          -fast_dec: repack archive only with fast decompressing methods");
    Put_Line("          -touch:    set time stamps to now");
    Put_Line("          -lower:    set full file names to lower case");
    Put_Line("          -del_comm: delete comment");
    Put_Line("          -comp:     compare original and repacked archives (paranoid mode)");
    New_Line;
    Put_Line("external packers:");
    New_Line;
    declare
      procedure Display(p: Zipper_specification)  is
        fix: String(1..12):= (others => ' ');
      begin
        Insert(fix,fix'First, S(p.title));
        Put("   " & fix);
        fix:= (others => ' ');
        Insert(fix,fix'First, S(p.name));
        Put_Line(" exe: " & fix & "  URL: " & S(p.URL));
      end Display;
    begin
      for e in External loop
        if e = External'First or else ext(e).name /= ext(External'Pred(e)).name then
          Display(ext(e));
        end if;
      end loop;
      Display(defl_opt);
    end;
    New_Line;
    return;
  end if;
  Flexible_temp_files.Initialize;
  for i in 1..Argument_Count loop
    declare
      arg      : constant String:= Argument(i);
      arg_zip  : constant String:= Add_zip_ext(arg);
      ext      : constant String:= Get_ext(arg_zip);
      arg_nozip: constant String:= Remove_ext(arg_zip);
      arg_rezip: constant String:= arg_nozip & ".repacked." & ext;
      arg_log  : constant String:= arg_nozip & ".ReZip.html";
      info_zip,
      info_rezip : Zip.Zip_info;
    begin
      if arg(arg'First) = '-' or arg(arg'First) = '/' then
        -- Options
        declare
          opt: constant String:= To_Lower(arg(arg'First+1..arg'Last));
        begin
          if opt = "defl" then
            deflate_only:= True;
          elsif opt = "fast_dec" then
            fast_decomp:= True;
          elsif opt = "comp" then
            compare:= True;
          elsif opt = "touch" then
            touch:= True;
          elsif opt = "lower" then
            lower:= True;
          elsif opt = "del_comm" then
            del_comment:= True;
          end if;
        end;
      elsif Zip.Exists(arg_zip) then
        Repack_contents(arg_zip, arg_rezip, arg_log);
        if compare then
          Zip.Load( info_zip, arg_zip );
          Zip.Load( info_rezip, arg_rezip );
          Comp_Zip_Prc(info_zip, info_rezip);
        end if;
      else
        Put_Line("  ** Error: archive not found: " & arg_zip);
      end if;
    end;
  end loop;
  Flexible_temp_files.Finalize;
end ReZip;
