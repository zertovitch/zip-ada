------------------------------------------------------------------------------
--  File:            ZipAda.adb
--  Description:     A minimal standalone command-line zip archiving utility
--                     using the Zip-Ada library.
--  Author:          Gautier de Montmollin
------------------------------------------------------------------------------
--  Important changes:
--
--  ZA v. 49: password can be set
--  ZA v. 28: uses the Zip.Create package
--  ZA v. 26: modified for the new Zip_Stream package

with Ada.Calendar;                      use Ada.Calendar;
with Ada.Command_Line;                  use Ada.Command_Line;
with Ada.Directories;                   use Ada.Directories;
with Ada.Text_IO;                       use Ada.Text_IO;
with Ada.Wide_Text_IO;
with Ada.Float_Text_IO;                 use Ada.Float_Text_IO;
with Ada.Strings.Fixed;                 use Ada.Strings.Fixed;
with Ada.Strings.UTF_Encoding.Conversions;
with Ada.Strings.Unbounded;             use Ada.Strings.Unbounded;
with Ada.Strings.Wide_Fixed;
with Ada.Characters.Handling;           use Ada.Characters.Handling;
with Interfaces;

with Zip_Streams;                       use Zip_Streams;
with Zip.Compress, Zip.Create;          use Zip.Create;

with Zip_Console_IO;
with Show_License;

procedure ZipAda is

  T0, T1 : Ada.Calendar.Time;
  seconds_elapsed : Duration;

  procedure Blurb is
  begin
    Put_Line ("ZipAda * minimalistic standalone zipping tool.");
    Put_Line ("Demo for Zip-Ada library, by G. de Montmollin");
    Put_Line ("Library version " & Zip.version & " dated " & Zip.reference);
    Put_Line ("URL: " & Zip.web);
    Show_License (Current_Output, "zip.ads");
  end Blurb;

  function Cut_name (n : Wide_String; l : Natural) return Wide_String is
    dots : constant Wide_String := "...";
  begin
    if n'Length > l then
      return dots & n (n'Last - (l - 1) + dots'Length .. n'Last);
    else
      return n;
    end if;
  end Cut_name;

  --  Final zipfile stream
  MyStream : aliased File_Zipstream;
  Info : Zip_Create_Info;
  password, password_confirm : Unbounded_String;

  procedure Add_1_Stream (Stream : in out Root_Zipstream_Type'Class) is
    Compressed_Size : Zip.Zip_64_Data_Size_Type;
    Final_Method    : Natural;
    use Interfaces;
  begin
    Put ("  Adding ");
    declare
      maxlen : constant := 24;
      Unicode_name : constant Wide_String :=
        Ada.Strings.UTF_Encoding.Conversions.Convert (Get_Name (Stream));
      cut : constant Wide_String := Cut_name (Unicode_name, maxlen);
      use Ada.Strings.Wide_Fixed;
    begin
      Ada.Wide_Text_IO.Put (cut & (1 + maxlen - cut'Length) * ' ');
    end;
    --
    Add_Stream
      (Info, Stream,
       Zip_Console_IO.My_feedback'Access,
       To_String (password), Compressed_Size, Final_Method);
    --
    if Size (Stream) = 0 then
      Put ("          ");
    end if;
    Put (' ');
    declare
      meth : constant String := Zip.Image (Zip.Method_from_code (Final_Method));
    begin
      Put (meth & (Zip.PKZip_method'Width - meth'Length) * ' ');
    end;
    if Size (Stream) > 0 then
      Put (", to ");
      Put (100.0 * Float (Compressed_Size) / Float (Size (Stream)), 3, 2, 0);
      Put ('%');
    end if;
    Put_Line (", done.");
  end Add_1_Stream;

  function Add_zip_ext (s : String) return String is
  begin
    if s'Length < 4 or else
       To_Upper (s (s'Last - 3 .. s'Last)) /= ".ZIP"
    then
      return s & ".zip";
    else
      return s;
    end if;
  end Add_zip_ext;

  use Zip.Compress;

  method : Compression_Method := Deflate_1;
  zip_name_set : Boolean := False;

  procedure Zip_a_file (arg : String) is
    InStream : File_Zipstream;
  begin
    Set_Name (InStream, arg);
    Set_Time (InStream, Ada.Directories.Modification_Time (arg));
    Set_Unicode_Name_Flag (InStream, True);
    Open (InStream, In_File);
    Add_1_Stream (InStream);
    Close (InStream);
  exception
    when Ada.Text_IO.Use_Error =>
      Put_Line ("  ** Warning: skipping invalid entry: " & arg);
  end Zip_a_file;

  len : Natural := 0;  --  absolute directory prefix, to be skipped.

  --  Recursive directory scan expanded from this example:
  --
  --  http://rosettacode.org/wiki/Walk_a_directory/Recursively#Ada

  procedure Walk (Directory_Or_File_Name : String; Pattern : String; Level : Natural; Recursive : Boolean) is
    --
    procedure Process_file (Item : Directory_Entry_Type) is
    begin
      if Simple_Name (Item) /= "." and then Simple_Name (Item) /= ".." then
        declare
          fn : constant String := Full_Name (Item);
        begin
          Zip_a_file (fn (fn'First + len .. fn'Last));
        end;
      end if;
    end Process_file;
    --
    procedure Walk_subdirectory (Item : Directory_Entry_Type) is
    begin
      if Simple_Name (Item) /= "." and then Simple_Name (Item) /= ".." then
        Walk (Full_Name (Item), Pattern, Level + 1, True);
      end if;
    exception
      when Ada.Directories.Name_Error => null;
    end Walk_subdirectory;
    --
  begin
    if Level = 0 then  --  Figure out the length of the absolute path
      len := Full_Name (".")'Length + 1;
    end if;
    --  Process files
    Search (Directory_Or_File_Name, Pattern, (Directory => False, others => True), Process_file'Access);
    --  Process subdirectories
    if Recursive then
      Search (Directory_Or_File_Name, "", (Directory => True, others => False), Walk_subdirectory'Access);
    end if;
  exception
    when Ada.Directories.Name_Error => -- "unknown directory" -> probably a file.
      if Level = 0 then
            if Zip.Exists (Directory_Or_File_Name) then
              Zip_a_file (Directory_Or_File_Name);
            else
              Put_Line ("  ** Warning [a]: name not matched: " & Directory_Or_File_Name);
            end if;
        Zip_a_file (Directory_Or_File_Name);
      end if;
  end Walk;

  type Scan_mode is (
    files_only,
    files_and_dirs,
    files_and_dirs_recursive,
    patterns_recursive
  );
  scan : Scan_mode := files_only;

  procedure Enter_password (title : String; pwd : out Unbounded_String) is
    c : Character;
  begin
    Put_Line (title);
    loop
      Get_Immediate (c);
      exit when c < ' ';
      pwd := pwd & c;
    end loop;
  end Enter_password;

  Wrong_password, Overwrite_disallowed : exception;

  procedure Process_argument (i : Positive) is
    arg     : constant String := Argument (i);
    arg_zip : constant String := Add_zip_ext (arg);
    answer  : Character;
  begin
    if arg (arg'First) = '-' or arg (arg'First) = '/' then
      --  Options
      declare
        --  Spaces to avoid too short slices
        opt : constant String := arg (arg'First + 1 .. arg'Last) & "    ";
        eX  : constant String := opt (opt'First .. opt'First + 1);
      begin
        if eX = "e0" then
          method := Store;
        elsif eX = "er" then
          case opt (opt'First + 2) is
            when '1'    => method := Reduce_1;
            when '2'    => method := Reduce_2;
            when '3'    => method := Reduce_3;
            when others => method := Reduce_4;
          end case;
        elsif eX = "es" then
          method := Shrink;
        elsif eX = "ed" then
          case opt (opt'First + 2) is
            when 'f'    => method := Deflate_Fixed;
            when '0'    => method := Deflate_0;
            when '1'    => method := Deflate_1;
            when '2'    => method := Deflate_2;
            when 'r'    => method := Deflate_R;
            when others => method := Deflate_3;
          end case;
        elsif eX = "el" then
          case opt (opt'First + 2) is
            when '0'    => method := LZMA_0;
            when '1'    => method := LZMA_1;
            when '2'    => method := LZMA_2;
            when others => method := LZMA_3;
          end case;
        elsif eX = "ep" then
          case opt (opt'First + 2) is
            when '1'    => method := Preselection_1;
            when others => method := Preselection_2;
          end case;
        elsif opt (opt'First .. opt'First + 3) = "dir " then
          scan := Scan_mode'Max (scan, files_and_dirs);
        elsif eX = "r " then
          scan := files_and_dirs_recursive;
        elsif eX = "r2" then
          scan := patterns_recursive;
        elsif opt (opt'First) = 'p' or opt (opt'First) = 's' then
          --  The "-s" variant is kept for compatibility.
          if arg'Length > 2 then  --  Password is appended to the option
            password := To_Unbounded_String (arg (arg'First + 2 .. arg'Last));
          else
            Enter_password ("Enter password", password);
            Enter_password ("Confirm password", password_confirm);
            if password /= password_confirm then
              Put_Line ("Password mismatch.");
              raise Wrong_password;
            end if;
          end if;
        end if;
      end;
    elsif not zip_name_set then
      zip_name_set := True;
      if Zip.Exists (arg_zip) then
        Put ("Archive " & arg_zip & " already exists! Overwrite (y/n) ?");
        Get_Immediate (answer);
        answer := To_Upper (answer);
        Put_Line (" -> " & answer);
        if answer /= 'Y' then
          Put_Line ("Stopped.");
          raise Overwrite_disallowed;
        end if;
      end if;
      Put_Line ("Creating archive " & arg_zip);
      Put_Line ("Method: " & Compression_Method'Image (method));
      T0 := Clock;
      Create_Archive (Info, MyStream'Unchecked_Access, arg_zip, method, Zip.error_on_duplicate);
    else -- First real argument has already been used for archive's name
      if To_Upper (arg) = To_Upper (Name (Info)) then
        Put_Line ("  ** Warning: skipping archive's name as entry: " & arg);
        --  avoid zipping the archive itself!
        --  NB: case insensitive
      else
        case scan is
          when files_only =>
            if Zip.Exists (arg) then
              Zip_a_file (arg);
            else
              Put_Line ("  ** Warning [b]: name not matched: " & arg);
            end if;
          when files_and_dirs =>
            Walk (arg, "*", 0, False);
          when files_and_dirs_recursive =>
            Walk (arg, "*", 0, True);
          when patterns_recursive =>
            Walk (".", arg, 0, True);
        end case;
      end if;
    end if;
  end Process_argument;

begin
  Blurb;
  --  Set the file name encoding as UTF-8.
  --  NB: GNAT (as of version CE 2019) doesn't seem to need it.
  Zip_Streams.Form_For_IO_Open_and_Create := To_Unbounded_String ("encoding=utf8");
  --
  for i in 1 .. Argument_Count loop
    Process_argument (i);
  end loop;
  --
  --  We are done, or no archive was created.
  --
  if Is_Created (Info) then
    Finish (Info);
    T1 := Clock;
    seconds_elapsed := T1 - T0;
    Put ("Time elapsed : ");
    Put (Float (seconds_elapsed), 4, 2, 0);
    Put_Line (" sec");
  else
    Put_Line ("Usage: zipada [options] archive[.zip] name(s)");
    New_Line;
    Put_Line ("Options:  -e0    : ""Store"": zero compression, archiving only (like tar)");
    Put_Line ("          -erN   : ""Reduce"" 2-pass method, factor N = 1..4");
    Put_Line ("          -es    : ""Shrink"" method (LZW algorithm)");
    Put_Line ("          -edf   : ""Deflate"" method, with one ""fixed"" block (weak)");
    Put_Line ("          -edN   : ""Deflate"" method, ""dynamic"" compression, strength N = 0..3");
    Put_Line ("          -elN   : ""LZMA"" method, strength N = 0..3");
    Put_Line ("          -epN   : preselection of an appropriate method, strength N = 1..2");
    New_Line;
    Put_Line ("      NB: default method is ""Deflate"", strength 1 (-ed1)");
    New_Line;
    Put_Line ("          -dir   : name(s) may be also directories,");
    Put_Line ("                      whose entire contents will be archived");
    Put_Line ("          -r     : same as ""-dir"", but recursively archives full subdirectories");
    Put_Line ("                      of the named directories as well");
    Put_Line ("          -r2    : search name(s) in current and all subdirectories as well;");
    Put_Line ("                      please enclose name(s) that have wildcards with");
    Put_Line ("                      single quotes, for example: '*.adb'");
    Put_Line ("          -p     : define a password for encryption (user is prompted)");
    Put_Line ("          -pPwd  : define a password for encryption (e.g. ""Pwd"")");
  end if;
end ZipAda;
