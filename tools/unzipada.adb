------------------------------------------------------------------------------
--  File:            UnZipAda.adb
--  Description:     A minimal standalone command-line unzipping tool
--                     using the Zip-Ada library.
--  Author:          Gautier de Montmollin
------------------------------------------------------------------------------

with Ada.Characters.Handling;           use Ada.Characters.Handling;
with Ada.Command_Line;                  use Ada.Command_Line;
with Ada.Calendar;                      use Ada.Calendar;
with Ada.Text_IO;                       use Ada.Text_IO;
with Ada.Float_Text_IO;                 use Ada.Float_Text_IO;
with Interfaces;                        use Interfaces;

with Ada.Directories;  --  Ada 2005

--  Non-standard item absent from Ada.Directories
--  with Set_Modification_Time_GNAT;  --  This procedure is system-dependent

with Zip, UnZip;

--  Pure Ada Text_IO-fashion feedback; should work on every
--  computer having a screen [and some text console too] :

with Zip_Console_IO;
with Show_License;

procedure UnZipAda is

  procedure Set_Modification_Time_B (Name : in String;
                                     To   : in Ada.Calendar.Time) is
  begin
    null;  --  If you want the time stamps, uncomment the following and the "with" above.
    --  Set_Modification_Time_GNAT (Name, To);
  exception
    when others =>
      null; -- !! utf-8 or ascii names with characters > pos 127 fail
  end Set_Modification_Time_B;
  pragma Unreferenced (Set_Modification_Time_B);

  Set_Time_Stamp : constant UnZip.Set_Time_Stamp_proc :=
    --    If you want the time stamps, uncomment the following
    --    and look into Set_Modification_Time_B above.
    --
    --  Set_Modification_Time_B'Unrestricted_Access;
    null;

  use UnZip;

  z_options        : UnZip.Option_set := UnZip.no_option;
  quiet            : Boolean := False;
  lower_case_match : Boolean := False;
  comment          : Boolean := False;

  fda :          Zip.Feedback_proc     := Zip_Console_IO.My_feedback'Access;
  rca :          Resolve_conflict_proc := Zip_Console_IO.My_resolve_conflict'Access;
  tda :          Tell_data_proc        := Zip_Console_IO.My_tell_data'Access;
  gpw : constant Get_password_proc     := Zip_Console_IO.My_get_password'Access;

  last_option : Natural := 0;

  password, exdir : String (1 .. 1024);
  pass_len, exdir_len : Natural := 0;

  Directory_Separator : constant Character := '/';
  --  '/' is also accepted by Windows

  function Add_extract_directory (File_Name : String) return String is
    --  OK for UNIX & Windows, but VMS has "[x.y.z]filename.ext"
  begin
    if exdir_len = 0 then
      return File_Name;
    elsif exdir (exdir_len) = '\' or exdir (exdir_len) = '/' then
      return exdir (1 .. exdir_len) & File_Name;
    else
      return exdir (1 .. exdir_len) & Directory_Separator & File_Name;
    end if;
  end Add_extract_directory;

  function Compose_File_Name (
    File_Name     : String;
    Name_encoding : Zip.Zip_name_encoding
  )
  return String
  is
  pragma Unreferenced (Name_encoding);
    fn1 : String := File_Name;
  begin
    if lower_case_match then
      fn1 := To_Lower (fn1);
    end if;
    return Add_extract_directory (fn1);
  end Compose_File_Name;

  My_FS_routines : constant FS_routines_type :=
    (Create_Path         => Ada.Directories.Create_Path'Access,  --  Ada 2005
     Set_Time_Stamp      => Set_Time_Stamp,
     Compose_File_Name   => Compose_File_Name'Unrestricted_Access,
     others              => null
    );

  T0, T1 : Time;
  seconds_elapsed : Duration;

  package IIO is new Integer_IO (Integer);
  package MIO is new Modular_IO (Zip.Zip_64_Data_Size_Type);

  procedure Blurb is
  begin
    Put_Line ("UnZipAda * minimal standalone unzipping tool");
    Put_Line ("Demo for the Zip-Ada library, by G. de Montmollin");
    Put_Line ("Library version " & Zip.version & " dated " & Zip.reference);
    Put_Line ("URL: " & Zip.web);
    Show_License (Current_Output, "zip.ads");
  end Blurb;

  procedure Help is
  begin
    Blurb;
    Put_Line ("Usage: unzipada [options] zipfile[.zip] [files...]");
    New_Line;
    Put_Line ("options:  -t     : test .zip file integrity, no write");
    Put_Line ("          -j     : junk archived directory structure");
    Put_Line ("          -d dir : extract to ""dir"" instead of current");
    Put_Line ("          -c     : case sensitive name matching");
    Put_Line ("          -l     : force lower case on stored names");
    Put_Line ("          -a     : output as text file, with native line endings");
    Put_Line ("          -z     : display .zip archive comment only");
    Put_Line ("          -p Pwd : define a password for decryption (e.g. ""Pwd"")");
    Put_Line ("          -q     : quiet mode");
  end Help;

  zi : Zip.Zip_info;
  use Zip_Console_IO;

begin
  if Argument_Count = 0 then
    Help;
    return;
  end if;
  for i in 1 .. Argument_Count loop
    if Argument (i)(1) = '-' or else Argument (i)(1) = '/' then
      if last_option = i then
        null; -- was in fact an argument for previous option (e.g. "-s")
      else
        last_option := i;
        if Argument (i)'Length = 1 then
          Help;
          return;
        end if;
        case To_Lower (Argument (i)(2)) is
          when 't' =>
            z_options (test_only) := True;
          when 'j' =>
            z_options (junk_directories) := True;
          when 'd' =>
            if i = Argument_Count then
              Help;
              return;  --  "-d" without the directory or anything ?!
            end if;
            declare
              arg_exdir : constant String := Argument (i + 1);
            begin
              exdir (1 .. arg_exdir'Length) := arg_exdir;
              exdir_len := arg_exdir'Length;
            end;
            last_option := i + 1;
          when 'c' =>
            z_options (case_sensitive_match) := True;
          when 'l' =>
            lower_case_match := True;
          when 'a' =>
            z_options (extract_as_text) := True;
          when 'p' | 's' =>  --  The "-s" variant is kept for compatibility.
            if i = Argument_Count then
              Help;
              return; -- "-s" without the password or anything ?!
            end if;
            declare
              arg_pass : constant String := Argument (i + 1);
            begin
              password (1 .. arg_pass'Length) := arg_pass;
              pass_len := arg_pass'Length;
            end;
            last_option := i + 1;
          when 'q' =>
            quiet := True;
          when 'z' =>
            comment := True;
          when others  =>
            Help;
            return;
        end case;
      end if;
    end if;
  end loop;

  current_user_attitude := yes;

  if quiet then
    fda := null;
    rca := null;
    tda := null;
  end if;

  Zip_Console_IO.Summary.Reset;

  if Argument_Count = last_option then -- options only ?!
    Help;
    return;
  end if;
  declare
    archive_given : constant String := Argument (last_option + 1);
    zip_ext : Boolean := False;
    extract_all : Boolean;
    --
    function Archive return String is
    begin
      if zip_ext then
        return archive_given & ".zip";
      else
        return archive_given;
      end if;
    end Archive;
    --
  begin
    if not Zip.Exists (Archive) then
      zip_ext := True;
      if not Zip.Exists (Archive) then
        Put_Line ("Archive file '" & archive_given &
                  "' or '" & Archive & "' not found");
        return;
      end if;
    end if;
    extract_all := Argument_Count = last_option + 1;
    --  options and zipfile only

    if not quiet then
      Blurb;
    end if;
    if not (quiet or comment) then
      if z_options (test_only) then
        Put ("Testing");
      else
        if Set_Time_Stamp = null then
          Put_Line (" Warning: time stamps and attributes of files" &
                    " in archive are not reproduced !");
          New_Line;
        end if;
        Put ("Extracting");
      end if;
      if not extract_all then
        Put (" some file(s) from");
      end if;
      Put_Line (" archive " & Archive);
    end if;

    T0 := Clock;
    if comment then  --  Option: -z , display comment only
      Zip.Load (zi, Archive);
      Zip.Put_Multi_Line (Standard_Output, Zip.Zip_comment (zi));
    elsif extract_all then
      Extract (
        Archive,
        fda, rca, tda, gpw,
        z_options,
        password (1 .. pass_len),
        My_FS_routines
      );
    else
      Zip.Load (zi, Archive);
      for i in last_option + 2 .. Argument_Count loop
        Extract (zi, Argument (i),
          fda, rca, tda, gpw,
          z_options,
          password (1 .. pass_len),
          My_FS_routines
        );
      end loop;
    end if;
    T1 := Clock;
  end;

  seconds_elapsed := T1 - T0;

  if not (quiet or comment) then
    New_Line (2);
    IIO.Put (Summary.total_entries, 7);
    Put (" entries  ------ Total ------ ");
    MIO.Put (Summary.total_compressed, 10);
    if Summary.total_uncompressed = 0 then
      Put (" :         ");
    else
      Put (" :");
      IIO.Put (
        Natural (
          (100.0 * Long_Float (Summary.total_compressed)) /
          Long_Float (Summary.total_uncompressed)
        ), 4);
      Put ("% of ");
    end if;
    MIO.Put (Summary.total_uncompressed, 10);
    New_Line (2);

    if z_options (test_only) then
      Put_Line ("Test: no error found");
      New_Line;
      Put_Line ("Statistics per Zip sub-format (""method""):");
      for m in Summary.files_per_method'Range loop
        if Summary.files_per_method (m) > 0 then
          Put ("  " & Summary.Nice_image (m) & "... ");
          IIO.Put (Summary.files_per_method (m), 5);
          Put (" files");
          if Summary.uncompressed_per_method (m) > 0 then
            Put (",");
            IIO.Put (
              Natural (
                (100.0 * Long_Float (Summary.uncompressed_per_method (m))) /
                Long_Float (Summary.total_uncompressed)
              ), 4
            );
            Put ("% of all data; compr.-to-decompr. ratio: ");
            IIO.Put (
              Natural (
                (100.0 * Long_Float (Summary.compressed_per_method (m))) /
                Long_Float (Summary.uncompressed_per_method (m))
              ), 4
            );
            Put ('%');
          end if;
          New_Line;
        end if;
      end loop;
      New_Line;
    end if;

    Put ("Time elapsed : ");
    Put (Float (seconds_elapsed), 4, 2, 0);
    Put_Line (" sec");

    Put_Line ("Archive successfully processed (or empty archive, or no archive!)");
  end if;

end UnZipAda;
