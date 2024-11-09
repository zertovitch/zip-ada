------------------------------------------------------------------------------
--  File:            Comp_Zip.adb
--  Description:     A zip comparison tool using Zip-Ada lib.
--                   Core moved to Comp_Zip_Prc (22-Sep-2009)
--  Author:          Gautier de Montmollin
------------------------------------------------------------------------------

with Zip, UnZip, Comp_Zip_Prc;
with Show_License;

with Ada.Calendar,
     Ada.Command_Line,
     Ada.Strings.Unbounded,
     Ada.Text_IO;

procedure Comp_Zip is
  z : array (1 .. 2) of Zip.Zip_Info;

  function Try_with_zip (zip_name : String) return String is
  begin
    if Zip.Exists (zip_name) then
      return zip_name;
    else
      return zip_name & ".zip";
      --  Maybe the file doesn't exist, but we tried our best...
    end if;
  end Try_with_zip;

  use Ada.Text_IO;

  procedure Blurb is
  begin
    Put_Line ("Comp_Zip * compare two zip archive files, including their contents");
    Put_Line ("Demo for the Zip-Ada library, by G. de Montmollin");
    Put_Line ("Library version " & Zip.version & " dated " & Zip.reference);
    Put_Line ("URL: " & Zip.web);
    Show_License (Current_Output, "zip.ads");
  end Blurb;

  use Ada.Calendar, Ada.Command_Line, Ada.Strings.Unbounded;

  quiet    : Natural := 0;
  password : Unbounded_String;
  total_differences : Natural;
  T0, T1, T2 : Time;

begin
  if Argument_Count < 2 then
    Blurb;
    Put_Line ("Usage: comp_zip archive1[.zip] archive2[.zip] [options]");
    New_Line;
    Put_Line ("Options: -q1   : (quieter level 1): summary only");
    Put_Line ("         -q2   : (quieter level 2): shorter summary only");
    Put_Line ("         -q3   : (quieter level 3): 1 line per archive");
    Put_Line ("         -q4   : (quieter level 4): no console output, only exit code");
    Put_Line ("         -pPwd : define a password for decryption (e.g. ""Pwd"")");
    New_Line;
    Put_Line ("The exit code is the number of mismatching entries:");
    Put_Line ("  - entry name is in both archives, but contents are different (+1)");
    Put_Line ("  - entry name is only in one archive but absent in the other one (+1)");
    Put_Line ("The exit code 0 means that both archives have identical contents.");
    New_Line;
    Put ("Press Return");
    Skip_Line;
    return;
  end if;
  T0 := Clock;
  for i in 1 .. 2 loop
    declare
      n : constant String := Try_with_zip (Argument (i));
    begin
      Zip.Load (z (i), n);
    exception
      when Zip.Archive_open_error =>
        Put ("Can't open archive [" & n & ']'); raise;
      when UnZip.Wrong_password      =>
        Put ("Archive has a password"); raise;
    end;
  end loop;
  for a in 3 .. Argument_Count loop
    declare
      arg : String renames Argument (a);
    begin
      if arg'Length > 2 and then arg (arg'First .. arg'First + 1) = "-q" then
        quiet := Natural'Value (arg (arg'First + 2 .. arg'Last));
      elsif arg'Length > 2 and then arg (arg'First .. arg'First + 1) = "-p" then
        password := To_Unbounded_String (arg (arg'First + 2 .. arg'Last));
      end if;
    end;
  end loop;
  --
  if quiet = 0 then
    Blurb;
  end if;
  T1 := Clock;
  Comp_Zip_Prc (z (1), z (2), quiet, To_String (password), total_differences);
  T2 := Clock;
  if quiet < 3 then
    Put_Line
      ("Time elapsed :" & Duration'Image (T2 - T0) &
       " seconds (loading catalogues: " & Duration'Image (T1 - T0) & ").");
  end if;
  Set_Exit_Status (Exit_Status (total_differences));
  --
end Comp_Zip;
