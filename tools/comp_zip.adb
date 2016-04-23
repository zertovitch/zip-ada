------------------------------------------------------------------------------
--  File:            Comp_Zip.adb
--  Description:     A zip comparison tool using Zip-Ada lib.
--                   Core moved to Comp_Zip_Prc (22-Sep-2009)
--  Author:          Gautier de Montmollin
------------------------------------------------------------------------------

with Zip, UnZip, Comp_Zip_Prc;

with Ada.Command_Line;                  use Ada.Command_Line;
with Ada.Text_IO;                       use Ada.Text_IO;

procedure Comp_Zip is
  z: array(1..2) of Zip.Zip_info;

  function Try_with_zip(name: String) return String is
  begin
    if Zip.Exists(name) then
      return name;
    else
      return name & ".zip";
      -- Maybe the file doesn't exist, but we tried our best...
    end if;
  end Try_with_zip;

  quiet: Natural:= 0;

  procedure Blurb is
  begin
    Put_Line("Comp_Zip * compare two zip archive files, incl. contents");
    Put_Line("Demo for the Zip-Ada library, by G. de Montmollin");
    Put_Line("Library version " & Zip.version & " dated " & Zip.reference );
    Put_Line("URL: " & Zip.web);
    New_Line;
  end Blurb;

begin
  if Argument_Count < 2 then
    Blurb;
    Put_Line("Usage: comp_zip archive1[.zip] archive2[.zip] [options]");
    New_Line;
    Put_Line("Options: -q1: (quiet level 1): summary only");
    Put_Line("         -q2: (quiet level 2): shorter summary only");
    return;
  end if;
  for i in 1..2 loop
    declare
      n: constant String:= Try_with_zip(Argument(i));
    begin
      Zip.Load( z(i), n );
    exception
      when Zip.Zip_file_open_Error =>
        Put( "Can't open archive [" & n & ']' ); raise;
      when UnZip.Wrong_password      =>
        Put( "Archive has a password" ); raise;
    end;
  end loop;
  for a in 3 .. Argument_Count loop
    declare
      arg: String renames Argument(a);
    begin
      if arg'Length > 2 and then arg(arg'First..arg'First+1) = "-q" then
        quiet:= Natural'Value(arg(arg'First+2..arg'Last));
      end if;
    end;
  end loop;
  --
  if quiet = 0 then
    Blurb;
  end if;
  Comp_Zip_Prc(z(1), z(2), quiet);
  --
end Comp_Zip;
