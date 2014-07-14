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

begin
  if Argument_Count < 2 then
    Put_Line("Comp_Zip * compare two zip archive files, incl. contents");
    Put_Line("Demo for the Zip-Ada library, by G. de Montmollin");
    Put_Line("Library version " & Zip.version & " dated " & Zip.reference );
    Put_Line("URL: " & Zip.web);
    New_Line;
    Put_Line("Usage: comp_zip archive1[.zip] archive2[.zip]");
    return;
  end if;
  for i in 1..2 loop
    declare
      n: constant String:= Try_with_zip(Argument(i));
    begin
      Zip.Load( z(i), n );
    exception
      when Zip.Zip_file_open_error =>
        Put( "Can't open archive [" & n & ']' ); raise;
      when UnZip.Wrong_password      =>
        Put( "Archive has a password" ); raise;
    end;
  end loop;
  --
  Comp_Zip_Prc(z(1), z(2));
  --
end Comp_Zip;
