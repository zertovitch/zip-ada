--  Zip_Dir_List shows the list of all directories appearing in a Zip archive file.
--
--  Tool was originally made for answering:
--
--  https://stackoverflow.com/questions/32508443/how-to-get-a-list-of-directories-in-a-zip

with Zip;
with Show_License;

with Ada.Command_Line,
     Ada.Containers.Indefinite_Ordered_Maps,
     Ada.Text_IO;

procedure Zip_Dir_List is

  package Dir_Maps is new Ada.Containers.Indefinite_Ordered_Maps (String, String);

  dir_map : Dir_Maps.Map;

  procedure Record_Directory (the_path : String) is
    use Dir_Maps;
  begin
    if dir_map.Find (the_path) = No_Element then
      dir_map.Insert (the_path, the_path);  --  We store the path as key *and* element
    end if;
  end Record_Directory;

  procedure Record_Directories (n : String) is
  begin
    for i in n'Range loop
      if n (i) = '/' or n (i) = '\' then
        --  NB:  - A Zip file should use '/' as separator, but some may have it wrong...
        --       - The search is compatible with UTF-8 strings.
        Record_Directory (n (n'First .. i));
      end if;
    end loop;
  end Record_Directories;

  procedure Record_directories is new Zip.Traverse (Record_Directories);

  function Try_with_zip (file_name : String) return String is
  begin
    if Zip.Exists (file_name) then
      return file_name;
    else
      return file_name & ".zip";
      --  Maybe the file doesn't exist, but we tried our best...
    end if;
  end Try_with_zip;

  z : Zip.Zip_info;

  use Ada.Command_Line, Ada.Text_IO;

begin
  if Argument_Count < 1 then
    Put_Line ("Zip_Dir_List * Shows the list of all directories appearing in a Zip archive file.");
    Put_Line ("Demo for the Zip-Ada library, by G. de Montmollin");
    Put_Line ("Library version " & Zip.version & " dated " & Zip.reference);
    Put_Line ("URL: " & Zip.web);
    Show_License (Current_Output, "zip.ads");
    Put_Line ("Usage: zip_dir_list archive[.zip]");
    return;
  end if;
  declare
    n : constant String := Try_with_zip (Argument (1));
  begin
    Zip.Load (z, n);
  exception
    when Zip.Archive_open_error =>
      Put ("Can't open archive [" & n & ']'); raise;
  end;
  --
  Record_directories (z);
  --
  --  Show results:
  --
  for d of dir_map loop
    Put_Line (d);
  end loop;
end Zip_Dir_List;
