--  Zip_Dir_List shows the list of all directories appearing in a Zip archive file.
--
--  Tool originally made for answering:
--
--  https://stackoverflow.com/questions/32508443/how-to-get-a-list-of-directories-in-a-zip

with Zip;
with Show_License;

with Ada.Command_Line;                  use Ada.Command_Line;
with Ada.Containers.Ordered_Maps;
with Ada.Strings.Unbounded;             use Ada.Strings.Unbounded;
with Ada.Text_IO;                       use Ada.Text_IO;

procedure Zip_Dir_List is

  function S (Source : Ada.Strings.Unbounded.Unbounded_String) return String
    renames Ada.Strings.Unbounded.To_String;
  function U (Source : String) return Ada.Strings.Unbounded.Unbounded_String
    renames Ada.Strings.Unbounded.To_Unbounded_String;

  package Dir_maps is new Ada.Containers.Ordered_Maps (
    Key_Type     => Unbounded_String,
    Element_Type => Unbounded_String,
    "<"          => "<",
    "="          => "="
  );

  dir_map : Dir_maps.Map;

  procedure Record_directory (n : String) is
    sep : Natural := 0;
  begin
    for i in n'Range loop
      if n (i) = '/' or n (i) = '\' then
        --  NB:  - a Zip file should use '/' as separator, but some may have it wrong...
        --       - compatible with UTF-8 strings
        --
        sep := i;
      end if;
    end loop;
    if sep > 0 then
      declare
        the_path : constant Unbounded_String := U (n (n'First .. sep));
        use Dir_maps;
      begin
        if dir_map.Find (the_path) = No_Element then
          dir_map.Insert (the_path, the_path);  --  We store the path as key *and* element
        end if;
      end;
    end if;
  end Record_directory;

  procedure Record_directories is new Zip.Traverse (Record_directory);

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
  Record_directories (z);
  --
  --  Show results
  --
  for d of dir_map loop
    Put_Line (S (d));
  end loop;
end Zip_Dir_List;
