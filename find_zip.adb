------------------------------------------------------------------------------
--  File:            Find_Zip.adb
--  Description:     Search a text string in files packed in a zip archive.
--  Date/version:    3-Feb-2009
--  Author:          Gautier de Montmollin
------------------------------------------------------------------------------

with Ada.Command_Line;                  use Ada.Command_Line;
with Ada.Text_IO;                       use Ada.Text_IO;
with Ada.Integer_Text_IO;               use Ada.Integer_Text_IO;
with Ada.Characters.Handling;           use Ada.Characters.Handling;

with Zip;
with UnZip.Streams;                     use UnZip.Streams;

procedure Find_Zip is

  max: constant:= 2**10; -- 1024
  str: String(1..max);  -- str(1..stl) = string to search
  stl: Natural; -- string length
  l: Character; -- last character of the search string

  z: Zip.Zip_info;

  ignore_case: constant Boolean:= True;

  procedure Search_1_file( name: String ) is
    f: Zipped_File_Type;
    s: Stream_Access;
    c: Character;
    occ: Natural:= 0;
    -- Define a circular buffer
    siz: constant:= max;
    type Buffer_range is mod siz;
    buf: array(Buffer_range) of Character:= (others => ' ');
    i, bup: Buffer_range:= 0;
    j: Natural;
  begin
    Open( f, z, name );
    s:= Stream(f);
    while not End_of_file(f) loop
      Character'Read(s,c);
      if ignore_case then
        c:= To_Upper(c);
      end if;
      if c = l then -- last character do match, search further...
        i:= bup;
        j:= stl;
        match: loop
          i:= i-1;
          j:= j-1;
          if j = 0 then -- we survived the whole search string
            occ:= occ+1;
            exit match;
          end if;
          exit match when str(j) /= buf(i);
        end loop match;
      end if;
      buf(bup):= c;
      bup:= bup+1;
    end loop;
    Close(f);
    if occ > 0 then
      Put(occ, 5);
      Put_Line(" in [" & To_Lower(name) & "]");
    end if;
  end Search_1_file;

  procedure Search_all_files is new Zip.Traverse( Search_1_file );

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
    Put_Line("Find_Zip * Search a text string in files packed in a zip archive.");
    Put_Line("Demo for the Zip-Ada library, by G. de Montmollin");
    Put_Line("Library version " & Zip.version & " dated " & Zip.reference );
    Put_Line("URL: " & Zip.web);
    New_Line;
    Put_Line("Usage: find_zip archive[.zip] [""]text[""]");
    return;
  end if;
  declare
    n: constant String:= Try_with_zip(Argument(1));
  begin
    Zip.Load( z, n );
  exception
    when Zip.Zip_file_open_error =>
      Put( "Can't open archive [" & n & ']' ); raise;
    when UnZip.Wrong_password      =>
      Put( "Archive has a password" ); raise;
  end;
  declare
    s: String:= Argument(2);
  begin
    Put_Line("Searching string [" & s & "]");
    if ignore_Case then
      s:= To_Upper(s);
    end if;
    stl:= s'Length;
    if stl > str'Length then
      raise Constraint_Error;
    end if;
    str(1..stl):= s;
    l:= str(stl);
  end;
  Search_all_files(z);
end Find_Zip;
