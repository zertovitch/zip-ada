------------------------------------------------------------------------------
--  File:            Find_Zip.adb
--  Description:     Search a text string in files packed in a zip archive.
--  Author:          Gautier de Montmollin
------------------------------------------------------------------------------

with Ada.Calendar;
with Ada.Characters.Handling;           use Ada.Characters.Handling;
with Ada.Command_Line;                  use Ada.Command_Line;
with Ada.Integer_Text_IO;               use Ada.Integer_Text_IO;
with Ada.Streams;
with Ada.Strings.Fixed;                 use Ada.Strings.Fixed;
with Ada.Text_IO;                       use Ada.Text_IO;

with Zip;
with UnZip.Streams;                     use UnZip.Streams;
with Show_License;

procedure Find_Zip is

  max : constant := 2**10;   --  1024
  str : String (1 .. max);   --  str(1..stl) = string to search
  stl : Natural;  --  string length
  l : Character;  --  last character of the search string

  z : Zip.Zip_info;

  ignore_case : constant Boolean := True;

  procedure Search_1_file_using_output_stream (file_name : String) is
    occ : Natural := 0;
    --  Define a circular buffer
    siz : constant := max;
    type Buffer_range is mod siz;
    buf : array (Buffer_range) of Character := (others => ' ');
    bup : Buffer_range := 0;
    --
    --  We define a local, ad-hoc stream type.
    --
    type Search_stream is new Ada.Streams.Root_Stream_Type with null record;
    --
    overriding procedure Read
      (Self   : in out Search_stream;
       Item   :    out Ada.Streams.Stream_Element_Array;
       Last   :    out Ada.Streams.Stream_Element_Offset) is null;  --  Not used.

    overriding procedure Write
      (Self   : in out Search_stream;
       Item   : in     Ada.Streams.Stream_Element_Array);

    --  Implementation of Write:
    overriding procedure Write
      (Self   : in out Search_stream;
       Item   : in     Ada.Streams.Stream_Element_Array)
    is
      pragma Unreferenced (Self);
      c : Character;
      i : Buffer_range := 0;
      j : Natural;
    begin
      for sei in Item'Range loop
        c := Character'Val (Item (sei));
        if ignore_case then
          c := To_Upper (c);
        end if;
        if c = l then  --  last character do match, search further...
          i := bup;
          j := stl;
          match : loop
            i := i - 1;  --  this loops modulo max: 3, 2, 1, 0, max-1, max-2, ...
            j := j - 1;
            if j = 0 then  --  we survived the whole search string
              occ := occ + 1;
              exit match;
            end if;
            exit match when str (j) /= buf (i);
          end loop match;
        end if;
        buf (bup) := c;
        bup := bup + 1;
      end loop;
    end Write;

    sst : Search_stream;

  begin
    Extract (
      Destination      => sst,
      Archive_Info     => z,
      Entry_Name       => file_name,
      Ignore_Directory => False
    );
    if occ > 0 then
      Put (occ, 5);
      Put_Line (" in [" & To_Lower (file_name) & "]'s contents");
    end if;
  end Search_1_file_using_output_stream;

  --  Old variant using an input stream (memory footprint is uncompressed
  --  size plus fixed amounts: can be large!)

  procedure Search_1_file_using_input_stream (file_name : String) is
    f : Zipped_File_Type;
    s : Stream_Access;
    c : Character;
    occ : Natural := 0;
    --  Define a circular buffer
    siz : constant := max;
    type Buffer_range is mod siz;
    buf : array (Buffer_range) of Character := (others => ' ');
    i, bup : Buffer_range := 0;
    j : Natural;
  begin
    Open (f, z, file_name);
    s := Stream (f);
    while not End_Of_File (f) loop
      Character'Read (s, c);
      if ignore_case then
        c := To_Upper (c);
      end if;
      if c = l then  --  last character do match, search further...
        i := bup;
        j := stl;
        match : loop
          i := i - 1;  --  this loops modulo max: 3, 2, 1, 0, max-1, max-2, ...
          j := j - 1;
          if j = 0 then  --  we survived the whole search string
            occ := occ + 1;
            exit match;
          end if;
          exit match when str (j) /= buf (i);
        end loop match;
      end if;
      buf (bup) := c;
      bup := bup + 1;
    end loop;
    Close (f);
    if occ > 0 then
      Put (occ, 5);
      Put_Line (" in [" & To_Lower (file_name) & "] (inward stream method)");
    end if;
  end Search_1_file_using_input_stream;
  pragma Unreferenced (Search_1_file_using_input_stream);

  procedure Search_all_files is new Zip.Traverse (Search_1_file_using_output_stream);

  procedure Search_in_entry_name (file_name : String) is
    un : String := file_name;
  begin
    if ignore_case then
      un := To_Upper (un);
    end if;
    if Index (un, str (1 .. stl)) > 0 then
      Put_Line (" Found in [" & To_Lower (file_name) & "]'s entry name");
    end if;
  end Search_in_entry_name;

  procedure Search_all_file_names is new Zip.Traverse (Search_in_entry_name);

  function Try_with_zip (file_name : String) return String is
  begin
    if Zip.Exists (file_name) then
      return file_name;
    else
      return file_name & ".zip";
      --  Maybe the file doesn't exist, but we tried our best...
    end if;
  end Try_with_zip;

  procedure Load_Archive_Catalogue is
    n : constant String := Try_with_zip (Argument (1));
  begin
    Zip.Load (z, n);
  exception
    when Zip.Archive_open_error =>
      Put ("Can't open archive [" & n & ']'); raise;
    when UnZip.Wrong_password      =>
      Put ("Archive has a password"); raise;
  end Load_Archive_Catalogue;

  procedure Prepare_Search_String is
    s : String := Argument (2);
  begin
    Put_Line ("Searching string [" & s & "]");
    if ignore_case then
      s := To_Upper (s);
    end if;
    stl := s'Length;
    if stl > str'Length then
      raise Constraint_Error;
    end if;
    str (1 .. stl) := s;
    l := str (stl);
  end Prepare_Search_String;

  procedure Blurb is
  begin
    Put_Line ("Find_Zip * Search a text string in files packed in a zip archive.");
    Put_Line ("Demo for the Zip-Ada library, by G. de Montmollin");
    Put_Line ("Library version " & Zip.version & " dated " & Zip.reference);
    Put_Line ("URL: " & Zip.web);
    Show_License (Current_Output, "zip.ads");
    Put_Line ("Usage: find_zip archive[.zip] [""]text[""]");
  end Blurb;

  T0, T1, T2 : Ada.Calendar.Time;
  use Ada.Calendar;

begin
  if Argument_Count < 2 then
    Blurb;
    return;
  end if;
  T0 := Clock;
  Load_Archive_Catalogue;
  Prepare_Search_String;
  T1 := Clock;
  Search_all_files (z);
  Search_all_file_names (z);
  T2 := Clock;
  Put_Line
    ("Time elapsed :" & Duration'Image (T2 - T0) &
     " seconds (loading catalogue: " & Duration'Image (T1 - T0) & ").");
end Find_Zip;
