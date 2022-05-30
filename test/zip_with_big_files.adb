------------------------------------------------------------------------------
--  File:            Zip_with_big_files.adb
--  Description:     Demo/test: test 4 GiB limit of the Zip_32 archive
--                     format and the automatic promotion to Zip_64.
--  Author:          Gautier de Montmollin
------------------------------------------------------------------------------

with Zip.Compress;
with Zip.Create;                        use Zip.Create;

with Ada.Integer_Text_IO;               use Ada.Integer_Text_IO;
with Ada.Strings.Fixed;                 use Ada.Strings.Fixed;
with Ada.Text_IO;

procedure Zip_with_big_files is
  stream  : aliased Zip_File_Stream;
  archive : Zip_Create_Info;

  procedure Add_one_entry (file_name : String; rep_1, rep_2 : Natural) is
    archive_entry : aliased Zip_Entry_Stream_Type;
  begin
    Open (archive_entry, Buffer_Growth_Factor => 2);
    for count_2 in 1 .. rep_2 loop
      String'Write (archive_entry'Access, rep_1 * " ");
    end loop;
    Close (archive_entry, file_name, use_clock, archive);
  end Add_one_entry;

  function Leading_zeros (i, zeros : Integer) return String is
    pad : constant Integer := 10 ** zeros;
    str : String (1 .. zeros + 2);
  begin
    Put (str, i + pad);
    return str (3 .. str'Last);
  end Leading_zeros;

  procedure Create_with_Trace (file_name : String; mode : Character) is
  begin
    Ada.Text_IO.Put_Line (file_name);
    Create_Archive
     (archive,
      stream'Unchecked_Access,
      file_name,
      (if mode = 'a' then Zip.Compress.Store else Zip.Compress.Deflate_0));
  end Create_with_Trace;

begin
  --
  --  Test limit for cumulative size
  --
  for max in 40 .. 41 loop  --  max=40 : Zip_32, max=41 : promoted to Zip_64
    Create_with_Trace ("big_archive_" & Leading_zeros (max, 2) & ".zip", 'a');
    for i in 1 .. max loop
      Add_one_entry (
        "Entry #" & Leading_zeros (i, 2) & ".txt",
        100 * 1024 * 1024,  --  Cute 100 MiB entries
        1
      );
    end loop;
    Finish (archive);
  end loop;
  --
  --  Test limit for single entries
  --
  for mode in Character'('a') .. 'b' loop
    for size in 3 .. 5 loop
      Create_with_Trace ("big_entry_" & Leading_zeros (size, 1) & '_' & mode & ".zip", mode);
      Add_one_entry (
        "Entry " & Leading_zeros (size, 1) & "GiB.txt",
        size * 1024 * 1024,
        1024
      );
      Finish (archive);
    end loop;
  end loop;
end Zip_with_big_files;
