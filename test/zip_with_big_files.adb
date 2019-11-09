------------------------------------------------------------------------------
--  File:            Zip_with_big_files.adb
--  Description:     Demo/test: test 4GB limit of "ZIP32" archive format
--  Author:          Gautier de Montmollin
------------------------------------------------------------------------------

with Zip.Compress;
with Zip.Create;                        use Zip.Create;

with Ada.Integer_Text_IO;               use Ada.Integer_Text_IO;
with Ada.Strings.Fixed;                 use Ada.Strings.Fixed;

procedure Zip_with_big_files is
  stream  : aliased Zip_File_Stream;
  archive : Zip_Create_Info;

  procedure Add_one_entry (file_name : String; rep : Natural) is
  begin
    Zip.Create.Add_String (
      Info              => archive,
      Contents          => rep * " ",
      Name_in_archive   => file_name
    );
  end Add_one_entry;

  function Leading_zeros (i, zeros : Integer) return String is
    pad : constant Integer := 10 ** zeros;
    str : String (1 .. zeros + 2);
  begin
    Put (str, i + pad);
    return str (3 .. str'Last);
  end Leading_zeros;

begin
  for max in 40 .. 41 loop  --  ok with 40, too large with 41
    Create_Archive (
      archive,
      stream'Unchecked_Access,
      "big" & Integer'Image (max) & ".zip", Zip.Compress.Store
    );
    for i in 1 .. max loop
      Add_one_entry (
        "Entry #" & Leading_zeros (i, 2) & ".txt",
        100 * 1024 * 1024  --  Cute 100 MB entries
      );
    end loop;
    Finish (archive);
  end loop;
end Zip_with_big_files;
