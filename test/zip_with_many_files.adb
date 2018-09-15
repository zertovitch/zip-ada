------------------------------------------------------------------------------
--  File:            Zip_with_many_files.adb
--  Description:     Demo/test:
--                     - stuff a large number of files into a .zip file
--                     - test 65535 limit of "ZIP32" archive format
--  Author:          Gautier de Montmollin
------------------------------------------------------------------------------

with Zip_Streams;                       use Zip_Streams;
with Zip.Compress;
with Zip.Create;                        use Zip.Create;

with Ada.Integer_Text_IO;               use Ada.Integer_Text_IO;
with Ada.Strings.Fixed;                 use Ada.Strings.Fixed;

procedure Zip_with_many_files is
  stream  : aliased File_Zipstream;
  archive : Zip_Create_info;

  procedure Add_one_entry(file_name: String; rep: Natural) is
  begin
    Zip.Create.Add_String(
      Info              => archive,
      Contents          => "..." & rep * ("Hello! My name is: """ & file_name & '"' & ASCII.LF),
      Name_in_archive   => file_name
    );
  end Add_one_entry;

  function Leading_zeros (i, zeros: Integer) return String is
    pad: constant Integer := 10 ** zeros;
    str: String (1..zeros+2);
  begin
    Put (str, i + pad);
    return str (3 .. str'Last);
  end Leading_zeros;

begin
  Create(archive, stream'Unchecked_Access, "many.zip", Zip.Compress.Deflate_1);
  for i in 1 .. 65_535 loop
    Add_one_entry(
      "Entry #" & Leading_zeros (i, 5) & ".txt",
      Integer'Max(0, i / 100 - 10)  --  Obtain a certain number of incompressible entries.
    );
  end loop;
  Finish(archive);
end Zip_with_many_files;
