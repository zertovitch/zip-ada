------------------------------------------------------------------------------
--  File:            Zip_with_many_files.adb
--  Description:     Demo/test: stuff a large number of files into a .zip file
--  Date/version:    27-Apr-2016; 25-Feb-2010
--  Author:          Gautier de Montmollin
------------------------------------------------------------------------------

with Zip_Streams;                       use Zip_Streams;
with Zip.Compress;
with Zip.Create;                        use Zip.Create;

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

begin
  Create(archive, stream'Unchecked_Access, "many.zip", Zip.Compress.Deflate_1);
  for i in 1 .. 65_535 loop
    Add_one_entry(
      "Entry #" & Integer'Image(i) & ".txt",
      Integer'Max(0, i / 100 - 10)  --  Obtain a certain number of incompressible entries.
    );
  end loop;
  Finish(archive);
end Zip_with_many_files;
