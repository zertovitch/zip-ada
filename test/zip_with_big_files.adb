------------------------------------------------------------------------------
--  File:            Zip_with_big_files.adb
--  Description:     Demo/test: test 4GB limits of Zip ("ZIP32") archives
--  Author:          Gautier de Montmollin
------------------------------------------------------------------------------

with Zip_Streams;                       use Zip_Streams;
with Zip.Compress;
with Zip.Create;                        use Zip.Create;

with Ada.Strings.Fixed;                 use Ada.Strings.Fixed;

procedure Zip_with_big_files is
  stream  : aliased File_Zipstream;
  archive : Zip_Create_info;

  procedure Add_one_entry(file_name: String; rep: Natural) is
  begin
    Zip.Create.Add_String(
      Info              => archive,
      Contents          => rep * " ",
      Name_in_archive   => file_name
    );
  end Add_one_entry;

begin
  Create(archive, stream'Unchecked_Access, "big.zip", Zip.Compress.Store);
  for i in 1 .. 38 loop
    Add_one_entry(
      "Entry #" & Integer'Image(i) & ".txt",
      100 * 1024 * 1024  --  Cute 100 MB entries
    );
  end loop;
  Finish(archive);
end Zip_with_big_files;
