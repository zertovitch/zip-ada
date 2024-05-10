--  Zip archive creation demo.
--  For a complete Zip creation tool, see tools/zipada.adb .
--
with Zip.Create;

procedure Demo_Zip is
  use Zip.Create;
  zip_file : aliased Zip_File_Stream;  --  Archive is a file in this example.
  archive : Zip_Create_Info;
begin
  Create_Archive (archive, zip_file'Unchecked_Access, "mini_zip.zip");
  --  We insert a couple of files into the Zip archive:
  Add_File (archive, "demo/demo_zip.adb", Password => "1234");
  Add_File (archive, "demo/demo_unzip.adb");
  --  Here we add directly some contents to the Zip archive
  --  without a file in the first place:
  Add_String
    (archive,
     "==== Hello world! ====" & ASCII.LF &
     "It is such a nice ""Hello world""-like demo, isn't it ?",
     "nice_string.txt");
  Add_Empty_Folder (archive, "empty_folder/");
  Finish (archive);
end Demo_Zip;
