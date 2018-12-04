--  Zip archive creation demo.
--  For a complete Zip creation tool, see tools/zipada.adb .
--
with Zip_Streams;                       use Zip_Streams;
with Zip.Create;                        use Zip.Create;

procedure Demo_Zip is
  zip_file : aliased File_Zipstream;  --  Archive is a file in this example.
  archive : Zip_Create_info;
begin
  Create (archive, zip_file'Unchecked_Access, "mini_zip.zip");
  Add_File (archive, "demo/demo_zip.adb", Password => "1234");
  Add_File (archive, "demo/demo_unzip.adb");
  --  Here we add directly some contents to the Zip archive
  --  without a file in the first place.
  Add_String (archive,
    "==== Hello world! ====" & ASCII.LF &
    "It is such a nice ""Hello world""-like demo, isn't it ?",
    "nice_string.txt"
  );
  Finish (archive);
end Demo_Zip;
