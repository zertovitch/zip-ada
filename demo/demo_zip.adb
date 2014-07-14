with Zip_Streams;                       use Zip_Streams;
with Zip.Create;                        use Zip.Create;

procedure Demo_zip is
  zip_file : aliased File_Zipstream; -- Archive is a file
  archive : Zip_Create_info;
begin
  Create (archive, zip_file'Unchecked_Access, "mini_zip.zip" );
  Add_File(archive, "demo_zip.adb");
  Add_File(archive, "demo_unzip.adb");
  Add_String(archive,
    "==== Hello world! ====" & ASCII.LF &
    "It is such a nice ""Hello world""-like demo, isn't it ?",
    "nice_string.txt"
  );
  Finish (archive);
end Demo_zip;
