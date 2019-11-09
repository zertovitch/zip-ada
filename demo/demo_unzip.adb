--  Zip archive extraction demo.
--
--  For a complete Zip extraction tool, see tools/unzipada.adb .
--
--  Other tools using Zip extraction (to streams) are:
--    tools/comp_zip.adb and tools/find_zip.adb .
--
--  See tools/zip_dir_list.adb for a simple example
--    of a Zip directory traversal.
--
with Ada.Text_IO;                       use Ada.Text_IO;
with UnZip.Streams;                     use UnZip.Streams, UnZip;
with Demo_Zip;

procedure Demo_UnZip is
  f : Zipped_File_Type;
  s : Stream_Access;
  c : Character;
begin
  --  Make sure we have the Zip file for the demo, by
  --    calling the Zip file *creation* demo...
  --
  Demo_Zip;
  --
  --  Extract data to a *file*.
  --    You can customize this with Create_Path (creation of missing paths)
  --    and Compose_File_Name (add a custom prefix directory for instance)
  --    via the file_system_routines parameter. See tools/unzipada.adb for
  --    a complete example.
  --
  Extract ("mini_zip.zip", "demo/demo_unzip.adb", "demo_unzip_$unzipped$.adb");
  --
  --  Testing UnZip.Streams: extract data to a non-file *stream*.
  --    We just output the contents of a compressed entry to standard output.
  --
  Open (f, "mini_zip.zip", "demo/demo_unzip.adb", Ignore_Directory => True);
  s := Stream (f);
  while not End_Of_File (f) loop
    Character'Read (s, c);
    Put (c);
  end loop;
  New_Line;
  Put ("*** Dump done. Size of entry named [" &
    Name (f) & "] is" &
    UnZip.Streams.Count'Image (Size (f)) &
    ". Press Enter!"
  );
  Close (f);
  Skip_Line;
  --
end Demo_UnZip;
