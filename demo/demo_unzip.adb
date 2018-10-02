with Ada.Text_IO;                       use Ada.Text_IO;
with UnZip.Streams;                     use UnZip.Streams, UnZip;
with Demo_zip;

procedure Demo_UnZip is
  f: Zipped_File_Type;
  s: Stream_Access;
  c: Character;
begin
  --  Make sure we have the Zip file for the demo...
  --
  Demo_zip;
  --
  --  Extract to a file.
  --    You can customize this with Create_Path (creation of missing paths)
  --    and Compose_File_Name (add a custom prefix directory for instance)
  --    via the file_system_routines parameter. See tools/unzipada.adb for
  --    a complete example.
  --
  Extract("mini_zip.zip", "demo/demo_unzip.adb", "demo_unzip_$unzipped$.adb");
  --
  --  Testing UnZip.Streams: we just output the contents of an entry
  --  to standard output.
  --
  Open(f, "mini_zip.zip", "demo/demo_unzip.adb");
  s:= Stream(f);
  while not End_Of_File(f) loop
    Character'Read(s,c);
    Put(c);
  end loop;
  Close(f);
  New_Line;
  Put("*** Dump done, press Enter!");
  Skip_Line;
  --
end Demo_UnZip;
