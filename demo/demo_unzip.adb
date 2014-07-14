with Ada.Text_IO;                       use Ada.Text_IO;
with UnZip.Streams;                     use UnZip.Streams, UnZip;
with Demo_Zip;

procedure Demo_UnZip is
  f: Zipped_File_Type;
  s: Stream_Access;
  c: Character;
begin
  Demo_Zip; -- Make sure we have the Zip file...
  --
  Extract("mini_zip.zip", "demo_unzip.adb", "demo_unzip_$unzipped$.adb");
  --
  -- Testing UnZip.Streams:
  -- We just output the contents of file to standard output
  --
  Open(f, "mini_zip.zip", "demo_unzip.adb");
  s:= Stream(f);
  while not End_of_file(f) loop
    Character'Read(s,c);
    Put(c);
  end loop;
  Close(f);
  New_Line;
  Put("*** Dump done, press Enter!");
  Skip_Line;
  --
end Demo_UnZip;
