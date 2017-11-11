--  Fuzzing for the various Zip-Ada compression methods.
--  Test is derived from ZipTest.
--  What is tested here:
--    x --[compression]--> zip --[decompression]--> x'
--  x should be equal to x' - and of course
--  there should be no error inbetween.

with Zip_Streams; use Zip_Streams;
with Zip.Compress;
with Zip.Create;   use Zip.Create;
with RW_File; use RW_File;

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.IO_Exceptions, Ada.Text_IO;

procedure Fuzzip is

   mem_stream_Zip_archive : aliased Memory_Zipstream;

   Info1 : Zip_Create_info;

   UnbZipFile : Unbounded_String;
   Original : Unbounded_String;

   Mem_Stream_Content : aliased Memory_Zipstream;

begin
   -- Read the file1.txt in unbounded string (see also the specific Zip.Create.Add_String)
   RW_File.Read_File ("test/file1.txt", Original);
   -- Set a stream to the unbounded string
   Set (Mem_Stream_Content, Original);
   Set_Name(Mem_Stream_Content, "my_dir/file1_z.txt"); -- any name we like to store it with

   --  Add stream to the list
   Create (Info1, mem_stream_Zip_archive'Unchecked_Access, "to_memo.zip");
   Add_Stream (Info1, Mem_Stream_Content);

   Finish (Info1);

   Get (mem_stream_Zip_archive, UnbZipFile);

   --  !! Here: unzip the archive to another string and compar to original

   RW_File.Write_File (Get_Name(mem_stream_Zip_archive), UnbZipFile);

   --  exception
   --     when Ada.IO_Exceptions.Name_Error =>
   --        Ada.Text_IO.Put_Line("file1.txt or file2.txt is missing! Press Enter.");
   --        Ada.Text_IO.Skip_Line;
end Fuzzip;
