--  Fuzzing for the various Zip-Ada compression methods.

with Zip_Streams; use Zip_Streams;
with Zip.Compress;
with Zip.Create;   use Zip.Create;
with RW_File; use RW_File;

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.IO_Exceptions, Ada.Text_IO;

procedure Fuzzip is

   MyStream_memory : aliased Memory_Zipstream;

   Info1 : Zip_Create_info;

   UnbZipFile : Unbounded_String;
   UnbFile1 : Unbounded_String;

   MyStream1 : aliased Memory_Zipstream;

begin
   Create (Info1, MyStream_memory'Unchecked_Access, "to_memo.zip");

   -- Read the file1.txt in unbounded string (see also the specific Zip.Create.Add_String)
   RW_File.Read_File ("test/file1.txt", UnbFile1);
   -- Set a stream to the unbounded string
   Set (MyStream1, UnbFile1);
   Set_Name(MyStream1, "my_dir/file1_z.txt"); -- any name we like to store it with

   --  Add stream to the list
   Add_Stream (Info1, MyStream1);

   Finish (Info1);

   Get (MyStream_memory, UnbZipFile);
   RW_File.Write_File (Get_Name(MyStream_memory), UnbZipFile);

   --  exception
   --     when Ada.IO_Exceptions.Name_Error =>
   --        Ada.Text_IO.Put_Line("file1.txt or file2.txt is missing! Press Enter.");
   --        Ada.Text_IO.Skip_Line;
end Fuzzip;
