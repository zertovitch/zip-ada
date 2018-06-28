-- Contributed by ITEC - NXP Semiconductors
--
-- June 2008
--
-- Tests the direction: Data as any stream ----Compress----> Zip as any stream
--
--  Stream 1 (MyStream_memory) => Zip stream created as Unbounded_String in memory then later save to disk.
--  Stream 2 (MyStream_file)   => Zip stream created as new zip-file directly on disk.

--  File1 (MyStream1) => Stream pointing to a Unbounded_String which has the content of a file from a disk.
--  File2 (MyStream2) => Stream pointing to a file directly from disk.

with Zip_Streams; use Zip_Streams;
with Zip.Compress;
with Zip.Create;   use Zip.Create;
with RW_File;

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.IO_Exceptions, Ada.Text_IO;

procedure ZipTest is

   MyStream_memory : aliased Memory_Zipstream;
   MyStream_file : aliased File_Zipstream;

   Info1 : Zip_Create_info;
   Info2 : Zip_Create_info;

   UnbZipFile : Unbounded_String;
   UnbFile1 : Unbounded_String;

   MyStream1 : aliased Memory_Zipstream;
   MyStream2 : aliased File_Zipstream;

begin
   Create (Info2, MyStream_file'Unchecked_Access,   "to_file.zip", Zip.Compress.Shrink);
   Create (Info1, MyStream_memory'Unchecked_Access, "to_memo.zip", Zip.Compress.Shrink);

   -- Read the file1.txt in unbounded string (see also the specific Zip.Create.Add_String)
   RW_File.Read_File ("file1.txt", UnbFile1);
   -- Set a stream to the unbounded string
   Set (MyStream1, UnbFile1);
   Set_Name(MyStream1, "my_dir/file1_z.txt"); -- any name we like to store it with

   -- Read the file2.txt directly (see also the specific Zip.Create.Add_File)
   Set_Name(MyStream2, "file2.txt");
   Open (MyStream2, In_File);
   -- The following can be ommited if we want to keep 'file2.txt'
   Set_Name(MyStream2, "my_dir/file2_z.txt"); -- any name we like to store it with

   --  Add stream to the list
   Add_Stream (Info2, MyStream1);
   Add_Stream (Info2, MyStream2);
   --  Reset Streams
   Set_Index (MyStream1, 1);
   Set_Index (MyStream2, 1);
   --  Add stream to the list
   Add_Stream (Info1, MyStream1);
   Add_Stream (Info1, MyStream2);

   Finish (Info1);
   Finish (Info2);

   Get (MyStream_memory, UnbZipFile);
   RW_File.Write_File (Get_Name(MyStream_memory), UnbZipFile);

exception
   when Ada.IO_Exceptions.Name_Error =>
      Ada.Text_IO.Put_Line("file1.txt or file2.txt is missing! Press Enter.");
      Ada.Text_IO.Skip_Line;
end ZipTest;
