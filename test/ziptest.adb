-- Contributed by ITEC - NXP Semiconductors
--
-- June 2008
--
-- Tests the direction: Data as any stream ----Compress----> Zip as any stream
--
--  Stream 1 (acc_MyStream_memory) => Zip stream created as Unbounded_String in memory then later save to disk.
--  Stream 2 (acc_MyStream_file)   => Zip stream created as new zip-file directly on disk.

--  File1 (StreamFile1) => Stream pointing to a Unbounded_String which has the content of a file from a disk.
--  File2 (StreamFile2) => Stream pointing to a file directly from disk.

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Zip_Streams; use Zip_Streams;
with RW_File; use RW_File;
with Zip.Compress;
with Zip.Create;   use Zip.Create;
with Ada.Streams.Stream_IO; use Ada.Streams.Stream_IO;

procedure ZipTest is

   MyStream_memory : aliased Unbounded_Stream;
   acc_MyStream_memory : constant Zipstream_Class := MyStream_memory'Unchecked_Access;

   MyStream_file : aliased ZipFile_Stream;
   acc_MyStream_file : constant Zipstream_Class := MyStream_file'Unchecked_Access;

   Info1 : Zip_Create_info;
   Info2 : Zip_Create_info;

   UnbZipFile : Unbounded_String;

   UnbFile1 : Unbounded_String;
   MyStream1 : aliased Unbounded_Stream;
   StreamFile1 : constant Zipstream_Class := MyStream1'Unchecked_Access;

   MyStream2 : aliased ZipFile_Stream;
   StreamFile2 : constant Zipstream_Class := MyStream2'Unchecked_Access;
begin
   Create (Info2, acc_MyStream_file,   "to_file.zip", Zip.Compress.Shrink);
   Create (Info1, acc_MyStream_memory, "to_memo.zip", Zip.Compress.Shrink);

   -- Read the file1.txt in unbounded string (see also the specific Zip.Create.Add_String)
   RW_File.Read_File ("file1.txt", UnbFile1);
   -- Set a stream to the unbounded string
   Set (MyStream1, UnbFile1);
   SetName(StreamFile1, "my_dir/file1_z.txt"); -- any name we like to store it with

   -- Read the file2.txt directly (see also the specific Zip.Create.Add_File)
   SetName(StreamFile2, "file2.txt");
   Open (MyStream2, In_File);
   -- The following can be ommited if we want to keep 'file2.txt'
   SetName(StreamFile2, "my_dir/file2_z.txt"); -- any name we like to store it with

   --  Add stream to the list
   Add_Stream (Info2, StreamFile1);
   Add_Stream (Info2, StreamFile2);
   --  Reset Streams
   Set_Index (StreamFile1, 1);
   Set_Index (StreamFile2, 1);
   --  Add stream to the list
   Add_Stream (Info1, StreamFile1);
   Add_Stream (Info1, StreamFile2);

   Finish (Info1);
   Finish (Info2);

   Get (MyStream_memory, UnbZipFile);
   RW_File.Write_File (GetName(acc_MyStream_memory), UnbZipFile);

end ZipTest;
