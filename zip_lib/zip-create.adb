with Ada.Unchecked_Deallocation;
with Ada.Text_IO;
with Interfaces;                        use Interfaces;

package body Zip.Create is

   procedure Create(Info        : out Zip_Create_info;
                    Z_Stream    : in Zipstream_Class;
                    Name        : String;
                    Compress    : Zip.Compress.Compression_Method:= Zip.Compress.Shrink) is
   begin
      Info.Stream := Z_Stream;
      Info.Compress := Compress;
      if Name /= "" then
         SetName (Info.Stream, Name);
      end if;
      --
      -- If we have a real file (ZipFile_Stream or descendent), create the file too:
      --
      if Z_Stream.all in ZipFile_Stream'Class then
        Zip_Streams.Create (ZipFile_Stream(Z_Stream.all), Ada.Streams.Stream_IO.Out_File);
      end if;
   end Create;

   procedure Dispose is new
     Ada.Unchecked_Deallocation (Dir_entries, Pdir_entries);

   procedure Resize (A    : in out Pdir_entries;
                     Size : Integer) is
      Hlp : constant Pdir_entries := new Dir_entries (1 .. Size);
   begin
      if A = null then
         A := Hlp;
      else
         Hlp (1 .. Integer'Min (Size, A'Length)) :=
           A (1 .. Integer'Min (Size, A'Length));
         Dispose (A);
         A := Hlp;
      end if;
   end Resize;

   procedure Add_Stream (Info           : in out Zip_Create_info;
                         Stream         : Zipstream_Class)
   is
     Compressed_Size: Zip.File_size_type; -- dummy
     Final_Method   : Natural;            -- dummy
   begin
     Add_Stream(Info, Stream, null, Compressed_Size, Final_Method);
   end Add_Stream;

   procedure Add_Stream (Info           : in out Zip_Create_info;
                         Stream         : Zipstream_Class;
                         Feedback       : in     Feedback_proc;
                         Compressed_Size:    out Zip.File_size_type;
                         Final_Method   :    out Natural)
   is
      Last       : Integer := 1;
      mem1, mem2 : Integer := 1;
      entry_name : String:= GetName (Stream);
   begin
      -- Appnote.txt, V. J. :
      -- " All slashes should be forward slashes '/' as
      -- opposed to backwards slashes '\' "
      for i in entry_name'Range loop
        if entry_name(i) = '\' then
          entry_name(i):= '/';
        end if;
      end loop;
      --
      if Info.Contains /= null then
         Last := Info.Contains'Length + 1;
      end if;
      Resize (Info.Contains, Last);
      --  Administration
      Info.Contains (Last).head.made_by_version      := 23; -- version 2.30
      Info.Contains (Last).head.comment_length       := 0;
      Info.Contains (Last).head.disk_number_start    := 0;
      Info.Contains (Last).head.internal_attributes  := 0; -- 0:binary; 1:text
      Info.Contains (Last).head.external_attributes               := 0;
      Info.Contains (Last).head.short_info.needed_extract_version :=
        10; -- Value put by Zip/PKZip
      Info.Contains (Last).head.short_info.bit_flag               := 0;
      Info.Contains (Last).head.short_info.file_timedate          :=
        GetTime (Stream);
      Info.Contains (Last).head.short_info.dd.uncompressed_size   :=
        Unsigned_32 (Size (Stream));
      Info.Contains (Last).head.short_info.filename_length        :=
        entry_name'Length;
      Info.Contains (Last).head.short_info.extra_field_length     := 0;
      Info.Contains (Last).name := To_Unbounded_String (entry_name);

      mem1 := Index (Info.Stream);
      Info.Contains (Last).head.local_header_offset := Unsigned_32 (mem1) - 1;
      --  Write the local header with incomplete informations
      Zip.Headers.Write (Info.Stream, Info.Contains (Last).head.short_info);

      String'Write(Info.Stream, To_String (Info.Contains (Last).name));

      --  Write compressed file
      Zip.Compress.Compress_data
        (input            => Stream,
         output           => Info.Stream,
         input_size_known => True,
         input_size       =>
           Info.Contains (Last).head.short_info.dd.uncompressed_size,
         method           => Info.Compress,
         feedback         => Feedback,
         CRC              => Info.Contains (Last).head.short_info.dd.crc_32,
         output_size      => Info.Contains (Last).head.short_info.dd.compressed_size,
         zip_type         => Info.Contains (Last).head.short_info.zip_type
      );
      mem2 := Index (Info.Stream);
      --  Go back to the local header to rewrite it
      --  with complete informations
      Set_Index (Info.Stream, mem1);
      Zip.Headers.Write (Info.Stream, Info.Contains (Last).head.short_info);
      --  Return to momentaneous end of file
      Set_Index (Info.Stream, mem2);
      --
      Compressed_Size:= Info.Contains (Last).head.short_info.dd.compressed_size;
      Final_Method   := Natural(Info.Contains (Last).head.short_info.zip_type);
   end Add_Stream;

   procedure Add_File (Info              : in out Zip_Create_info;
                       Name              : String;
                       Name_in_archive   : String:= "";
                       -- default: add the file in the archive
                       -- under the same name
                       Delete_file_after : Boolean:= False
                       -- practical to delete temporary file after
                       -- adding
   )
   is
      temp_zip_stream     : aliased ZipFile_Stream;
      acc_temp_zip_stream : constant Zipstream_Class := temp_zip_stream'Unchecked_Access;
      use Ada.Text_IO;
      fd: File_Type;
   begin
     -- Read the file
     SetName(acc_temp_zip_stream, Name);
     Open(temp_zip_stream, Ada.Streams.Stream_IO.In_File);
     -- Eventually we set a new name for archiving:
     if Name_in_archive /= "" then
        SetName(acc_temp_zip_stream, Name_in_archive);
     end if;
     -- Stuff into the .zip archive:
     Add_Stream (Info, acc_temp_zip_stream);
     Close(temp_zip_stream);
     if Delete_file_after then
        Open(fd, In_File, Name);
        Delete(fd);
     end if;
   end Add_File;

   procedure Add_String (Info            : in out Zip_Create_info;
                         Contents        : String;
                         Name_in_archive : String
   )
   is
   begin
     Add_String(Info, To_Unbounded_String(Contents), Name_in_archive);
   end Add_String;

   procedure Add_String (Info            : in out Zip_Create_info;
                         Contents        : Unbounded_String;
                         Name_in_archive : String
   )
   is
     temp_zip_stream     : aliased Unbounded_Stream;
     acc_temp_zip_stream : constant Zipstream_Class := temp_zip_stream'Unchecked_Access;
   begin
     Set(temp_zip_stream, Contents);
     SetName(acc_temp_zip_stream, Name_in_archive);
     Add_Stream (Info, acc_temp_zip_stream);
   end Add_String;

   procedure Finish (Info : in out Zip_Create_info) is
      ed : Zip.Headers.End_of_Central_Dir;
   begin
      --
      --  2/ Almost done - write Central Directory:
      --
      ed.central_dir_offset := Unsigned_32 (Index (Info.Stream)) - 1;
      ed.total_entries := 0;
      ed.central_dir_size := 0;
      ed.main_comment_length := 0;
      if Info.Contains /= null then
        for e in Info.Contains'Range loop
           ed.total_entries := ed.total_entries + 1;
           Zip.Headers.Write (Info.Stream, Info.Contains (e).head);
           String'Write(Info.Stream, To_String (Info.Contains (e).name));
           ed.central_dir_size :=
             ed.central_dir_size +
               Zip.Headers.central_header_length +
                 Unsigned_32 (Info.Contains (e).head.short_info.filename_length);
        end loop;
        Dispose (Info.Contains);
      end if;
      ed.disknum := 0;
      ed.disknum_with_start := 0;
      ed.disk_total_entries := ed.total_entries;
      Zip.Headers.Write (Info.Stream, ed);
      --
      -- If we have a real file (ZipFile_Stream or descendent), close the file too:
      --
      if Info.Stream.all in ZipFile_Stream'Class then
        Zip_Streams.Close (ZipFile_Stream(Info.Stream.all));
      end if;
   end Finish;

end Zip.Create;
