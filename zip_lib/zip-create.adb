--  Legal licensing note:

--  Copyright (c) 2008 .. 2019 Gautier de Montmollin (maintainer)
--  SWITZERLAND

--  Permission is hereby granted, free of charge, to any person obtaining a copy
--  of this software and associated documentation files (the "Software"), to deal
--  in the Software without restriction, including without limitation the rights
--  to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
--  copies of the Software, and to permit persons to whom the Software is
--  furnished to do so, subject to the following conditions:

--  The above copyright notice and this permission notice shall be included in
--  all copies or substantial portions of the Software.

--  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
--  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
--  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
--  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
--  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
--  OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
--  THE SOFTWARE.

--  NB: this is the MIT License, as found 21-Aug-2016 on the site
--  http://www.opensource.org/licenses/mit-license.php

with Ada.Directories;
with Ada.Text_IO;
with Ada.Unchecked_Deallocation;

with Interfaces;

package body Zip.Create is

   use Interfaces, Zip_Streams, Zip.Headers;

   procedure Create_Archive (
      Info            : out Zip_Create_Info;
      Z_Stream        : in Zip_Streams.Zipstream_Class_Access;
      Archive_Name    : String;
      Compress_Method : Zip.Compress.Compression_Method := Zip.Compress.Deflate_1;
      Duplicates      : Duplicate_name_policy           := admit_duplicates
   )
   is
   begin
      Info.Stream   := Z_Stream;
      Info.Compress := Compress_Method;
      if Archive_Name /= "" then
         Set_Name (Info.Stream.all, Archive_Name);
      end if;
      --
      --  If we have a real file (File_Zipstream or descendent), create the file too:
      --
      if Z_Stream.all in File_Zipstream'Class then
        Zip_Streams.Create (File_Zipstream (Z_Stream.all), Zip_Streams.Out_File);
      end if;
      Info.Duplicates := Duplicates;
   end Create_Archive;

   function Is_Created (Info : Zip_Create_Info) return Boolean is
   begin
      return Info.Stream /= null;
   end Is_Created;

   procedure Set (Info       : in out Zip_Create_Info;
                  New_Method : Zip.Compress.Compression_Method)
   is
   begin
     Info.Compress := New_Method;
   end Set;

   function Name (Info : Zip_Create_Info) return String is
   begin
     return Get_Name (Info.Stream.all);
   end Name;

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

   --  Internal - add the catalogue entry corresponding to a
   --  compressed file in the Zip archive.
   --  The entire catalogue will be written at the end of the zip stream,
   --  and the entry as a local header just before the compressed data.
   --  The entry's is mostly incomplete in the end (name, size, ...); stream
   --  operations on the archive being built are not performed here,
   --  see Add_Stream for that.
   --
   procedure Add_catalogue_entry (Info : in out Zip_Create_Info)
   is
   begin
      if Info.Last_entry = 0 then
        Info.Last_entry := 1;
        Resize (Info.Contains, 32);
      else
        Info.Last_entry := Info.Last_entry + 1;
        if Info.Last_entry > Info.Contains'Last then
          --  Info.Contains is full, time to resize it!
          --  We do nothing less than double the size - better than
          --  whatever offer you'd get in your e-mails.
          Resize (Info.Contains, Info.Contains'Last * 2);
        end if;
      end if;
      declare
        cfh : Central_File_Header renames Info.Contains (Info.Last_entry).head;
      begin
        --  Administration
        cfh.made_by_version      := 23; -- version 2.30
        cfh.comment_length       := 0;
        cfh.disk_number_start    := 0;
        cfh.internal_attributes  := 0; -- 0: binary; 1: text
        cfh.external_attributes  := 0;
        cfh.short_info.needed_extract_version := 10; -- Value put by Zip/PKZip
        cfh.short_info.bit_flag  := 0;
      end;
   end Add_catalogue_entry;

   --  This is just for detecting duplicates
   procedure Insert_to_name_dictionary (file_name : String; m : in out Name_mapping.Map) is
     cm : Name_mapping.Cursor;
     OK : Boolean;
   begin
     m.Insert (To_Unbounded_String (file_name), cm, OK);
     if not OK then  --  Name already registered
       raise Duplicate_name with "Entry name = " & file_name;
     end if;
   end Insert_to_name_dictionary;

   procedure Add_Stream (Info     : in out Zip_Create_Info;
                         Stream   : in out Zip_Streams.Root_Zipstream_Type'Class;
                         Password : in     String := "")
   is
     Compressed_Size : Zip.File_size_type;  --  dummy
     Final_Method    : Natural;             --  dummy
   begin
     Add_Stream (Info, Stream, null, Password, Compressed_Size, Final_Method);
   end Add_Stream;

   four_gb : constant := 4 * (1024 ** 3);

   Zip_32_size_exceeded_message : constant String :=
     "Zip file too large (for Zip_32 archive format): more than 4 GiB total compressed size.";

   use Zip.Compress;

   procedure Add_Stream (Info            : in out Zip_Create_Info;
                         Stream          : in out Zip_Streams.Root_Zipstream_Type'Class;
                         Feedback        : in     Feedback_proc;
                         Password        : in     String := "";
                         Compressed_Size :    out Zip.File_size_type;
                         Final_Method    :    out Natural)
   is
      mem1, mem2 : ZS_Index_Type := 1;
      entry_name : String := Get_Name (Stream);
      Last : Positive;
   begin
      --  Appnote.txt, V. J. :
      --    " All slashes should be forward slashes '/' as opposed to backwards slashes '\' "
      for i in entry_name'Range loop
        if entry_name (i) = '\' then
          entry_name (i) := '/';
        end if;
      end loop;
      if Info.Duplicates = error_on_duplicate then
        --  Check for duplicates; raises Duplicate_name in this case.
        Insert_to_name_dictionary (entry_name, Info.name_dictionary);
      end if;
      Add_catalogue_entry (Info);
      Last := Info.Last_entry;
      declare
        cfh : Central_File_Header renames Info.Contains (Last).head;
        shi : Local_File_Header renames cfh.short_info;
      begin
        --  Administration - continued
        if Zip_Streams.Is_Unicode_Name (Stream) then
          shi.bit_flag := shi.bit_flag or Zip.Headers.Language_Encoding_Flag_Bit;
        end if;
        if Password /= "" then
          shi.bit_flag := shi.bit_flag or Zip.Headers.Encryption_Flag_Bit;
        end if;
        if Is_Read_Only (Stream) then
          cfh.external_attributes := cfh.external_attributes or 1;
        end if;
        shi.file_timedate        := Get_Time (Stream);
        shi.dd.uncompressed_size := Unsigned_32 (Size (Stream));
        shi.filename_length      := entry_name'Length;
        Info.Contains (Last).name           := new String'(entry_name);
        shi.extra_field_length   := 0;

        mem1 := Index (Info.Stream.all);
        cfh.local_header_offset := Unsigned_32 (mem1) - 1;
        --  Write the local header with incomplete informations
        Zip.Headers.Write (Info.Stream.all, shi);

        String'Write (Info.Stream, entry_name);

        Zip.Compress.Compress_data
          (input            => Stream,
           output           => Info.Stream.all,
           input_size_known => True,
           input_size       => shi.dd.uncompressed_size,
           method           => Info.Compress,
           feedback         => Feedback,
           password         => Password,
           content_hint     => Guess_type_from_name (entry_name),
           CRC              => shi.dd.crc_32,
           output_size      => shi.dd.compressed_size,
           zip_type         => shi.zip_type
          );
        if shi.zip_type = Compression_format_code.lzma_code then
          --
          --  For LZMA, we always put an EOS marker. From PKWARE's Appnote:
          --
          --      5.8.9 Data compressed with method 14, LZMA, may include an end-of-stream
          --      (EOS) marker ending the compressed data stream.  This marker is not
          --      required, but its use is highly recommended to facilitate processing
          --      and implementers should include the EOS marker whenever possible.
          --      When the EOS marker is used, general purpose bit 1 must be set.  If
          --      general purpose bit 1 is not set, the EOS marker is not present.
          --
          shi.bit_flag := shi.bit_flag or LZMA_EOS_Flag_Bit;
        end if;
        mem2 := Index (Info.Stream.all);
        if Info.zip_archive_format = Zip_32 and then mem2 > four_gb then
          raise Zip_Capacity_Exceeded with Zip_32_size_exceeded_message;
        end if;
        --  Go back to the local header to rewrite it with complete informations
        --  known after the compression: CRC value, compressed size, actual compression format.
        Set_Index (Info.Stream.all, mem1);
        Zip.Headers.Write (Info.Stream.all, shi);
        --  Return to momentaneous end of file
        Set_Index (Info.Stream.all, mem2);
        --
        Compressed_Size := shi.dd.compressed_size;
        Final_Method    := Natural (shi.zip_type);
      end;
   end Add_Stream;

   procedure Add_File (Info              : in out Zip_Create_Info;
                       File_Name         : String;
                       --  Name_in_archive: default: add the file in
                       --  the archive under the File's name.
                       Name_in_archive   : String            := "";
                       --  Delete_file_after: practical to delete temporary file after adding.
                       Delete_file_after : Boolean           := False;
                       Name_encoding     : Zip_name_encoding := IBM_437;
                       --  Time stamp for this entry
                       Modification_time : Time              := default_time;
                       Is_read_only      : Boolean           := False;
                       Feedback          : Feedback_proc     := null;
                       Password          : String            := ""
   )
   is
      temp_zip_stream : aliased File_Zipstream;
      use Ada.Text_IO;
      fd : File_Type;
      Compressed_Size : Zip.File_size_type; -- unused
      Final_Method    : Natural; -- unused
   begin
     --  Read the file
     Set_Name (temp_zip_stream, File_Name);
     Open (temp_zip_stream, Zip_Streams.In_File);
     --  Eventually we set a new name for archiving:
     if Name_in_archive /= "" then
        Set_Name (temp_zip_stream, Name_in_archive);
     end if;
     Set_Unicode_Name_Flag (temp_zip_stream, Name_encoding = UTF_8);
     Set_Read_Only_Flag (temp_zip_stream, Is_read_only);
     if Modification_time = use_file_modification_time then
       Set_Time (temp_zip_stream,
         Ada.Directories.Modification_Time (File_Name)
       );
     elsif Modification_time = use_clock then
       Set_Time (temp_zip_stream, Ada.Calendar.Clock);
     else
       Set_Time (temp_zip_stream, Modification_time);
     end if;
     --  Stuff into the .zip archive:
     Add_Stream (Info, temp_zip_stream, Feedback, Password, Compressed_Size, Final_Method);
     Close (temp_zip_stream);
     if Delete_file_after then
        Open (fd, In_File, File_Name);
        Delete (fd);
     end if;
   exception
     when User_abort =>
       if Is_Open (temp_zip_stream) then
         Close (temp_zip_stream);
       end if;
       raise;
   end Add_File;

   procedure Add_String (Info               : in out Zip_Create_Info;
                         Contents           : String;
                         Name_in_archive    : String;
                         --  Name_UTF_8_encoded = True if Name is actually UTF-8 encoded (Unicode)
                         Name_UTF_8_encoded : Boolean  := False;
                         Password           : String   := "";
                         --  Time stamp for this entry
                         Creation_time      : Zip.Time := default_time
   )
   is
   begin
     Add_String (
       Info               => Info,
       Contents           => To_Unbounded_String (Contents),
       Name_in_archive    => Name_in_archive,
       Name_UTF_8_encoded => Name_UTF_8_encoded,
       Password           => Password,
       Creation_time      => Creation_time
     );
   end Add_String;

   procedure Add_String (Info               : in out Zip_Create_Info;
                         Contents           : Unbounded_String;
                         Name_in_archive    : String;
                         --  Name_UTF_8_encoded = True if Name is actually UTF-8 encoded (Unicode)
                         Name_UTF_8_encoded : Boolean  := False;
                         Password           : String   := "";
                         --  Time stamp for this entry
                         Creation_time      : Zip.Time := default_time
   )
   is
     temp_zip_stream : Zip_Memory_Stream;
   begin
     Set (temp_zip_stream, Contents);
     Set_Name (temp_zip_stream, Name_in_archive);
     if Creation_time = use_clock
               --  If we have use_file_modification_time by mistake, use clock as well:
       or else Creation_time = use_file_modification_time
     then
       Set_Time (temp_zip_stream, Ada.Calendar.Clock);
     else
       Set_Time (temp_zip_stream, Creation_time);
     end if;
     Set_Unicode_Name_Flag (temp_zip_stream, Name_UTF_8_encoded);
     Add_Stream (Info, temp_zip_stream, Password);
   end Add_String;

   procedure Add_Compressed_Stream (
     Info           : in out Zip_Create_Info;                        --  Destination
     Stream         : in out Zip_Streams.Root_Zipstream_Type'Class;  --  Source
     Feedback       : in     Feedback_proc
   )
   is
      lh : Zip.Headers.Local_File_Header;
      data_descriptor_after_data : Boolean;
   begin
      Zip.Headers.Read_and_check (Stream, lh);
      data_descriptor_after_data := (lh.bit_flag and 8) /= 0;
      --  Copy name and ignore extra field
      declare
        name  : String (1 .. Positive (lh.filename_length));
        extra : String (1 .. Natural (lh.extra_field_length));
      begin
        String'Read (Stream'Access, name);
        String'Read (Stream'Access, extra);
        if Info.Duplicates = error_on_duplicate then
          --  Check for duplicates; raises Duplicate_name in this case:
          Insert_to_name_dictionary (name, Info.name_dictionary);
        end if;
        Add_catalogue_entry (Info);
        Info.Contains (Info.Last_entry).head.local_header_offset :=
          Unsigned_32 (Index (Info.Stream.all)) - 1;
        Info.Contains (Info.Last_entry).name := new String'(name);
        lh.extra_field_length := 0;  --  extra field is zeroed (causes problems if not)
        Zip.Headers.Write (Info.Stream.all, lh);  --  Copy local header to new stream
        String'Write (Info.Stream, name);         --  Copy entry name to new stream
      end;
      Zip.Copy_chunk (
        Stream,
        Info.Stream.all,
        Integer (lh.dd.compressed_size),
        Feedback => Feedback
      );
      --  Postfixed data descriptor contains the correct values for
      --  CRC and sizes. Example of Zip files using that descriptor: those
      --  created by Microsoft's OneDrive cloud storage (for downloading
      --  more than one file), in 2018.
      if data_descriptor_after_data then
        --  NB: some faulty JAR files may fail with Read_and_check.
        --  See UnZip.Decompress, Process_descriptor.
        Zip.Headers.Read_and_check (Stream, lh.dd);
        --  lh's values have been corrected on the way.
        Zip.Headers.Write (Info.Stream.all, lh.dd);  --  Copy descriptor to new stream.
      end if;
      Info.Contains (Info.Last_entry).head.short_info := lh;
   end Add_Compressed_Stream;

   procedure Finish (Info : in out Zip_Create_Info) is
      ed : Zip.Headers.End_of_Central_Dir;
      procedure Dispose is new Ada.Unchecked_Deallocation (String, p_String);
      current_index : ZS_Index_Type;
      procedure Get_index_and_check_Zip_32_limit is
      begin
        current_index := Index (Info.Stream.all);
        if Info.zip_archive_format = Zip_32 and then current_index > four_gb then
          raise Zip_Capacity_Exceeded with Zip_32_size_exceeded_message;
        end if;
      end Get_index_and_check_Zip_32_limit;
   begin
      --
      --  2/ Almost done - write Central Directory:
      --
      Get_index_and_check_Zip_32_limit;
      ed.central_dir_offset := Unsigned_32 (current_index) - 1;
      ed.total_entries := 0;
      ed.central_dir_size := 0;
      ed.main_comment_length := 0;
      if Info.zip_archive_format = Zip_32
        and then Info.Last_entry > Integer (Unsigned_16'Last)
      then
        raise Zip_Capacity_Exceeded with
           "Too many entries (for Zip_32 archive format): more than 65535.";
      end if;
      if Info.Contains /= null then
        for e in 1 .. Info.Last_entry loop
           ed.total_entries := ed.total_entries + 1;
           Zip.Headers.Write (Info.Stream.all, Info.Contains (e).head);
           String'Write (Info.Stream, Info.Contains (e).name.all);
           --  The extra field here is assumed to be empty!
           ed.central_dir_size :=
             ed.central_dir_size +
               Zip.Headers.central_header_length +
                 Unsigned_32 (Info.Contains (e).head.short_info.filename_length);
          Dispose (Info.Contains (e).name);
          Get_index_and_check_Zip_32_limit;
        end loop;
        Dispose (Info.Contains);
      end if;
      Info.Last_entry := 0;
      Info.name_dictionary.Clear;
      ed.disknum := 0;
      ed.disknum_with_start := 0;
      ed.disk_total_entries := ed.total_entries;
      Zip.Headers.Write (Info.Stream.all, ed);
      Get_index_and_check_Zip_32_limit;
      --
      --  If we have a real file (File_Zipstream or descendent), close the file too:
      --
      if Info.Stream.all in File_Zipstream'Class then
        Zip_Streams.Close (File_Zipstream (Info.Stream.all));
      end if;
   end Finish;

end Zip.Create;
