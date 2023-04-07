--  Legal licensing note:

--  Copyright (c) 2008 .. 2023 Gautier de Montmollin (maintenance and further development)
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

with Ada.Directories,
     Ada.IO_Exceptions,
     Ada.Text_IO,
     Ada.Unchecked_Deallocation;

package body Zip.Create is

   use Interfaces, Zip.Headers;

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
         Info.Stream.Set_Name (Archive_Name);
      end if;
      --
      --  If we have a real file (File_Zipstream or descendent), create the file too:
      --
      if Z_Stream.all in Zip_Streams.File_Zipstream'Class then
        Zip_Streams.File_Zipstream (Z_Stream.all).Create (Zip_Streams.Out_File);
      end if;
      Info.Duplicates := Duplicates;
      Info.zip_archive_format := Zip_32;
   end Create_Archive;

   function Is_Created (Info : Zip_Create_Info) return Boolean is
     use type Zip_Streams.Zipstream_Class_Access;
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
     return Info.Stream.Get_Name;
   end Name;

   procedure Dispose is new
     Ada.Unchecked_Deallocation (Dir_entries, Pdir_entries);

   procedure Resize (A    : in out Pdir_entries;
                     Size : Integer_M32) is
      Hlp : constant Pdir_entries := new Dir_entries (1 .. Size);
   begin
      if A = null then
         A := Hlp;
      else
         Hlp (1 .. Integer_M32'Min (Size, A'Length)) :=
           A (1 .. Integer_M32'Min (Size, A'Length));
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
        if Info.Last_entry = 2 ** 31 - 1 then
          raise Zip_Capacity_Exceeded with
             "Too many entries: more than 2,147,483,647.";
        end if;
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
     m.Insert (Ada.Strings.Unbounded.To_Unbounded_String (file_name), cm, OK);
     if not OK then  --  Name already registered
       raise Duplicate_name with "Entry name = " & file_name;
     end if;
   end Insert_to_name_dictionary;

   procedure Add_Stream (Info     : in out Zip_Create_Info;
                         Stream   : in out Zip_Streams.Root_Zipstream_Type'Class;
                         Password : in     String := "")
   is
     Compressed_Size : Zip.Zip_64_Data_Size_Type;  --  dummy
     Final_Method    : Natural;             --  dummy
   begin
     Add_Stream (Info, Stream, null, Password, Compressed_Size, Final_Method);
   end Add_Stream;

   four_GiB : constant := 4 * (1024 ** 3);  --  = 2 ** 32

   use Zip.Compress;

   procedure Check_Size
     (info  : in out Zip_Create_Info;
      value : in     Zip_Streams.ZS_Size_Type)  --  Archive index or input stream size
   is
     margin : constant := end_of_central_dir_length +
                          zip_64_end_of_central_dir_length +
                          zip_64_end_of_central_dir_locator_length +
                          2 ** 16 +  --  Zip archive comment
                          10;        --  Unknown unknown...
   begin
     if info.zip_archive_format = Zip_32 and then value >= four_GiB - margin then
       --  Promote format to Zip_64 (entry size or cumulated archive size too large for Zip_32).
       info.zip_archive_format := Zip_64;
       if value >= max_size - margin then
         raise Zip_Capacity_Exceeded with
           "Archive too large: size is 2 EiB (Exbibytes) or more.";
       end if;
     end if;
   end Check_Size;

   function Unixify (entry_name : String) return String is
     unixified : String (entry_name'Range) := entry_name;
   begin
     --  Appnote.txt, V. J. :
     --    " All slashes should be forward slashes '/' as opposed to backwards slashes '\' "
     for i in unixified'Range loop
       if unixified (i) = '\' then
         unixified (i) := '/';
       end if;
     end loop;
     return unixified;
   end Unixify;

   procedure Add_Stream (Info            : in out Zip_Create_Info;
                         Stream          : in out Zip_Streams.Root_Zipstream_Type'Class;
                         Feedback        : in     Feedback_proc;
                         Password        : in     String := "";
                         Compressed_Size :    out Zip.Zip_64_Data_Size_Type;
                         Final_Method    :    out Natural)
   is
      mem1, mem2 : Zip_Streams.ZS_Index_Type := 1;
      entry_name : constant String := Unixify (Stream.Get_Name);
      Last : Positive_M32;
      fh_extra : Local_File_Header_Extension;
   begin
      if Info.Duplicates = error_on_duplicate then
        --  Check for duplicates; raises Duplicate_name in this case.
        Insert_to_name_dictionary (entry_name, Info.name_dictionary);
      end if;
      Add_catalogue_entry (Info);
      Last := Info.Last_entry;
      declare
        cfh : Central_File_Header renames Info.Contains (Last).head;
        shi : Local_File_Header renames cfh.short_info;
        extra_field_policy : Extra_Field_Policy_Kind;
      begin
        --  Administration - continued
        if Zip_Streams.Is_Unicode_Name (Stream) then
          shi.bit_flag := shi.bit_flag or Zip.Headers.Language_Encoding_Flag_Bit;
        end if;
        if Password /= "" then
          shi.bit_flag := shi.bit_flag or Zip.Headers.Encryption_Flag_Bit;
        end if;
        if Stream.Is_Read_Only then
          cfh.external_attributes := cfh.external_attributes or 1;
        end if;
        Info.Contains (Last).name := new String'(entry_name);
        Check_Size (Info, Stream.Size);
        shi.file_timedate         := Stream.Get_Time;
        shi.dd.uncompressed_size  := Unsigned_64 (Stream.Size);
        shi.dd.compressed_size    := shi.dd.uncompressed_size;
        shi.filename_length       := entry_name'Length;
        shi.extra_field_length    := 0;

        mem1 := Info.Stream.Index;
        cfh.local_header_offset := Unsigned_64 (mem1) - 1;
        if Needs_Local_Zip_64_Header_Extension (shi, cfh.local_header_offset) then
          extra_field_policy := force_zip_64;
        else
          extra_field_policy := force_empty;
        end if;
        --  Write the local header with incomplete informations
        Zip.Headers.Write (Info.Stream.all, shi, extra_field_policy);

        String'Write (Info.Stream, entry_name);
        if extra_field_policy = force_zip_64 then
          --  Partial garbage. The extra field is rewritten later.
          fh_extra.tag  := 1;
          fh_extra.size := local_header_extension_short_length - 4;
          Zip.Headers.Write (Info.Stream.all, fh_extra, True);
        end if;

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
        mem2 := Info.Stream.Index;
        --  Go back to the local header to rewrite it with complete informations
        --  known after the compression: CRC value, compressed size, actual compression format.
        Info.Stream.Set_Index (mem1);
        Zip.Headers.Write (Info.Stream.all, shi, extra_field_policy);
        if extra_field_policy = force_zip_64 then
          String'Write (Info.Stream, entry_name);
          fh_extra.value_64 (1) := shi.dd.uncompressed_size;
          fh_extra.value_64 (2) := shi.dd.compressed_size;
          fh_extra.value_64 (3) := cfh.local_header_offset;  --  Not actually written.
          Zip.Headers.Write (Info.Stream.all, fh_extra, True);
        end if;
        --  Return to momentaneous end of file
        Info.Stream.Set_Index (mem2);
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
                       Modification_time : Time              := default_creation_time;
                       Is_read_only      : Boolean           := False;
                       Feedback          : Feedback_proc     := null;
                       Password          : String            := ""
   )
   is
      temp_zip_stream : aliased Zip_Streams.File_Zipstream;
      file_for_deletion : Ada.Text_IO.File_Type;
      Compressed_Size : Zip.Zip_64_Data_Size_Type; -- unused
      Final_Method    : Natural; -- unused
      use type Zip_Streams.Time;
   begin
     --  Read the file
     temp_zip_stream.Set_Name (Unixify (File_Name));
     temp_zip_stream.Open (Zip_Streams.In_File);
     --  Eventually we set a new name for archiving:
     if Name_in_archive /= "" then
        temp_zip_stream.Set_Name (Unixify (Name_in_archive));
     end if;
     temp_zip_stream.Set_Unicode_Name_Flag (Name_encoding = UTF_8);
     temp_zip_stream.Set_Read_Only_Flag (Is_read_only);
     if Modification_time = use_file_modification_time then
       temp_zip_stream.Set_Time
         (Ada.Directories.Modification_Time (File_Name));
     elsif Modification_time = use_clock then
       temp_zip_stream.Set_Time (Ada.Calendar.Clock);
     else
       temp_zip_stream.Set_Time (Modification_time);
     end if;
     --  Stuff into the .zip archive:
     Add_Stream (Info, temp_zip_stream, Feedback, Password, Compressed_Size, Final_Method);
     temp_zip_stream.Close;
     if Delete_file_after then
       Ada.Text_IO.Open (file_for_deletion, Ada.Text_IO.In_File, File_Name);
       Ada.Text_IO.Delete (file_for_deletion);
     end if;
   exception
     when User_abort =>
       if temp_zip_stream.Is_Open then
         temp_zip_stream.Close;
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
                         Creation_time      : Zip.Time := default_creation_time
   )
   is
   begin
     Add_String (
       Info               => Info,
       Contents           => Ada.Strings.Unbounded.To_Unbounded_String (Contents),
       Name_in_archive    => Name_in_archive,
       Name_UTF_8_encoded => Name_UTF_8_encoded,
       Password           => Password,
       Creation_time      => Creation_time
     );
   end Add_String;

   procedure Add_String (Info               : in out Zip_Create_Info;
                         Contents           : Ada.Strings.Unbounded.Unbounded_String;
                         Name_in_archive    : String;
                         --  Name_UTF_8_encoded = True if Name is actually UTF-8 encoded (Unicode)
                         Name_UTF_8_encoded : Boolean  := False;
                         Password           : String   := "";
                         --  Time stamp for this entry
                         Creation_time      : Zip.Time := default_creation_time
   )
   is
     temp_zip_stream : Zip_Memory_Stream;
     use type Zip_Streams.Time;
   begin
     temp_zip_stream.Set (Contents);
     temp_zip_stream.Set_Name (Unixify (Name_in_archive));
     if Creation_time = use_clock
               --  If we have use_file_modification_time by mistake, use clock as well:
       or else Creation_time = use_file_modification_time
     then
       temp_zip_stream.Set_Time (Ada.Calendar.Clock);
     else
       temp_zip_stream.Set_Time (Creation_time);
     end if;
     temp_zip_stream. Set_Unicode_Name_Flag (Name_UTF_8_encoded);
     Add_Stream (Info, temp_zip_stream, Password);
   end Add_String;

   procedure Add_Empty_Folder
     (Info               : in out Zip_Create_Info;
      Folder_Name        : in     String;
      --  Name_UTF_8_encoded = True if Name is actually UTF-8 encoded (Unicode)
      Name_UTF_8_encoded : in     Boolean  := False)
   is
     ufn : constant String := Unixify (Folder_Name);
   begin
     Add_String
       (Info               => Info,
        Contents           => "",
        Name_UTF_8_encoded => Name_UTF_8_encoded,
        Name_in_archive    =>
          (if ufn'Length > 0 and then ufn (ufn'Last) = '/'
           then
             ufn
           else
             ufn & '/'));
   end Add_Empty_Folder;

   procedure Add_Compressed_Stream
     (Info     : in out Zip_Create_Info;                        --  Destination
      Stream   : in out Zip_Streams.Root_Zipstream_Type'Class;  --  Source
      Feedback : in     Feedback_proc)
   is
      lh : Zip.Headers.Local_File_Header;
      data_descriptor_after_data : Boolean;
      offset : Unsigned_64;
   begin
      Zip.Headers.Read_and_check (Stream, lh);
      data_descriptor_after_data := (lh.bit_flag and 8) /= 0;
      --  Copy name and extra field
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
        offset := Unsigned_64 (Info.Stream.Index) - 1;
        Info.Contains (Info.Last_entry).head.local_header_offset := offset;
        Info.Contains (Info.Last_entry).name := new String'(name);
        --  Copy local header to new stream.
        --  Extra field, zip_64 or another kind, is copied.
        Zip.Headers.Write (Info.Stream.all, lh, from_header);
        --  Copy entry name to new stream:
        String'Write (Info.Stream, name);
        --  Copy extra field to new stream, usually a Zip64 field:
        String'Write (Info.Stream, extra);
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

   use Ada.Streams;

   procedure Dispose is new
     Ada.Unchecked_Deallocation (
       Stream_Element_Array,
       Stream_Element_Array_Access);

   procedure Resize (A           : in out Stream_Element_Array_Access;
                     A_Last_Used :        Stream_Element_Offset;
                     New_Size    :        Stream_Element_Offset)
   is
     Hlp : constant Stream_Element_Array_Access :=
                      new Stream_Element_Array (1 .. New_Size);
   begin
     if A = null then
       A := Hlp;
     else
       for I in 1 .. Stream_Element_Offset'Min (Hlp'Last, A_Last_Used) loop
         Hlp (I) := A (I);
       end loop;
       Dispose (A);
       A := Hlp;
     end if;
   end Resize;

   overriding procedure Write
     (Stream : in out Zip_Entry_Stream_Type;
      Item   :        Ada.Streams.Stream_Element_Array)
   is
     Needed : Stream_Element_Offset;
   begin
     if Stream.Buffer_Access = null then
       raise Ada.IO_Exceptions.Use_Error
         with "Stream is not open (Zip_Entry_Stream_Type)";
     end if;
     Needed := Stream.Last_Element + Item'Length;
     if Stream.Buffer_Access'Length < Needed then
       declare
         New_Size : Stream_Element_Offset := Stream.Buffer_Access'Length;
         Growth : constant Stream_Element_Offset
                    := Stream_Element_Offset (Stream.Growth);
       begin
         loop
           if New_Size > Stream_Element_Offset'Last / Growth then
             --  We want to avoid an out-of-range with New_Size * Growth.
             raise Constraint_Error
               with "Buffer capacity exhaustion (Zip_Entry_Stream_Type)";
           end if;
           New_Size := New_Size * Growth;
           exit when New_Size >= Needed;
         end loop;
         --  Ada.Text_IO.Put_Line("Grow");
         Resize (Stream.Buffer_Access, Stream.Last_Element, New_Size);
       end;
     end if;
     --
     for I in Item'Range loop
       Stream.Last_Element := Stream.Last_Element + 1;
       Stream.Buffer_Access (Stream.Last_Element) := Item (I);
     end loop;
   end Write;

   procedure Open (
     Zip_Entry_Stream     :    out Zip_Entry_Stream_Type;
     Initial_Buffer_Size  : in     Positive := Default_Zip_Entry_Buffer_Size;
     Buffer_Growth_Factor : in     Positive := Default_Zip_Entry_Buffer_Growth
   )
   is
   begin
     Zip_Entry_Stream.Last_Element := 0;
     Zip_Entry_Stream.Growth := Buffer_Growth_Factor;
     Resize (
       Zip_Entry_Stream.Buffer_Access,
       Zip_Entry_Stream.Last_Element,
       Stream_Element_Offset (Initial_Buffer_Size)
     );
   end Open;

   procedure Close (
     Zip_Entry_Stream : in out Zip_Entry_Stream_Type;
     Entry_Name       : in     String;
     Creation_Time    : in     Zip.Time := default_creation_time;
     Info             : in out Zip_Create_Info
   )
   is
     --  We define a local reader class for reading the contents of
     --  Zip_Entry_Stream as an *input* stream.
     type Captive_Type is new Zip_Streams.Root_Zipstream_Type with record
       Loc : Stream_Element_Offset := 1;
     end record;
     --
     overriding procedure Read
       (Stream : in out Captive_Type;
        Item   : out Stream_Element_Array;
        Last   : out Stream_Element_Offset);
     overriding procedure Write
       (Stream : in out Captive_Type;
        Item   :        Stream_Element_Array) is null;
     overriding function Index (S : in Captive_Type) return Zip_Streams.ZS_Index_Type;
     overriding function Size (S : in Captive_Type) return Zip_Streams.ZS_Size_Type;
     overriding function End_Of_Stream (S : in Captive_Type) return Boolean;
     --
     overriding procedure Set_Index (
        S  : in out Captive_Type;
        To :        Zip_Streams.ZS_Index_Type)
     is
     begin
       S.Loc := Stream_Element_Offset (To);
     end Set_Index;
     --
     overriding function Index (S : in Captive_Type) return Zip_Streams.ZS_Index_Type is
     begin
       return Zip_Streams.ZS_Index_Type (S.Loc);
     end Index;
     --
     overriding function Size (S : in Captive_Type) return Zip_Streams.ZS_Size_Type is
     pragma Unreferenced (S);
     begin
       return Zip_Streams.ZS_Size_Type (Zip_Entry_Stream.Last_Element);
     end Size;
     --
     overriding function End_Of_Stream (S : in Captive_Type) return Boolean is
     begin
       return S.Loc > Zip_Entry_Stream.Last_Element;
     end End_Of_Stream;
     --
     overriding procedure Read
       (Stream : in out Captive_Type;
        Item   : out Stream_Element_Array;
        Last   : out Stream_Element_Offset)
     is
       Available_From_Buffer : constant Stream_Element_Offset :=
         Stream_Element_Offset'Max (
           0,
           1 + Zip_Entry_Stream.Last_Element - Stream.Loc
           --  When Stream.Loc is equal to Zip_Entry_Stream.Last_Element,
           --  there is one (last) element to read.
         );
       Copy_Length : constant Stream_Element_Offset :=
         Stream_Element_Offset'Min (Item'Length, Available_From_Buffer);
     begin
       --  Read Copy_Length bytes from Zip_Entry_Stream.Buffer_Access,
       --  position Stream.Loc, into Item.
       --  Copy_Length = 0 when Item is empty or the buffer is
       --  fully read (i.e., when Loc = Last_Element + 1).
       Last := Item'First + Copy_Length - 1;
       for Offset in reverse 0 .. Copy_Length - 1 loop
         Item (Item'First + Offset) :=
           Zip_Entry_Stream.Buffer_Access (Stream.Loc + Offset);
       end loop;
       Stream.Loc := Stream.Loc + Copy_Length;
     end Read;
     --
     Reader_Stream : Captive_Type;
     use type Zip_Streams.Time;
   begin
     Reader_Stream.Set_Name (Entry_Name);
     if Creation_Time = use_clock
               --  If we have use_file_modification_time by mistake, use clock as well:
       or else Creation_Time = use_file_modification_time
     then
       Reader_Stream.Set_Time (Ada.Calendar.Clock);
     else
       Reader_Stream.Set_Time (Creation_Time);
     end if;
     --
     Add_Stream (Info, Reader_Stream);
     --
     Dispose (Zip_Entry_Stream.Buffer_Access);
   end Close;

   procedure Finish (Info : in out Zip_Create_Info) is
      ed : Zip.Headers.End_of_Central_Dir;
      procedure Dispose is new Ada.Unchecked_Deallocation (String, p_String);
      current_index : Zip_Streams.ZS_Index_Type;
      --
      --  If the stream is of File_Zipstream type or descendent, close the file too.
      --  Deallocate catalogue entries.
      procedure Close_eventual_file_and_deallocate is
      begin
        if Info.Stream.all in Zip_Streams.File_Zipstream'Class
          and then Zip_Streams.File_Zipstream (Info.Stream.all).Is_Open
        then
          Zip_Streams.File_Zipstream (Info.Stream.all).Close;
        end if;
        if Info.Contains /= null then
          for e in 1 .. Info.Last_entry loop
            Dispose (Info.Contains (e).name);
          end loop;
          Dispose (Info.Contains);
        end if;
        Info.Last_entry := 0;
        Info.name_dictionary.Clear;
      end Close_eventual_file_and_deallocate;
      --
      needs_local_zip64 : Boolean;
      fh_extra : Local_File_Header_Extension;
      ed64l    : Zip64_End_of_Central_Dir_Locator;
      ed64     : Zip64_End_of_Central_Dir;
   begin
      --
      --  2/ Almost done - write Central Directory:
      --
      current_index := Info.Stream.Index;
      ed.central_dir_offset := Unsigned_64 (current_index) - 1;
      ed.total_entries := 0;
      ed.central_dir_size := 0;
      ed.main_comment_length := 0;
      if Info.zip_archive_format = Zip_32
        and then Info.Last_entry >= Integer_M32 (Unsigned_16'Last)
      then
        --  Promote format to Zip_64 (too many entries for Zip_32).
        Info.zip_archive_format := Zip_64;
      end if;
      if Info.Contains /= null then
        for cat of Info.Contains (1 .. Info.Last_entry) loop
          ed.total_entries := ed.total_entries + 1;
          needs_local_zip64 :=
            Needs_Local_Zip_64_Header_Extension
              (cat.head.short_info, cat.head.local_header_offset);
          if needs_local_zip64 then
            cat.head.short_info.extra_field_length := local_header_extension_length;
            fh_extra.tag  := 1;
            fh_extra.size := local_header_extension_length - 4;
            fh_extra.value_64 (1) := cat.head.short_info.dd.uncompressed_size;
            fh_extra.value_64 (2) := cat.head.short_info.dd.compressed_size;
            fh_extra.value_64 (3) := cat.head.local_header_offset;
            cat.head.short_info.dd.uncompressed_size := 16#FFFF_FFFF#;
            cat.head.short_info.dd.compressed_size   := 16#FFFF_FFFF#;
            cat.head.local_header_offset             := 16#FFFF_FFFF#;
            --  Promote format to Zip_64 (entry too large for Zip_32).
            Info.zip_archive_format := Zip_64;
          else
            --  If there is no Zip_64 information,
            --  we set the extra header in the central dirctory header as empty.
            cat.head.short_info.extra_field_length := 0;
          end if;
          Write (Info.Stream.all, cat.head);
          String'Write (Info.Stream, cat.name.all);
          if needs_local_zip64 then
            Write (Info.Stream.all, fh_extra, False);
          end if;
          ed.central_dir_size :=
            ed.central_dir_size +
              Headers.central_header_length +
                Unsigned_64 (cat.head.short_info.filename_length) +
                Unsigned_64 (cat.head.short_info.extra_field_length);
          current_index := Info.Stream.Index;
        end loop;
        Check_Size (Info, current_index);
      end if;
      ed.disknum := 0;
      ed.disknum_with_start := 0;
      ed.disk_total_entries := ed.total_entries;
      --
      if Info.zip_archive_format = Zip_64 then
        ed64l.number_of_the_disk_with_the_start_of_the_zip64_end_of_central_dir := 0;
        ed64l.relative_offset_of_the_zip64_end_of_central_dir_record :=
          Unsigned_64 (Info.Stream.Index - 1);
        ed64l.total_number_of_disks := 1;
        --
        ed64.size := 44;
        ed64.version_made_by           := 16#2D#;
        ed64.version_needed_to_extract := 16#2D#;
        ed64.number_of_this_disk                                        := ed.disknum;
        ed64.number_of_the_disk_with_the_start_of_the_central_directory := ed.disknum_with_start;
        ed64.total_number_of_entries_in_the_central_directory_on_this_disk := ed.disk_total_entries;
        ed64.total_number_of_entries_in_the_central_directory              := ed.total_entries;
        ed64.size_of_the_central_directory        := ed.central_dir_size;
        ed64.offset_of_start_of_central_directory := ed.central_dir_offset;
        Write (Info.Stream.all, ed64);
        --
        Write (Info.Stream.all, ed64l);
        --
        ed.disk_total_entries := 16#FFFF#;
        ed.total_entries      := 16#FFFF#;
        ed.central_dir_size   := 16#FFFF_FFFF#;
        ed.central_dir_offset := 16#FFFF_FFFF#;
      end if;
      Write (Info.Stream.all, ed);
      --
      Close_eventual_file_and_deallocate;
   end Finish;

end Zip.Create;
