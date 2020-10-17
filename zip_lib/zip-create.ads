--  Zip archive creation
--
--  Contributed by ITEC - NXP Semiconductors
--  June 2008
--

--  Legal licensing note:

--  Copyright (c) 2008 .. 2020 Gautier de Montmollin (maintenance and further development)
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

--
--  Change log:
--  ==========
--
--  17-Aug-2020: GdM: Added Zip_Entry_Stream_Type.
--  23-Mar-2016: GdM: Create with Duplicate_name_policy
--  14-Feb-2015: GdM: Added "Is_Created" function
--  13-Feb-2015: GdM: Added "Password" parameter
--  30-Oct-2012: GdM: Removed all profiles using Zip_Streams' objects
--                       with accesses (cf 25-Oct's modifications)
--  26-Oct-2012: GdM: Added Add_Compressed_Stream
--  25-Oct-2012: GdM: Some procedures using Zip_Streams' objects also with
--                      pointer-free profiles (no more 'access' or access type)
--  14-Oct-2012: GdM: Added Set procedure for changing compression method
--  30-Mar-2010: GdM: Added Name function
--  25-Feb-2010: GdM: Fixed major bottlenecks around Dir_entries
--                      -> 5x faster overall for 1000 files, 356x for 100'000 !
--  17-Feb-2009: GdM: Added procedure Add_String
--  10-Feb-2009: GdM: Create / Finish: if Info.Stream is to a file,
--                      the underling file is also created / closed in time
--   4-Feb-2009: GdM: Added procedure Add_File
--

with Zip.Compress;
with Zip.Headers;
with Zip_Streams;

with Ada.Containers.Hashed_Maps;
with Ada.Strings.Unbounded.Hash;

package Zip.Create is

   type Zip_Create_Info is private;

   subtype Zip_File_Stream is Zip_Streams.File_Zipstream;
   --  You can use this type for creating Zip archives as files.
   subtype Zip_Memory_Stream is Zip_Streams.Memory_Zipstream;
   --  You can use this type for creating Zip archives in memory.

   --  Create the Zip archive; create the file if the stream is a file

   procedure Create_Archive (
      Info            : out Zip_Create_Info;
      Z_Stream        : in Zip_Streams.Zipstream_Class_Access;
      Archive_Name    : String;
      Compress_Method : Zip.Compress.Compression_Method := Zip.Compress.Deflate_1;
      Duplicates      : Duplicate_name_policy           := admit_duplicates
   );

   function Is_Created (Info : Zip_Create_Info) return Boolean;

   --  Set a new compression format for the next data to be added to the archive.
   --  Can be useful if data are known to be already compressed - or not.

   procedure Set (Info       : in out Zip_Create_Info;
                  New_Method :        Zip.Compress.Compression_Method);

   function Name (Info : Zip_Create_Info) return String;

   --  Add a new entry to a Zip archive, from a general *input* Zipstream
   --  The entry's name is set by Set_Name on the Stream before calling Add_Stream.

   procedure Add_Stream (Info     : in out Zip_Create_Info;
                         Stream   : in out Zip_Streams.Root_Zipstream_Type'Class;
                         Password : in     String := "");

   procedure Add_Stream (Info            : in out Zip_Create_Info;
                         Stream          : in out Zip_Streams.Root_Zipstream_Type'Class;
                         Feedback        : in     Feedback_proc;
                         Password        : in     String := "";
                         Compressed_Size :    out Zip.Zip_32_Data_Size_Type;
                         Final_Method    :    out Natural);

   default_time                  : Zip_Streams.Time renames Zip_Streams.default_time;

   --  If use_file_modification_time is passed to Add_File, Ada.Directories.Modification_Time
   --  will be called on File_Name and that time will be used for setting the Zip entry's time
   --  stamp. NB: Ada.Directories.Modification_Time is not reliable: it may fail on UTF-8 file
   --  names on some Ada systems.
   --
   use_file_modification_time    : Zip_Streams.Time renames Zip_Streams.special_time_1;

   --  If use_clock is passed to Add_File or Add_String, Ada.Calendar.Clock will be called
   --  and that time will be used for setting the Zip entry's time stamp.
   --  NB: Ada.Calendar.Clock may be time-consuming on some Ada systems.
   --
   use_clock                     : Zip_Streams.Time renames Zip_Streams.special_time_2;

   --  Add a new entry to a Zip archive, from an entire file

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
   );

   --  Add a new entry to a Zip archive, from a buffer stored in a string

   procedure Add_String (Info               : in out Zip_Create_Info;
                         Contents           : String;
                         Name_in_archive    : String;
                         --  Name_UTF_8_encoded = True if Name is actually UTF-8 encoded (Unicode)
                         Name_UTF_8_encoded : Boolean  := False;
                         Password           : String   := "";
                         --  Time stamp for this entry
                         Creation_time      : Zip.Time := default_time
   );

   use Ada.Strings.Unbounded;

   procedure Add_String (Info               : in out Zip_Create_Info;
                         Contents           : Unbounded_String;
                         Name_in_archive    : String;
                         --  Name_UTF_8_encoded = True if Name is actually UTF-8 encoded (Unicode)
                         Name_UTF_8_encoded : Boolean  := False;
                         Password           : String   := "";
                         --  Time stamp for this entry
                         Creation_time      : Zip.Time := default_time
   );

   --  Add a new entry to a Zip archive, copied from another Zip archive.
   --  This is useful for duplicating archives with some differences, like adding, replacing,
   --  removing or recompressing entries - see the AZip file manager for an application example.
   --  The streams' indices are set at the beginning of local headers in both archives.
   --
   procedure Add_Compressed_Stream (
     Info           : in out Zip_Create_Info;                        --  Destination
     Stream         : in out Zip_Streams.Root_Zipstream_Type'Class;  --  Source
     Feedback       : in     Feedback_proc
   );

   --  Zip_Entry_Stream_Type
   -------------------------
   --  With that type, you can add an entry as an *output* stream
   --  to a Zip archive. The workflow is:
   --
   --     Create_Archive (Info, ...);
   --     [for each entry]:
   --         Open (Zip_Entry_Stream, Guess);  --  Guess = guess of data size
   --         [various occurrences of]: T'Write (Zip_Entry_Stream, Data);
   --         Close (Zip_Entry_Stream, "contents.dat", Info);
   --     Finish (Info);
   --
   --  For a full example, see: test/test_zip_entry_stream.adb

   type Zip_Entry_Stream_Type is
     new Ada.Streams.Root_Stream_Type with private;

   Default_Zip_Entry_Buffer_Size   : constant := 1024 ** 2;
   Default_Zip_Entry_Buffer_Growth : constant := 8;

   procedure Open (
     Zip_Entry_Stream     :    out Zip_Entry_Stream_Type;
     Initial_Buffer_Size  : in     Positive := Default_Zip_Entry_Buffer_Size;
     Buffer_Growth_Factor : in     Positive := Default_Zip_Entry_Buffer_Growth
   );

   procedure Close (
     Zip_Entry_Stream : in out Zip_Entry_Stream_Type;
     Entry_Name       : in     String;
     Creation_Time    : in     Zip.Time := default_time;
     Info             : in out Zip_Create_Info
   );

   --  Finish: complete the Zip archive when all desired entries have
   --  been added; close the Zip file if the archive stream is in
   --  File_Zipstream's class.
   --
   procedure Finish (Info : in out Zip_Create_Info);

   --  The following exception is raised on cases when the Zip archive
   --  creation exceeds the Zip_32 format's capacity: 4 GiB total size,
   --  65535 entries.

   Zip_Capacity_Exceeded : exception;

private

   type Dir_entry is record
     head : Zip.Headers.Central_File_Header;
     name : p_String;
   end record;

   type Dir_entries is array (Positive range <>) of Dir_entry;
   type Pdir_entries is access Dir_entries;

   --  The use of Hashed_Maps makes Test_Zip_Create_Info_Timing run ~10x faster than
   --  with the unbalanced binary tree of previous versions.
   --
   package Name_mapping is
     new Ada.Containers.Hashed_Maps (Unbounded_String, Positive, Hash, "=");

   type Zip_Create_Info is record
     Stream             : Zip_Streams.Zipstream_Class_Access;
     Compress           : Zip.Compress.Compression_Method;
     Contains           : Pdir_entries := null;
     --  'Contains' has unused room, to avoid reallocating each time:
     Last_entry         : Natural := 0;
     Duplicates         : Duplicate_name_policy;
     --  We set up a name dictionary just for detecting duplicate entries:
     name_dictionary    : Name_mapping.Map;
     zip_archive_format : Zip_archive_format_type := Zip_32;
   end record;

   type Stream_Element_Array_Access is
     access Ada.Streams.Stream_Element_Array;

   type Zip_Entry_Stream_Type is new Ada.Streams.Root_Stream_Type with record
     Buffer_Access : Stream_Element_Array_Access := null;
     Last_Element  : Ada.Streams.Stream_Element_Offset;
     Growth        : Positive;
   end record;

   overriding procedure Read
     (Stream : in out Zip_Entry_Stream_Type;
      Item   :    out Ada.Streams.Stream_Element_Array;
      Last   :    out Ada.Streams.Stream_Element_Offset)
   is null;

   overriding procedure Write
     (Stream : in out Zip_Entry_Stream_Type;
      Item   :        Ada.Streams.Stream_Element_Array);

end Zip.Create;
