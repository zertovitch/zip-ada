--  Zip archive creation
--
--  Contributed by ITEC - NXP Semiconductors
--  June 2008
--

--  Legal licensing note:

--  Copyright (c) 2008 .. 2018 Gautier de Montmollin (maintainer)
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

-- NB: this is the MIT License, as found 21-Aug-2016 on the site
-- http://www.opensource.org/licenses/mit-license.php

--
--  Change log:
--  ==========
--
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

with Zip.Headers; use Zip.Headers;
with Zip.Compress; use Zip.Compress;
with Zip_Streams; use Zip_Streams;

with Ada.Containers.Hashed_Maps;
with Ada.Strings.Unbounded.Hash;

package Zip.Create is

   type Zip_Create_info is private;

   -- Create the Zip archive; create the file if the stream is a file

   procedure Create(Info          : out Zip_Create_info;
                    Z_Stream      : in Zipstream_Class_Access;
                    Name          : String;
                    Compress      : Zip.Compress.Compression_Method:= Zip.Compress.Deflate_1;
                    Duplicates    : Duplicate_name_policy:= admit_duplicates
   );

   function Is_Created(Info: Zip_Create_info) return Boolean;

   -- Set a new compression format for the next data to be added to the archive.
   -- Can be useful if data are known to be already compressed - or not.

   procedure Set(Info       : in out Zip_Create_info;
                 New_Method : Zip.Compress.Compression_Method);

   function Name(Info: Zip_Create_info) return String;

   -- Add a new entry to a Zip archive, from a general input Zipstream

   procedure Add_Stream (Info     : in out Zip_Create_info;
                         Stream   : in out Root_Zipstream_Type'Class;
                         Password : in     String:= "");

   procedure Add_Stream (Info           : in out Zip_Create_info;
                         Stream         : in out Root_Zipstream_Type'Class;
                         Feedback       : in     Feedback_proc;
                         Password       : in     String:= "";
                         Compressed_Size:    out Zip.File_size_type;
                         Final_Method   :    out Natural);

   -- Add a new entry to a Zip archive, from an entire file

   procedure Add_File (Info              : in out Zip_Create_info;
                       Name              : String;
                       Name_in_archive   : String:= "";
                       -- default: add the file in the archive
                       -- under the same name
                       Delete_file_after : Boolean:= False;
                       -- practical to delete temporary file after adding
                       Name_encoding     : Zip_name_encoding:= IBM_437;
                       Modification_time : Time:= default_time;
                       Is_read_only      : Boolean:= False;
                       Feedback          : Feedback_proc:= null;
                       Password          : String:= ""
   );

   -- Add a new entry to a Zip archive, from a buffer stored in a string

   procedure Add_String (Info              : in out Zip_Create_info;
                         Contents          : String;
                         Name_in_archive   : String;
                         --   Name_UTF_8_encoded = True if Name is actually UTF-8 encoded (Unicode)
                         Name_UTF_8_encoded: Boolean:= False;
                         Password          : String:= "";
                         --  Time stamp for this entry, e.g. Zip.Convert(Ada.Calendar.Clock)
                         Creation_time     : Zip.Time:= default_time
   );

   use Ada.Strings.Unbounded;

   procedure Add_String (Info              : in out Zip_Create_info;
                         Contents          : Unbounded_String;
                         Name_in_archive   : String;
                         --   Name_UTF_8_encoded = True if Name is actually UTF-8 encoded (Unicode)
                         Name_UTF_8_encoded: Boolean:= False;
                         Password          : String:= "";
                         --  Time stamp for this entry, e.g. Zip.Convert(Ada.Calendar.Clock)
                         Creation_time     : Zip.Time:= default_time
   );

   --  Add a new entry to a Zip archive, copied from another Zip archive.
   --  This is useful for duplicating archives with some differences, like adding, replacing,
   --  removing or recompressing entries - see the AZip file manager for an application example.
   --  The streams' indices are set at the beginning of local headers in both archives.
   --
   procedure Add_Compressed_Stream (
     Info           : in out Zip_Create_info;            --  Destination
     Stream         : in out Root_Zipstream_Type'Class;  --  Source
     Feedback       : in     Feedback_proc
   );

   --  Complete the Zip archive; close the file if the stream is a file

   procedure Finish (Info       : in out Zip_Create_info);

   --  The following is raised on cases when the Zip archive creation exceeds
   --  the Zip_32 format's capacity: 4GB total size, 65535 entries.

   Zip_Capacity_Exceeded : exception;

private

   type p_String is access String;

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

   type Zip_Create_info is record
     Stream             : Zipstream_Class_Access;
     Compress           : Zip.Compress.Compression_Method;
     Contains           : Pdir_entries := null;
     --  'Contains' has unused room, to avoid reallocating each time:
     Last_entry         : Natural := 0;
     Duplicates         : Duplicate_name_policy;
     --  We set up a name dictionary just for avoiding duplicate entries:
     dir                : Name_mapping.Map;
     zip_archive_format : Zip_archive_format_type := Zip_32;
   end record;

end Zip.Create;
