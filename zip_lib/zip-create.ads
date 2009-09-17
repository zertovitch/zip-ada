-- Contributed by ITEC - NXP Semiconductors
-- June 2008
--
-- Change log:
-- ==========
-- 17-Feb-2009: GdM: Added procedure Add_String
-- 10-Feb-2009: GdM: Create / Finish: if Info.Stream is to a file,
--                     the underling file is also created / closed in time
--  4-Feb-2009: GdM: Added procedure Add_File
--

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Zip.Headers; use Zip.Headers;
with Zip.Compress; use Zip.Compress;
with Zip_Streams; use Zip_Streams;

package Zip.Create is

   type Zip_Create_info is private;

   -- Create the Zip archive; create the file if the stream is a file

   procedure Create(Info        : out Zip_Create_info;
                    Z_Stream    : in Zipstream_Class;
                    Name        : String;
                    Compress    : Zip.Compress.Compression_Method:= Zip.Compress.Shrink);


   -- Add new entries to a Zip archive, from a general stream

   procedure Add_Stream (Info   : in out Zip_Create_info;
                         Stream : Zipstream_Class);

   procedure Add_Stream (Info           : in out Zip_Create_info;
                         Stream         : Zipstream_Class;
                         Feedback       : in     Feedback_proc;
                         Compressed_Size:    out Zip.File_size_type;
                         Final_Method   :    out Natural);

   -- Add new entries to a Zip archive, from a file

   procedure Add_File (Info              : in out Zip_Create_info;
                       Name              : String;
                       Name_in_archive   : String:= "";
                       -- default: add the file in the archive
                       -- under the same name
                       Delete_file_after : Boolean:= False
                       -- practical to delete temporary file after
                       -- adding
   );

   -- Add new entries to a Zip archive, from a buffer stored in a string

   procedure Add_String (Info            : in out Zip_Create_info;
                         Contents        : String;
                         Name_in_archive : String
   );

   procedure Add_String (Info            : in out Zip_Create_info;
                         Contents        : Unbounded_String;
                         Name_in_archive : String
   );

   -- Complete the Zip archive; close the file if the stream is a file

   procedure Finish (Info       : in out Zip_Create_info);

private

   type Dir_entry is record
      head : Zip.Headers.Central_File_Header;
      name : Unbounded_String;
   end record;

   type Dir_entries is array (Integer range <>) of Dir_entry;
   type Pdir_entries is access Dir_entries;

   type Zip_Create_info is record
      Stream   : Zipstream_Class;
      compress : Zip.Compress.Compression_Method;
      Contains : Pdir_entries:= null;
   end record;

end Zip.Create;
