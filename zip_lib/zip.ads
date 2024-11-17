--   ________  ___   ______       ______      ___
--  /___..._/  |.|   |.___.\     /. __ .\   __|.|   ____
--     /../    |.|   |.____/     |.|__|.|  /....|  __\..\
--   _/../___  |.|   |.|    ===  |..__..| |. = .| | = ..|
--  /_______/  |_|  /__|        /__|  |_|  \__\_|  \__\_|

--  Zip library
---------------
--
--  Library for manipulating archive files in the Zip format
--
--  Pure Ada 2005+ code, 100% portable: OS-, CPU- and compiler- independent.
--
--  Version / date / download info: see the version, reference, web strings
--   defined at the end of the public part of this package.

--  Legal licensing note:

--  Copyright (c) 1999 .. 2024 Gautier de Montmollin
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

--  NB: this is the MIT License, as found 12-Sep-2007 on the site
--  http://www.opensource.org/licenses/mit-license.php

with Zip_Streams;
with Ada.Calendar, Ada.Finalization, Ada.Streams.Stream_IO, Ada.Text_IO;
with Interfaces;
with System;

package Zip is

  -----------------------------------------------------------------
  --  Zip_Info                                                   --
  -----------------------------------------------------------------
  --  Zip_Info contains the Zip file name (if it is a file)      --
  --  or its input stream access, and the archive's directory.   --
  -----------------------------------------------------------------

  type Zip_Info is
    new Ada.Finalization.Controlled with private;

  -----------------------------------------------
  --  Load the whole .zip directory contained  --
  --  in archive (from) for quick searching.   --
  -----------------------------------------------

   type Duplicate_name_policy is
     (admit_duplicates,     --  two entries in the Zip archive may have the same full name
      error_on_duplicate);  --  raise exception on attempt to add twice the same entry name

  --  Load from a file

  procedure Load
    (info            : out Zip_Info;
     from            : in  String;  --  Zip file name
     case_sensitive  : in  Boolean := False;
     duplicate_names : in  Duplicate_name_policy := error_on_duplicate);

  --  Load from a stream

  procedure Load
    (info            :    out Zip_Info;
     from            : in out Zip_Streams.Root_Zipstream_Type'Class;
     case_sensitive  : in     Boolean := False;
     duplicate_names : in     Duplicate_name_policy := error_on_duplicate);

  Archive_corrupted,
  Archive_open_error,
  Duplicate_name : exception;

  Zip_file_open_error : exception renames Archive_open_error;  --  Archive is not always a file!
  pragma Obsolescent (Zip_file_open_error, "Better use the name: Archive_open_error");

  --  Zip_File_Error: exception renames Archive_corrupted;   --   Now really obsolete.
  --  pragma Obsolescent(Zip_File_Error);                    --   Now really obsolete.

  function Is_loaded (info : in Zip_Info) return Boolean;

  function Zip_Name (info : in Zip_Info) return String;

  function Zip_Comment (info : in Zip_Info) return String;

  function Zip_Stream (info : in Zip_Info) return Zip_Streams.Zipstream_Class_Access;

  function Entries (info : in Zip_Info) return Natural;

  procedure Delete (info : in out Zip_Info);
  pragma Obsolescent (Delete, "Delete happens automatically since v.56.");

  Forgot_to_load_zip_info : exception;

  --  Data sizes in archive
  subtype Zip_32_Data_Size_Type is Interfaces.Unsigned_32;
  subtype Zip_64_Data_Size_Type is Interfaces.Unsigned_64;

  ---------

  --  Compression "methods" - actually, *formats* - in the "official" PKWARE Zip format.
  --  Details in appnote.txt, part V.J
  --
  --     C : supported by Zip-Ada for compressing
  --     D : supported by Zip-Ada for decompressing

  type PKZip_method is
    (store,       --  C, D
     shrink,      --  C, D
     reduce_1,    --  C, D
     reduce_2,    --  C, D
     reduce_3,    --  C, D
     reduce_4,    --  C, D
     implode,     --     D
     tokenize,
     deflate,     --  C, D
     deflate_e,   --     D  -  "Enhanced deflate" or "Deflate64"
     bzip2_meth,  --  C, D
     lzma_meth,   --  C, D
     zstandard,
     mp3_recomp,
     xz_recomp,
     jpeg_recomp,
     wavpack,
     ppmd,
     unknown);

  subtype Reduce_Format is PKZip_method range reduce_1 .. reduce_4;

  --  Return a String image, nicer than the 'Image attribute.
  function Image (m : PKZip_method) return String;

  --  Technical: translates the method code as set in zip archives
  function Method_from_Code (x : Interfaces.Unsigned_16) return PKZip_method;
  function Method_from_Code (x : Natural) return PKZip_method;

  --  Internal time definition
  subtype Time is Zip_Streams.Time;
  function Convert (date : in Ada.Calendar.Time) return Time
    renames Zip_Streams.Calendar.Convert;
  function Convert (date : in Time) return Ada.Calendar.Time
    renames Zip_Streams.Calendar.Convert;

  --  Entry names within Zip archives are encoded either with
  --      * the IBM PC (the one with a monochrome screen, only text mode)'s
  --          character set: IBM 437
  --  or
  --      * Unicode UTF-8
  --
  --  Documentation: PKWARE's Appnote.txt, APPENDIX D - Language Encoding (EFS)

  type Zip_Name_Encoding is (IBM_437, UTF_8);

  --  Traverse a whole Zip_info directory in sorted order, giving the
  --  name for each entry to an user-defined "Action" procedure.
  --  Concretely, you can process a whole Zip file that way, by extracting data
  --  with Extract, or open a reader stream with UnZip.Streams.
  --  See the Comp_Zip or Find_Zip tools as application examples.
  generic
    with procedure Action (name : String);  --  'name' is compressed entry's name
  procedure Traverse (z : Zip_Info);

  --  Same as Traverse, but Action gives also full name information.
  --  The pair (name, name_encoding) allows for an unambiguous Unicode
  --  name decoding. See the AZip project for an implementation.
  generic
    with procedure Action
      (name          : String;  --  'name' is compressed entry's name
       name_encoding : Zip_Name_Encoding);
  --
  procedure Traverse_Unicode (z : Zip_Info);

  --  Same as Traverse, but Action gives also full technical informations
  --  about the compressed entry.
  generic
    with procedure Action
      (name             : String;  --  'name' is compressed entry's name
       file_index       : Zip_Streams.ZS_Index_Type;
       comp_size        : Zip_64_Data_Size_Type;
       uncomp_size      : Zip_64_Data_Size_Type;
       crc_32           : Interfaces.Unsigned_32;
       date_time        : Time;
       method           : PKZip_method;
       name_encoding    : Zip_Name_Encoding;
       read_only        : Boolean;
       encrypted_2_x    : Boolean;  --  PKZip 2.x encryption
       user_code        : in out Integer);
  --
  procedure Traverse_verbose (z : Zip_Info);

  --  Academic: see how well the name tree is balanced
  procedure Tree_Stat
    (z         : in     Zip_Info;
     total     :    out Natural;
     max_depth :    out Natural;
     avg_depth :    out Float);

  --------------------------------------------------------------------------
  -- Offsets - various procedures giving 1-based indexes to local headers --
  --------------------------------------------------------------------------

  --  Find 1st offset in a Zip stream (i.e. the first's archived entry's offset)

  procedure Find_first_Offset
    (file           : in out Zip_Streams.Root_Zipstream_Type'Class;
     file_index     :    out Zip_Streams.ZS_Index_Type);

  --  If the archive is empty (the 22 byte .zip file), there is no first entry or offset.
  Archive_is_empty : exception;

  --  Find offset of a certain compressed file
  --  in a Zip file (file opened and kept open)

  procedure Find_Offset
    (file           : in out Zip_Streams.Root_Zipstream_Type'Class;
     name           : in     String;
     case_sensitive : in     Boolean;
     file_index     :    out Zip_Streams.ZS_Index_Type;
     comp_size      :    out Zip_64_Data_Size_Type;
     uncomp_size    :    out Zip_64_Data_Size_Type;
     crc_32         :    out Interfaces.Unsigned_32);

  --  Find offset of a certain compressed file in a pre-loaded Zip_info data

  procedure Find_Offset
    (info           : in     Zip_Info;
     name           : in     String;
     name_encoding  :    out Zip_Name_Encoding;
     file_index     :    out Zip_Streams.ZS_Index_Type;
     comp_size      :    out Zip_64_Data_Size_Type;
     uncomp_size    :    out Zip_64_Data_Size_Type;
     crc_32         :    out Interfaces.Unsigned_32);

  --  Find offset of a certain compressed file in a pre-loaded Zip_info data.
  --  This version scans the whole catalogue and returns the index of the first
  --  entry with a matching name, ignoring directory information.
  --  For instance, if the Zip archive contains "zip-ada/zip_lib/zip.ads",
  --  "zip.ads" will match - or even "ZIP.ads" if info has been loaded in case-insensitive mode.
  --  Caution: this may be much slower than the exact search with Find_offset.

  procedure Find_Offset_without_Directory
    (info           : in     Zip.Zip_Info;
     name           : in     String;
     name_encoding  :    out Zip.Zip_Name_Encoding;
     file_index     :    out Zip_Streams.ZS_Index_Type;
     comp_size      :    out Zip_64_Data_Size_Type;
     uncomp_size    :    out Zip_64_Data_Size_Type;
     crc_32         :    out Interfaces.Unsigned_32);

  Entry_name_not_found : exception;
  File_name_not_found : exception renames Entry_name_not_found;
  pragma Obsolescent (File_name_not_found, "Better use the name: Entry_name_not_found");

  function Exists (info : Zip_Info; name : String) return Boolean;

  --  User code: any information e.g. as a result of a string search,
  --  archive comparison, archive update, recompression,...

  procedure Set_User_Code (info : Zip_Info; name : String; code : Integer);

  function User_Code (info : Zip_Info; name : String) return Integer;

  procedure Get_Sizes
    (info           : in     Zip_Info;
     name           : in     String;
     comp_size      :    out Zip_64_Data_Size_Type;
     uncomp_size    :    out Zip_64_Data_Size_Type);

  --  User-defined procedure for feedback occuring during
  --  compression or decompression (entry_skipped meaningful
  --  only for the latter)

  type Feedback_Proc is access
    procedure
      (percents_done : in     Natural;   --  %'s completed
       entry_skipped : in     Boolean;   --  indicates one can show "skipped", no %'s
       user_abort    :    out Boolean);  --  e.g. transmit a "click on Cancel" here

  -------------------------------------------------------------------------
  -- Goodies - things used internally by Zip-Ada but are not bound to    --
  -- Zip archive purposes and that might be generally useful.            --
  -------------------------------------------------------------------------

  --  Block_Read: general-purpose procedure (nothing really specific to Zip /
  --  UnZip): reads either the whole buffer from a file, or if the end of
  --  the file lays inbetween, a part of the buffer.
  --
  --  The procedure's names and parameters corresponds to Borland / Turbo
  --  Pascal / Delphi's BlockRead's.

  subtype Byte is Interfaces.Unsigned_8;
  type Byte_Buffer is array (Integer range <>) of aliased Byte;
  type p_Byte_Buffer is access Byte_Buffer;

  procedure Block_Read
    (file          : in     Ada.Streams.Stream_IO.File_Type;
     buffer        :    out Byte_Buffer;
     actually_read :    out Natural);
     --  ^ = buffer'Length if no end of file occurred
     --      before last buffer element.

  --  Same for general streams
  --
  procedure Block_Read
    (stream        : in out Zip_Streams.Root_Zipstream_Type'Class;
     buffer        :    out Byte_Buffer;
     actually_read :    out Natural);
     --  ^ = buffer'Length if no end of stream occurred
     --      before last buffer element.

  --  Same, but instead of giving actually_read, raises End_Error if
  --  the buffer cannot be fully read.
  --  This mimics the 'Read stream attribute; can be a lot faster, depending
  --  on the compiler's run-time library.
  procedure Block_Read
    (stream : in out Zip_Streams.Root_Zipstream_Type'Class;
     buffer :    out Byte_Buffer);

  --  This mimics the 'Write stream attribute; can be a lot faster, depending
  --  on the compiler's run-time library.
  --  NB: here we can use the root stream type: no question of size, index,...
  procedure Block_Write
    (stream : in out Ada.Streams.Root_Stream_Type'Class;
     buffer : in     Byte_Buffer);

  --  Copy a chunk from a stream into another one, using a temporary buffer
  procedure Copy_Chunk
    (from        : in out Zip_Streams.Root_Zipstream_Type'Class;
     into        : in out Ada.Streams.Root_Stream_Type'Class;
     bytes       : Natural;
     buffer_size : Positive := 1024 * 1024;
     Feedback    : Feedback_Proc := null);

  --  Copy a whole file into a stream, using a temporary buffer
  procedure Copy_File
    (file_name   : String;
     into        : in out Ada.Streams.Root_Stream_Type'Class;
     buffer_size : Positive := 1024 * 1024);

  --  This does the same as Ada 2005's Ada.Directories.Exists
  --  Just there as helper for Ada 95 only systems
  --
  function Exists (file_name : String) return Boolean;

  --  Write a string containing line endings (possibly from another system)
  --  into a text file, with the "correct", native line endings.
  --  Works for displaying/saving correctly
  --  CR&LF (DOS/Win), LF (UNIX), CR (Mac OS < 9)
  --
  procedure Put_Multi_Line
    (out_file :        Ada.Text_IO.File_Type;
     text     :        String);

  procedure Write_as_Text
    (out_file  :        Ada.Text_IO.File_Type;
     buffer    :        Byte_Buffer;
     last_char : in out Character);  --  track line-ending characters between writes

  function Hexadecimal (x : Interfaces.Unsigned_32) return String;

  -----------------------------------------------------------------
  --  Information about this package - e.g., for an "about" box  --
  -----------------------------------------------------------------

  version   : constant String := "60";
  reference : constant String := "17-Nov-2024";
  --  Hopefully the latest version can be acquired from one of those URLs:
  web       : constant String := "https://unzip-ada.sourceforge.io/";
  web2      : constant String := "https://sourceforge.net/projects/unzip-ada/";
  web3      : constant String := "https://github.com/zertovitch/zip-ada";
  web4      : constant String := "https://alire.ada.dev/crates/zipada";

  ---------------------
  --  Private items  --
  ---------------------

private

  --  Zip_info, 23.VI.1999.
  --
  --  The PKZIP central directory is coded here as a binary tree
  --  to allow a fast retrieval of the searched offset in zip file.
  --  E.g. for a 1000-file archive, the offset will be found in less
  --  than 11 moves: 2**10=1024 (balanced case), without any read
  --  in the archive.
  --
  --  Notes on search dictionary
  ------------------------------
  --  19-Oct-2018: rev. 670 to 683 used a Vector and a Hashed Map
  --      from Ada.Containers. The loading of the dictionary was
  --      much faster (2x), but there were performance bottlenecks elsewhere,
  --      not solved by profiling. On an archive with 18000 small entries of
  --      around 1 KiB each, comp_zip ran 100x slower!
  --      Neither the restricted use of Unbounded_String, nor the replacement
  --      of the Vector by an array helped solving the performance issue.
  --  2022: second attempt with Vectors & Indefinite_Hashed_Maps (both a vector
  --      and a map are needed because a Zip archive may contain entries with
  --      duplicate keys; otherwise a map would be sufficient).
  --         - Test_Zip_Info_Timing: load time on many_65535.zip:
  --              0.75 seconds (binary tree) ->  0.44 seconds (vector & map)
  --         - But... comp_zip many_4096.zip many_4096.zip -q2:
  --              5.5  seconds (binary tree) -> 13.2  seconds (vector & map) !

  type Dir_node;
  type p_Dir_node is access Dir_node;

  type Dir_node (name_len : Natural) is record
    left, right      : p_Dir_node;
    dico_name        : String (1 .. name_len);  --  UPPER if case-insensitive search
    file_name        : String (1 .. name_len);
    file_index       : Zip_Streams.ZS_Index_Type;
    comp_size        : Zip_64_Data_Size_Type;
    uncomp_size      : Zip_64_Data_Size_Type;
    crc_32           : Interfaces.Unsigned_32;
    date_time        : Time;
    method           : PKZip_method;
    name_encoding    : Zip_Name_Encoding;
    read_only        : Boolean;  --  TBD: attributes of most supported systems
    encrypted_2_x    : Boolean;
    user_code        : Integer;
  end record;

  type Zip_archive_format_type is (Zip_32, Zip_64);

  type p_String is access String;

  type Zip_info is new Ada.Finalization.Controlled with record
    loaded             : Boolean := False;
    case_sensitive     : Boolean;
    zip_file_name      : p_String;                            --  a file name...
    zip_input_stream   : Zip_Streams.Zipstream_Class_Access;  --  ...or an input stream
    --  ^ when not null, we use this, and not zip_file_name
    dir_binary_tree    : p_Dir_node;
    total_entries      : Natural;
    zip_file_comment   : p_String;
    zip_archive_format : Zip_archive_format_type := Zip_32;
  end record;

  --  After a copy, need to clone a few things.
  overriding procedure Adjust   (info : in out Zip_info);
  --  Free heap-allocated memory.
  overriding procedure Finalize (info : in out Zip_info);

  --  System.Word_Size: 13.3(8): A word is the largest amount of storage
  --  that can be conveniently and efficiently manipulated by the hardware,
  --  given the implementation's run-time model.
  --
  min_bits_32 : constant := Integer'Max (32, System.Word_Size);
  min_bits_16 : constant := Integer'Max (16, System.Word_Size);

  --  We define an Integer type which is at least 32 bits, but n bits
  --  on a native n (> 32) bits architecture.
  --  Integer_M16 is not needed: Integer already guarantees 16 bits
  --
  type Integer_M32 is range -2**(min_bits_32 - 1) .. 2**(min_bits_32 - 1) - 1;
  subtype Natural_M32  is Integer_M32 range 0 .. Integer_M32'Last;
  subtype Positive_M32 is Integer_M32 range 1 .. Integer_M32'Last;

  type Unsigned_M16 is mod 2**min_bits_16;
  type Unsigned_M32 is mod 2**min_bits_32;

  --  Codes for compression formats in Zip archives
  --  See PKWARE's Appnote, "4.4.5 compression method"
  --
  package Compression_format_code is
    store_code        : constant :=  0;
    shrink_code       : constant :=  1;
    reduce_code       : constant :=  2;
    implode_code      : constant :=  6;
    tokenize_code     : constant :=  7;
    deflate_code      : constant :=  8;
    deflate_e_code    : constant :=  9;
    bzip2_code        : constant := 12;
    lzma_code         : constant := 14;
    zstandard_code    : constant := 93;
    mp3_code          : constant := 94;
    xz_code           : constant := 95;
    jpeg_code         : constant := 96;
    wavpack_code      : constant := 97;
    ppmd_code         : constant := 98;
  end Compression_format_code;

end Zip;
