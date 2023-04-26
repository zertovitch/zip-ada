--   ________  ___   ______       ______      ___
--  /___..._/  |.|   |.___.\     /. __ .\   __|.|   ____
--     /../    |.|   |.____/     |.|__|.|  /....|  __\..\
--   _/../___  |.|   |.|    ===  |..__..| |. = .| | = ..|
--  /_______/  |_|  /__|        /__|  |_|  \__\_|  \__\_|

--  Zip.Headers
---------------
--
--  This package provides:
--
--  * Definiton of PKZIP information structures (cf appnote.txt),
--  * Reading a header from a data stream (Read_and_check),
--  * Copying a header from a buffer (Copy_and_check)
--  * Writing a header to a data stream (Write)

--  Legal licensing note:

--  Copyright (c) 2000 .. 2022 Gautier de Montmollin
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

--  NB: this is the MIT License, as found on the site
--  http://www.opensource.org/licenses/mit-license.php

  --  Some quick explanations about the Zip file structure - GdM 2001, 2012
  --
  --  The zip archive containing N entries can be roughly seen as
  --  a data stream with the following structure:
  --
  --  1) {local header, then compressed data} - that, N times
  --  2) central directory, with a summary of each of the N entries
  --  3) end-of-central-directory, with a summary of the central directory
  --
  --  Since N is not necessarily known before or during the phase 1,
  --  the central directory's size is also potentially unknown.
  --  Then obvious place for the central directory is *after* the data,
  --  it is why it appears on phase 2.
  --
  --  An advantage of that structure is that the .ZIP archive can be later
  --  appended to an .EXE, for self-extracting purposes, or to other
  --  kind of files.
  --
  --  So, the most general infos are at the end, and we crawl back
  --  for more precise infos:
  --
  --  1) end-of-central-directory
  --  2) central directory
  --  3) zipped data entries

--  Change log:
--  ==========
--
--  29-May-2022: GdM: Support for Zip64 extensions.
--
--  22-Nov-2012: GdM: End-of-central-directory loaded in a single stream Read
--                      operation instead of up to ~1.4 million Read
--                      operations (for a non Zip file with 65535 times
--                      the letter 'P'). Execution flow simplified, without
--                      use of exceptions. Massive speedup there, on files
--                      that are either invalid Zip files, or Zip files with
--                      a large comment.
--
--  30-Oct-2012: GdM: Removed all profiles using Zip_Streams' objects
--                      with accesses (cf 25-Oct's modifications)
--  25-Oct-2012: GdM: Some procedures using Zip_Streams' objects also with
--                    pointer-free profiles (no more 'access' or access type)
--  16-Nov-2009: GdM: Replaced Ada.Calendar.Time by Zip.Time in headers, due to
--                   perf. issues in some run-times' Ada.Calendar.Time_Of

package Zip.Headers is

  use Interfaces, Zip_Streams;

  ----------------------------------------------------------------------
  -- PKZIP data descriptor, put after streamed compressed data - PK78 --
  ----------------------------------------------------------------------

  type Data_descriptor is record
    --  PK78                           --  1 .. 4
    crc_32             : Unsigned_32;  --  5 .. 8
    compressed_size,
    uncompressed_size  : Unsigned_64;
    --  ^ Stricto sensu, the data descriptor is 32 bit, but the actual
    --    size may be larger (complemented through a Zip64 extra field).
  end record;

  data_descriptor_length : constant := 16;

  --  This header needs to be read in continuation of
  --  the compressed data -> access to a buffer
  procedure Copy_and_check (
    buffer        : in     Byte_Buffer;
    the_data_desc :    out Data_descriptor
  );

  procedure Read_and_check (
    stream        : in out Root_Zipstream_Type'Class;
    the_data_desc :    out Data_descriptor
  );

  bad_data_descriptor : exception;

  procedure Write (
    stream        : in out Root_Zipstream_Type'Class;
    the_data_desc : in     Data_descriptor
  );

  -----------------------------------------------------------------------
  -- PKZIP local file header, in front of every file in archive - PK34 --
  -----------------------------------------------------------------------

  --  Appnote: 4.4.4 general purpose bit flag: (2 bytes)
  --
  --  Bit 0:  If set, indicates that the file is encrypted.
  Encryption_Flag_Bit        : constant := 2 **  0;
  --  Bit 1:  If set, indicates an EOS marker is used.
  LZMA_EOS_Flag_Bit          : constant := 2 **  1;
  --  Bit 11: Language encoding flag (EFS). If this bit is set, the filename and
  --          comment fields for this file MUST be encoded using UTF-8.
  Language_Encoding_Flag_Bit : constant := 2 ** 11;

  type Local_File_Header is record
    --  PK34                                --  1 .. 4
    needed_extract_version : Unsigned_16;   --  5 .. 6
    bit_flag               : Unsigned_16;   --  Appnote: 4.4.4 general purpose bit flag
    zip_type               : Unsigned_16;
    file_timedate          : Time;
    dd                     : Data_descriptor;
    filename_length,
    extra_field_length     : Unsigned_16;
  end record;

  local_header_length : constant := 30;

  procedure Read_and_check (
    stream : in out Root_Zipstream_Type'Class;
    header :    out Local_File_Header
  );

  bad_local_header : exception;

  type Extra_Field_Policy_Kind is
    (from_header,  --  This policy is for preserving an entry from another Zip
                   --  file. The extra field could be a Zip_64 or another kind.
     force_empty,
     force_zip_64);

  procedure Write (
    stream             : in out Root_Zipstream_Type'Class;
    header             : in     Local_File_Header;
    extra_field_policy : in     Extra_Field_Policy_Kind
  );

  ---------------------------------------------
  -- PKZIP local file header Zip64 extension --
  ---------------------------------------------

  --  4.5.3 Zip64 Extended Information Extra Field

  type Values_64 is array (1 .. 3) of Unsigned_64;

  type Local_File_Header_Extension is record
    tag      : Unsigned_16;
    size     : Unsigned_16;
    value_64 : Values_64;
  end record;

  local_header_extension_length : constant := 28;

  --  Shorter length (witout the offset field) for the local header
  --  extension is not properly documented in appnote.txt but is
  --  required by WinZip and 7z (otherwise you get: WARNINGS: Headers Error).
  --
  --  Fortunately someone published a nice blog post with
  --  a Zip64 example, as simple as possible...
  --  https://blog.yaakov.online/zip64-go-big-or-go-home/
  --
  local_header_extension_short_length : constant := 20;

  local_header_extension_tag : constant := 1;

  procedure Read_and_check (
    stream : in out Root_Zipstream_Type'Class;
    header :    out Local_File_Header_Extension
  );

  --  Depending on its size (1, 2, >=3 values) we may
  --  need (or not) to change the corresponding variables.
  --  E.g. 7z may set only the first item, which is correct
  --  but is contrary to the requirement of appnote.txt (4.5.3)
  --  to have at least the first two items.
  --
  procedure Interpret
    (header            : in     Local_File_Header_Extension;
     uncompressed_size : in out Unsigned_64;
     compressed_size   : in out Unsigned_64;
     offset            : in out Unsigned_64);

  procedure Write (
    stream : in out Root_Zipstream_Type'Class;
    header : in     Local_File_Header_Extension;
    short  : in     Boolean
  );

  ---------------------------------------------------------
  --  PKZIP file header, as in central directory - PK12  --
  ---------------------------------------------------------
  --  NB: a central header contains a local header in the middle

  type Central_File_Header is record
    made_by_version     : Unsigned_16;
    short_info          : Local_File_Header;
    comment_length      : Unsigned_16;
    disk_number_start   : Unsigned_16;
    internal_attributes : Unsigned_16;  --  internal properties of data
    external_attributes : Unsigned_32;  --  1st byte if MS-DOS: see below
    local_header_offset : Unsigned_64;
  end record;

  --  MS-DOS external attributes:
  --
  --   Bit 0     Read-Only
  --   Bit 1     Hidden
  --   Bit 2     System
  --   Bit 3     Volume Label
  --   Bit 4     Directory
  --   Bit 5     Archive

  central_header_length : constant := 46;

  procedure Read_and_check (
    stream : in out Root_Zipstream_Type'Class;
    header :    out Central_File_Header
  );

  bad_central_header : exception;

  procedure Write (
    stream : in out Root_Zipstream_Type'Class;
    header : in     Central_File_Header
  );

  function Needs_Local_Zip_64_Header_Extension
    (header : Local_File_Header;
     offset : Unsigned_64  --  Not part of the Zip32 header but of the Zip64 one...
    )
  return Boolean;

  -------------------------------------------
  -- PKZIP end-of-central-directory - PK56 --
  -------------------------------------------

  type End_of_Central_Dir is record
    disknum             : Unsigned_32;
    disknum_with_start  : Unsigned_32;
    disk_total_entries  : Unsigned_64;
    total_entries       : Unsigned_64;
    central_dir_size    : Unsigned_64;
    central_dir_offset  : Unsigned_64;
    main_comment_length : Unsigned_16;
    --  The Zip archive may be appended to another file (for instance an
    --  executable for self-extracting purposes) of size N.
    --  Then, all offsets need to be shifted by N.
    --  N=0 if the Zip archive is on its own.
    --  The real offset of the end-of-central-dir
    --  will be N + central_dir_size + central_dir_offset.
    --  This way, we have an unique chance to determine N when reading the
    --  end-of-central-dir. N is stored in the field hereafter:
    offset_shifting    : ZS_Size_Type;  --  NB: type is at least 32 bits.
  end record;

  end_of_central_dir_length : constant := 22;

  --  The End-of-Central-Dir header is followed by a comment of
  --  unkown size and hence needs to be searched in special ways (see Load).

  --  Copy_and_check and Read_and_check assume a buffer or a stream
  --  pointing to the End-of-Central-Dir signature.
  procedure Copy_and_check (
    buffer  : in     Byte_Buffer;
    the_end :    out End_of_Central_Dir
  );

  procedure Read_and_check (
    stream  : in out Root_Zipstream_Type'Class;
    the_end :    out End_of_Central_Dir
  );

  bad_end : exception;

  procedure Write (
    stream  : in out Root_Zipstream_Type'Class;
    the_end : in     End_of_Central_Dir
  );

  --  A bit more elaborated variant of Read:
  --  find the End-of-Central-Dir and load it.
  --  It includes the processing of an eventual Zip64
  --  End-of-Central-Dir.

  procedure Load (
    stream  : in out Root_Zipstream_Type'Class;
    the_end :    out End_of_Central_Dir
  );

  ------------------------------------------------------------
  --  Zip64 extensions for end-of-central directory stuff.  --
  ------------------------------------------------------------

  --  References are from PKWare's appnote.txt.

  --  The Zip64 flavor of the end-of-central directory structure appears in
  --  the following order. The three records are read in reverse order.
  --

  --  4.3.14   Zip64 end of central directory record (variable size)
  --  4.3.15   Zip64 end of central directory locator (fixed size)
  --  4.3.16   End of central directory record (variable size), disabled with FFFF's

  type Zip64_End_of_Central_Dir is record
    size                                                          : Unsigned_64;  --  Size of the remaining record
    version_made_by                                               : Unsigned_16;
    version_needed_to_extract                                     : Unsigned_16;
    number_of_this_disk                                           : Unsigned_32;
    number_of_the_disk_with_the_start_of_the_central_directory    : Unsigned_32;
    total_number_of_entries_in_the_central_directory_on_this_disk : Unsigned_64;
    total_number_of_entries_in_the_central_directory              : Unsigned_64;
    size_of_the_central_directory                                 : Unsigned_64;
    offset_of_start_of_central_directory                          : Unsigned_64;
    --  ^ offset: with respect to the starting disk number
    --  zip64_extensible_data_sector (variable_size)
  end record;

  zip_64_end_of_central_dir_length : constant := 56;

  procedure Read_and_check (
    stream     : in out Root_Zipstream_Type'Class;
    the_end_64 :    out Zip64_End_of_Central_Dir
  );

  procedure Write (
    stream     : in out Root_Zipstream_Type'Class;
    the_end_64 : in     Zip64_End_of_Central_Dir
  );

  type Zip64_End_of_Central_Dir_Locator is record
    number_of_the_disk_with_the_start_of_the_zip64_end_of_central_dir : Unsigned_32;
    relative_offset_of_the_zip64_end_of_central_dir_record            : Unsigned_64;
    total_number_of_disks                                             : Unsigned_32;
  end record;

  zip_64_end_of_central_dir_locator_length : constant := 20;

  procedure Read_and_check (
    stream         : in out Root_Zipstream_Type'Class;
    the_end_64_loc :    out Zip64_End_of_Central_Dir_Locator
  );

  procedure Write (
    stream         : in out Root_Zipstream_Type'Class;
    the_end_64_loc : in     Zip64_End_of_Central_Dir_Locator
  );

end Zip.Headers;
