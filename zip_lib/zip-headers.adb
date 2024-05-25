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

package body Zip.Headers is

  -----------------------------------------------------------
  -- Byte array <-> various integers, with Intel endianess --
  -----------------------------------------------------------

  --  Get numbers with correct trucmuche endian, to ensure
  --  correct header loading on some non-Intel machines

  generic
    type Number is mod <>; -- range <> in Ada83 version (fake Interfaces)
  function Intel_x86_number (b : Byte_Buffer) return Number;

  function Intel_x86_number (b : Byte_Buffer) return Number is
    n : Number := 0;
  begin
    for i in reverse b'Range loop
      n := n * 256 + Number (b (i));
    end loop;
    return n;
  end Intel_x86_number;

  function Intel_nb is new Intel_x86_number (Unsigned_16);
  function Intel_nb is new Intel_x86_number (Unsigned_32);
  function Intel_nb is new Intel_x86_number (Unsigned_64);

  --  Put numbers with correct endianess as bytes

  generic
    type Number is mod <>; -- range <> in Ada83 version (fake Interfaces)
    size : Positive;
  function Intel_x86_buffer (n : Number) return Byte_Buffer;

  function Intel_x86_buffer (n : Number) return Byte_Buffer is
    b : Byte_Buffer (1 .. size);
    m : Number := n;
  begin
    for i in b'Range loop
      b (i) := Unsigned_8 (m and 255);
      m := m / 256;
    end loop;
    return b;
  end Intel_x86_buffer;

  function Intel_bf is new Intel_x86_buffer (Unsigned_16, 2);
  function Intel_bf is new Intel_x86_buffer (Unsigned_32, 4);
  function Intel_bf is new Intel_x86_buffer (Unsigned_64, 8);

  -------------------
  -- PK signatures --
  -------------------

  function PK_signature (buf : Byte_Buffer; code_1, code_2 : Unsigned_8) return Boolean is
  begin
    return buf (buf'First .. buf'First + 3) = (16#50#, 16#4B#, code_1, code_2);
    --  PK12, PK34, ...
  end PK_signature;

  procedure PK_signature (buf : in out Byte_Buffer; code_1, code_2 : Unsigned_8) is
  begin
    buf (1 .. 4) := (16#50#, 16#4B#, code_1, code_2);  --  PK12, PK34, ...
  end PK_signature;

  ------------------------------------------------------------------
  -- PKZIP data descriptor, after streamed compressed data - PK78 --
  ------------------------------------------------------------------

  procedure Copy_and_Check
    (buffer        : in     Byte_Buffer;
     the_data_desc :    out Data_Descriptor)
  is
  begin
    if not PK_signature (buffer, 7, 8) then
      raise bad_data_descriptor;
    end if;

    the_data_desc.crc_32            := Intel_nb (buffer  (5  .. 8));
    the_data_desc.compressed_size   := Intel_nb (buffer  (9  .. 12));
    the_data_desc.uncompressed_size := Intel_nb (buffer (13 .. 16));

  end Copy_and_Check;

  procedure Read_and_Check
    (stream        : in out Root_Zipstream_Type'Class;
     the_data_desc :    out Data_Descriptor)
  is
    ddb : Byte_Buffer (1 .. 16);
  begin
    Block_Read (stream, ddb);
    Copy_and_Check (ddb, the_data_desc);
  end Read_and_Check;

  procedure Write
    (stream        : in out Root_Zipstream_Type'Class;
     the_data_desc : in     Data_Descriptor)
  is
    ddb : Byte_Buffer (1 .. 16);
  begin
    PK_signature (ddb, 7, 8);

    ddb  (5 ..  8) := Intel_bf (the_data_desc.crc_32);
    ddb  (9 .. 12) := Intel_bf (the_data_desc.compressed_size);
    ddb (13 .. 16) := Intel_bf (the_data_desc.uncompressed_size);

    Block_Write (stream, ddb);
  end Write;

  -------------------------------------------------------
  -- PKZIP file header, as in central directory - PK12 --
  -------------------------------------------------------
  procedure Read_and_Check
    (stream : in out Root_Zipstream_Type'Class;
     header :    out Central_File_Header)
  is
    chb : Byte_Buffer (1 .. 46);
  begin
    Block_Read (stream, chb);

    if not PK_signature (chb, 1, 2) then
      raise bad_central_header;
    end if;

    header.made_by_version                   := Intel_nb (chb  (5 ..  6));
    header.short_info.needed_extract_version := Intel_nb (chb  (7 ..  8));
    header.short_info.bit_flag               := Intel_nb (chb  (9 .. 10));
    header.short_info.zip_type               := Intel_nb (chb (11 .. 12));
    header.short_info.file_timedate          :=
     Zip_Streams.Calendar.Convert (Unsigned_32'(Intel_nb (chb (13 .. 16))));
    header.short_info.dd.crc_32              := Intel_nb (chb (17 .. 20));
    header.short_info.dd.compressed_size     := Intel_nb (chb (21 .. 24));
    header.short_info.dd.uncompressed_size   := Intel_nb (chb (25 .. 28));
    header.short_info.filename_length        := Intel_nb (chb (29 .. 30));
    header.short_info.extra_field_length     := Intel_nb (chb (31 .. 32));
    header.comment_length                    := Intel_nb (chb (33 .. 34));
    header.disk_number_start                 := Intel_nb (chb (35 .. 36));
    header.internal_attributes               := Intel_nb (chb (37 .. 38));
    header.external_attributes               := Intel_nb (chb (39 .. 42));
    header.local_header_offset               := Intel_nb (chb (43 .. 46));

  end Read_and_Check;

  procedure Write
    (stream : in out Root_Zipstream_Type'Class;
     header : in     Central_File_Header)
  is
    chb : Byte_Buffer (1 .. 46);
  begin
    PK_signature (chb, 1, 2);

    chb  (5 ..  6) := Intel_bf (header.made_by_version);
    chb  (7 ..  8) := Intel_bf (header.short_info.needed_extract_version);
    chb  (9 .. 10) := Intel_bf (header.short_info.bit_flag);
    chb (11 .. 12) := Intel_bf (header.short_info.zip_type);
    chb (13 .. 16) := Intel_bf (Zip_Streams.Calendar.Convert (
                                    header.short_info.file_timedate)
                               );
    chb (17 .. 20) := Intel_bf (header.short_info.dd.crc_32);
    chb (21 .. 24) := Intel_bf (Unsigned_32 (header.short_info.dd.compressed_size));
    chb (25 .. 28) := Intel_bf (Unsigned_32 (header.short_info.dd.uncompressed_size));
    chb (29 .. 30) := Intel_bf (header.short_info.filename_length);
    chb (31 .. 32) := Intel_bf (header.short_info.extra_field_length);
    chb (33 .. 34) := Intel_bf (header.comment_length);
    chb (35 .. 36) := Intel_bf (header.disk_number_start);
    chb (37 .. 38) := Intel_bf (header.internal_attributes);
    chb (39 .. 42) := Intel_bf (header.external_attributes);
    chb (43 .. 46) := Intel_bf (Unsigned_32 (header.local_header_offset));

    Block_Write (stream, chb);
  end Write;

  function Needs_Local_Zip_64_Header_Extension
    (header : Local_File_Header;
     offset : Unsigned_64)  --  Not part of the Zip32 header but of the Zip64 one...
  return Boolean
  is
    do_we_want_always_zip64 : constant Boolean := False;
    --  ^ True:  Force Zip64 on all entries & central directory, for test purposes
    --    False: Zip64 is used only when needed
  begin
    return do_we_want_always_zip64 or
           header.dd.compressed_size   >= 16#FFFF_FFFF# or
           header.dd.uncompressed_size >= 16#FFFF_FFFF# or
           offset                      >= 16#FFFF_FFFF#;
  end Needs_Local_Zip_64_Header_Extension;

  -----------------------------------------------------------------------
  -- PKZIP local file header, in front of every file in archive - PK34 --
  -----------------------------------------------------------------------
  procedure Read_and_Check
    (stream : in out Root_Zipstream_Type'Class;
     header :    out Local_File_Header)
  is
    lhb : Byte_Buffer (1 .. local_header_length);
    u32 : Unsigned_32;
  begin
    Block_Read (stream, lhb);

    if not PK_signature (lhb, 3, 4) then
      raise bad_local_header;
    end if;

    header.needed_extract_version := Intel_nb (lhb  (5 ..  6));
    header.bit_flag               := Intel_nb (lhb  (7 ..  8));
    header.zip_type               := Intel_nb (lhb  (9 .. 10));
    header.file_timedate          := Zip_Streams.Calendar.Convert (Unsigned_32'(
                                     Intel_nb (lhb (11 .. 14))
                                   ));
    header.dd.crc_32              := Intel_nb (lhb (15 .. 18));
    u32                           := Intel_nb (lhb (19 .. 22));
    header.dd.compressed_size     := Unsigned_64 (u32);
    u32                           := Intel_nb (lhb (23 .. 26));
    header.dd.uncompressed_size   := Unsigned_64 (u32);
    header.filename_length        := Intel_nb (lhb (27 .. 28));
    header.extra_field_length     := Intel_nb (lhb (29 .. 30));

  end Read_and_Check;

  procedure Write
    (stream             : in out Root_Zipstream_Type'Class;
     header             : in     Local_File_Header;
     extra_field_policy : in     Extra_Field_Policy_Kind)
  is
    lhb : Byte_Buffer (1 .. local_header_length);
    extra_length : Unsigned_16;
  begin
    PK_signature (lhb, 3, 4);

    lhb  (5 ..  6) := Intel_bf (header.needed_extract_version);
    lhb  (7 ..  8) := Intel_bf (header.bit_flag);
    lhb  (9 .. 10) := Intel_bf (header.zip_type);
    lhb (11 .. 14) := Intel_bf (Zip_Streams.Calendar.Convert (header.file_timedate));
    lhb (15 .. 18) := Intel_bf (header.dd.crc_32);
    if extra_field_policy = force_zip_64 then
      lhb (19 .. 22) := (255, 255, 255, 255);
      lhb (23 .. 26) := (255, 255, 255, 255);
    else
      lhb (19 .. 22) := Intel_bf (Unsigned_32 (header.dd.compressed_size));
      lhb (23 .. 26) := Intel_bf (Unsigned_32 (header.dd.uncompressed_size));
    end if;
    lhb (27 .. 28) := Intel_bf (header.filename_length);

    case extra_field_policy is
      when from_header  => extra_length := header.extra_field_length;
      when force_empty  => extra_length := 0;
      when force_zip_64 => extra_length := local_header_extension_short_length;
    end case;
    lhb (29 .. 30) := Intel_bf (extra_length);

    Block_Write (stream, lhb);
  end Write;

  procedure Read_and_Check
    (stream : in out Root_Zipstream_Type'Class;
     header :    out Local_File_Header_Extension)
  is
    lhb_1 : Byte_Buffer (1 .. 4);
    lhb_2 : Byte_Buffer (5 .. local_header_extension_length);
  begin
    Block_Read (stream, lhb_1);

    header.tag               := Intel_nb (lhb_1  (1 ..  2));
    header.size              := Intel_nb (lhb_1  (3 ..  4));
    if header.tag /= local_header_extension_tag then
      return;  --  It's another kind of extra field.
    end if;
    if header.size < 8 then
      raise bad_local_header with "Zip64 extra field bytes < 8";
    end if;

    Block_Read (stream, lhb_2);
    header.value_64 (1) := Intel_nb (lhb_2  (5 .. 12));
    header.value_64 (2) := Intel_nb (lhb_2 (13 .. 20));
    header.value_64 (3) := Intel_nb (lhb_2 (21 .. 28));

  end Read_and_Check;

  procedure Interpret
    (header            : in     Local_File_Header_Extension;
     uncompressed_size : in out Unsigned_64;
     compressed_size   : in out Unsigned_64;
     offset            : in out Unsigned_64)
  is
    counter : Natural := 0;
  begin
    if header.tag /= local_header_extension_tag then
      return;  --  It's another kind of extra field.
    end if;
    --
    --  All fields are optional - an unusual feature...
    --
    if uncompressed_size = 16#FFFF_FFFF# then
      counter := counter + 1;
      uncompressed_size := header.value_64 (counter);
    end if;
    if compressed_size = 16#FFFF_FFFF# then
      counter := counter + 1;
      compressed_size := header.value_64 (counter);
    end if;
    if offset = 16#FFFF_FFFF# then
      counter := counter + 1;
      offset := header.value_64 (counter);
    end if;
    if counter * 8 > Natural (header.size) then
      raise bad_local_header
        with "Zip64 extra field bytes: invalid size:" & header.size'Image &
             ", needed:" & Integer'Image (counter * 8);
    end if;
  end Interpret;

  procedure Write
    (stream : in out Root_Zipstream_Type'Class;
     header : in     Local_File_Header_Extension;
     short  : in     Boolean)
  is
    lhb : Byte_Buffer (1 .. local_header_extension_length);
  begin
    lhb  (1 ..  2) := Intel_bf (header.tag);
    lhb  (3 ..  4) := Intel_bf (header.size);
    lhb  (5 .. 12) := Intel_bf (header.value_64 (1));
    lhb (13 .. 20) := Intel_bf (header.value_64 (2));
    lhb (21 .. 28) := Intel_bf (header.value_64 (3));
    if short then
      pragma Assert (header.size = local_header_extension_short_length - 4);
      Block_Write (stream, lhb (1 .. local_header_extension_short_length));
    else
      pragma Assert (header.size = local_header_extension_length - 4);
      Block_Write (stream, lhb);
    end if;
  end Write;

  -------------------------------------------
  -- PKZIP end-of-central-directory - PK56 --
  -------------------------------------------
  procedure Copy_and_Check
    (buffer  : in     Byte_Buffer;
     the_end :    out End_of_Central_Dir)
  is
    o : constant Integer := buffer'First - 1;
  begin
    if not PK_signature (buffer, 5, 6) then
      raise bad_end;
    end if;

    the_end.disknum             := Intel_nb (buffer (o +  5 .. o +  6));
    the_end.disknum_with_start  := Intel_nb (buffer (o +  7 .. o +  8));
    the_end.disk_total_entries  := Intel_nb (buffer (o +  9 .. o + 10));
    the_end.total_entries       := Intel_nb (buffer (o + 11 .. o + 12));
    the_end.central_dir_size    := Intel_nb (buffer (o + 13 .. o + 16));
    the_end.central_dir_offset  := Intel_nb (buffer (o + 17 .. o + 20));
    the_end.main_comment_length := Intel_nb (buffer (o + 21 .. o + 22));

  end Copy_and_Check;

  procedure Read_and_Check
    (stream  : in out Root_Zipstream_Type'Class;
     the_end :    out End_of_Central_Dir)
  is
    eb : Byte_Buffer (1 .. 22);
  begin
    Block_Read (stream, eb);
    Copy_and_Check (eb, the_end);
  end Read_and_Check;

  procedure Load
    (stream  : in out Root_Zipstream_Type'Class;
     the_end :    out End_of_Central_Dir)
  is
    min_end_start : ZS_Index_Type;  --  min_end_start >= 1
    max_comment : constant := 65_535;
    --  In appnote.txt :
    --  .ZIP file comment length        2 bytes
    end_64_loc : Zip64_End_of_Central_Dir_Locator;
    end_64     : Zip64_End_of_Central_Dir;
    found : Boolean;
    function Is_64 return Boolean is
    begin
      return the_end.total_entries = 16#FFFF# or
             the_end.central_dir_size = 16#FFFF_FFFF# or
             the_end.central_dir_offset = 16#FFFF_FFFF#;
    end Is_64;
  begin
    if Size (stream) < 22 then
      raise bad_end;
    end if;
    --  20-Jun-2001: abandon search below min_end_start.
    if Size (stream) <= max_comment then
      min_end_start := 1;
    else
      min_end_start := Size (stream) - max_comment;
    end if;
    Set_Index (stream, min_end_start);
    declare
      --  We copy a large chunk of the zip stream's tail into a buffer.
      large_buffer : Byte_Buffer (0 .. Natural (Size (stream) - min_end_start));
      ilb : Integer;
      x : ZS_Size_Type;
    begin
      found := False;
      Block_Read (stream, large_buffer);
      for i in reverse min_end_start .. Size (stream) - 21 loop
        --  Yes, we must _search_ for the header...
        --  because PKWARE put a variable-size comment _after_ it 8-(
        ilb := Integer (i - min_end_start);
        if PK_signature (large_buffer (ilb .. ilb + 3), 5, 6) then
          Copy_and_Check (large_buffer (ilb .. ilb + 21), the_end);
          if Is_64 then
            found := True;
            exit;
          end if;
          --  At this point, the buffer was successfully read, the_end is
          --  is set with its standard contents.
          --
          --  This is the *real* position of the end-of-central-directory block, to begin with:
          x := i;
          --  We subtract the *theoretical* (stored) position of the end-of-central-directory.
          --  The theoretical position is equal to central_dir_offset + central_dir_size.
          --  The theoretical position should be smaller or equal than the real position -
          --  unless the archive is corrupted.
          --  We do it step by step, because ZS_Size_Type was modular until rev. 644.
          --  Now it's a signed 64 bits, but we don't want to change anything again...
          --
          x := x - 1;  --  i >= 1, so no dragons here. The "- 1" is for adapting from the 1-based Ada index.
          exit when ZS_Size_Type (the_end.central_dir_offset) > x;  --  fuzzy value, will trigger bad_end
          x := x - ZS_Size_Type (the_end.central_dir_offset);
          exit when ZS_Size_Type (the_end.central_dir_size) > x;  --  fuzzy value, will trigger bad_end
          x := x - ZS_Size_Type (the_end.central_dir_size);
          --  Now, x is the difference : real - theoretical.
          --    x > 0  if the archive was appended to another file (typically an executable
          --           for self-extraction purposes).
          --    x = 0  if this is a "pure" Zip archive.
          the_end.offset_shifting := x;
          Set_Index (stream, i + 22);
          found := True;
          exit;
        end if;
      end loop;
      if not found then
        raise bad_end; -- Definitely no "end-of-central-directory" in this stream
      end if;
    end;
    --
    --  Zip64
    --
    if Is_64 then
      Set_Index (stream, Index (stream) - 42);
      Read_and_Check (stream, end_64_loc);
      --  We zero the offset shifting. This assumes that the offsets are as
      --  written, i.e. the Zip file is not appended to another file.
      --  The reason is that the "Zip64 end of central directory" has
      --  an 64-bit unknown size and thus can only reached by the stored offset,
      --  not via a calculation using the stream index, or via heurisitcs as above
      --  for the "Zip32".
      the_end.offset_shifting := 0;
      Set_Index (stream,
        ZS_Index_Type
          (end_64_loc.relative_offset_of_the_zip64_end_of_central_dir_record) +
          the_end.offset_shifting + 1);
      Read_and_Check (stream, end_64);
      the_end.disknum            := end_64.number_of_this_disk;
      the_end.disknum_with_start := end_64.number_of_the_disk_with_the_start_of_the_central_directory;
      the_end.disk_total_entries := end_64.total_number_of_entries_in_the_central_directory_on_this_disk;
      the_end.total_entries      := end_64.total_number_of_entries_in_the_central_directory;
      the_end.central_dir_size   := end_64.size_of_the_central_directory;
      the_end.central_dir_offset := end_64.offset_of_start_of_central_directory;
    end if;
  end Load;

  procedure Write
    (stream  : in out Root_Zipstream_Type'Class;
     the_end : in     End_of_Central_Dir)
  is
    eb : Byte_Buffer (1 .. 22);
  begin
    PK_signature (eb, 5, 6);

    eb  (5 ..  6) := Intel_bf (Unsigned_16 (the_end.disknum));
    eb  (7 ..  8) := Intel_bf (Unsigned_16 (the_end.disknum_with_start));
    eb  (9 .. 10) := Intel_bf (Unsigned_16 (the_end.disk_total_entries));
    eb (11 .. 12) := Intel_bf (Unsigned_16 (the_end.total_entries));
    eb (13 .. 16) := Intel_bf (Unsigned_32 (the_end.central_dir_size));
    eb (17 .. 20) := Intel_bf (Unsigned_32 (the_end.central_dir_offset));
    eb (21 .. 22) := Intel_bf (the_end.main_comment_length);

    Block_Write (stream, eb);
  end Write;

  procedure Read_and_Check
    (stream     : in out Root_Zipstream_Type'Class;
     the_end_64 :    out Zip64_End_of_Central_Dir)
  is
    eb : Byte_Buffer (1 .. zip_64_end_of_central_dir_length);
  begin
    Block_Read (stream, eb);
    if not PK_signature (eb, 6, 6) then
      raise bad_end with "Zip64_End_of_Central_Dir";
    end if;
    the_end_64.size                                                          := Intel_nb (eb   (5 .. 12));
    the_end_64.version_made_by                                               := Intel_nb (eb  (13 .. 14));
    the_end_64.version_needed_to_extract                                     := Intel_nb (eb  (15 .. 16));
    the_end_64.number_of_this_disk                                           := Intel_nb (eb  (17 .. 20));
    the_end_64.number_of_the_disk_with_the_start_of_the_central_directory    := Intel_nb (eb  (21 .. 24));
    the_end_64.total_number_of_entries_in_the_central_directory_on_this_disk := Intel_nb (eb  (25 .. 32));
    the_end_64.total_number_of_entries_in_the_central_directory              := Intel_nb (eb  (33 .. 40));
    the_end_64.size_of_the_central_directory                                 := Intel_nb (eb  (41 .. 48));
    the_end_64.offset_of_start_of_central_directory                          := Intel_nb (eb  (49 .. 56));
  end Read_and_Check;

  procedure Write
    (stream     : in out Root_Zipstream_Type'Class;
     the_end_64 : in     Zip64_End_of_Central_Dir)
  is
    eb : Byte_Buffer (1 .. zip_64_end_of_central_dir_length);
  begin
    PK_signature (eb, 6, 6);
    eb   (5 .. 12) := Intel_bf (the_end_64.size);
    eb  (13 .. 14) := Intel_bf (the_end_64.version_made_by);
    eb  (15 .. 16) := Intel_bf (the_end_64.version_needed_to_extract);
    eb  (17 .. 20) := Intel_bf (the_end_64.number_of_this_disk);
    eb  (21 .. 24) := Intel_bf (the_end_64.number_of_the_disk_with_the_start_of_the_central_directory);
    eb  (25 .. 32) := Intel_bf (the_end_64.total_number_of_entries_in_the_central_directory_on_this_disk);
    eb  (33 .. 40) := Intel_bf (the_end_64.total_number_of_entries_in_the_central_directory);
    eb  (41 .. 48) := Intel_bf (the_end_64.size_of_the_central_directory);
    eb  (49 .. 56) := Intel_bf (the_end_64.offset_of_start_of_central_directory);
    Block_Write (stream, eb);
  end Write;

  procedure Read_and_Check
    (stream         : in out Root_Zipstream_Type'Class;
     the_end_64_loc :    out Zip64_End_of_Central_Dir_Locator)
  is
    eb : Byte_Buffer (1 .. zip_64_end_of_central_dir_locator_length);
  begin
    Block_Read (stream, eb);
    if not PK_signature (eb, 6, 7) then
      raise bad_end with "Zip64_End_of_Central_Dir_Locator";
    end if;
    the_end_64_loc.number_of_the_disk_with_the_start_of_the_zip64_end_of_central_dir := Intel_nb (eb   (5 ..  8));
    the_end_64_loc.relative_offset_of_the_zip64_end_of_central_dir_record            := Intel_nb (eb   (9 .. 16));
    the_end_64_loc.total_number_of_disks                                             := Intel_nb (eb  (17 .. 20));
  end Read_and_Check;

  procedure Write
    (stream         : in out Root_Zipstream_Type'Class;
     the_end_64_loc : in     Zip64_End_of_Central_Dir_Locator)
  is
    eb : Byte_Buffer (1 .. zip_64_end_of_central_dir_locator_length);
  begin
    PK_signature (eb, 6, 7);
    eb   (5 ..  8) := Intel_bf (the_end_64_loc.number_of_the_disk_with_the_start_of_the_zip64_end_of_central_dir);
    eb   (9 .. 16) := Intel_bf (the_end_64_loc.relative_offset_of_the_zip64_end_of_central_dir_record);
    eb  (17 .. 20) := Intel_bf (the_end_64_loc.total_number_of_disks);
    Block_Write (stream, eb);
  end Write;

end Zip.Headers;
