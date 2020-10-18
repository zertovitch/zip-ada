--  Legal licensing note:

--  Copyright (c) 1999 .. 2020 Gautier de Montmollin
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

with Zip.Headers, UnZip.Decompress;

with Ada.Strings.Unbounded;
with Ada.Unchecked_Deallocation;
with Interfaces;                        use Interfaces;

package body UnZip.Streams is

   procedure Dispose is new
     Ada.Unchecked_Deallocation (String, p_String);

   procedure Dispose is new
     Ada.Unchecked_Deallocation (Ada.Streams.Stream_Element_Array,
                                 p_Stream_Element_Array);

   procedure Dispose is new
     Ada.Unchecked_Deallocation (UnZip_Stream_Type,
                                 Zipped_File_Type);

  --------------------------------------------------
  -- *The* internal 1-file unzipping procedure.   --
  -- Input must be _open_ and won't be _closed_ ! --
  --------------------------------------------------

  procedure UnZipFile (
    zip_stream      : in out Zip_Streams.Root_Zipstream_Type'Class;
    header_index    : in out Zip_Streams.ZS_Index_Type;
    mem_ptr         :    out p_Stream_Element_Array;
    out_stream_ptr  :        p_Stream;
    --  if not null, extract to out_stream_ptr, not to memory
    password        : in out Ada.Strings.Unbounded.Unbounded_String;
    hint_comp_size  : in     Zip.Zip_32_Data_Size_Type; -- Added 2007 for .ODS files
    hint_crc_32     : in     Unsigned_32;    -- Added 2012 for decryption
    cat_uncomp_size : in     Zip.Zip_32_Data_Size_Type
  )
  is
    work_index : Zip_Streams.ZS_Index_Type := header_index;
    local_header : Zip.Headers.Local_File_Header;
    data_descriptor_after_data : Boolean;
    encrypted : Boolean;
    method : Zip.PKZip_method;
    use Zip;
    mode : Write_mode;
  begin
    begin
      Zip_Streams.Set_Index (zip_stream, header_index);
      Zip.Headers.Read_and_check (zip_stream, local_header);
    exception
      when Zip.Headers.bad_local_header =>
        raise Zip.Archive_corrupted with "Bad local header";
      when others =>
        raise Zip.Archive_corrupted;
    end;

    method := Method_from_code (local_header.zip_type);
    if method = unknown then
      raise Unsupported_method;
    end if;

    --  calculate offset of data

    work_index :=
      work_index +
      Zip_Streams.ZS_Size_Type (
              local_header.filename_length    +
              local_header.extra_field_length +
              Zip.Headers.local_header_length
      );

    data_descriptor_after_data := (local_header.bit_flag and 8) /= 0;

    if data_descriptor_after_data then
      --  Sizes and crc are after the data
      local_header.dd.crc_32            := hint_crc_32;
      local_header.dd.uncompressed_size := cat_uncomp_size;
      local_header.dd.compressed_size   := hint_comp_size;
    else
      --  Sizes and crc are before the data
      if cat_uncomp_size /= local_header.dd.uncompressed_size then
        raise Uncompressed_size_Error;
      end if;
    end if;

    encrypted := (local_header.bit_flag and Zip.Headers.Encryption_Flag_Bit) /= 0;

    begin
      Zip_Streams.Set_Index (zip_stream, work_index);  --  eventually skips the file name
    exception
      when others =>
        raise Zip.Archive_corrupted with
          "End of stream reached (location: between local header and archived data)";
    end;

    if out_stream_ptr = null then
      mode := write_to_memory;
    else
      mode := write_to_stream;
    end if;
    --  Unzip correct type
    UnZip.Decompress.Decompress_data (
      zip_file                   => zip_stream,
      format                     => method,
      mode                       => mode,
      output_file_name           => "",
      output_memory_access       => mem_ptr,
      output_stream_access       => out_stream_ptr,
      feedback                   => null,
      explode_literal_tree       => (local_header.bit_flag and 4) /= 0,
      explode_slide_8KB_LZMA_EOS => (local_header.bit_flag and Zip.Headers.LZMA_EOS_Flag_Bit) /= 0,
      data_descriptor_after_data => data_descriptor_after_data,
      is_encrypted               => encrypted,
      password                   => password,
      get_new_password           => null,
      hint                       => local_header
    );

    --  Set the offset on the next zipped file
    header_index := header_index +
      Zip_Streams.ZS_Size_Type (
              local_header.filename_length    +
              local_header.extra_field_length +
              Zip.Headers.local_header_length
      ) +
      Zip_Streams.ZS_Size_Type (
        local_header.dd.compressed_size
      );

    if data_descriptor_after_data then
      header_index := header_index +
        Zip_Streams.ZS_Size_Type (Zip.Headers.data_descriptor_length);
    end if;

  end UnZipFile;

  procedure S_Extract (from             : Zip.Zip_info;
                       zip_stream       : in out Zip_Streams.Root_Zipstream_Type'Class;
                       what             : String;
                       password         : in String;
                       mem_ptr          : out p_Stream_Element_Array;
                       out_stream_ptr   : p_Stream;
                       Ignore_Directory : in Boolean
                      )
  is
    header_index : Zip_Streams.ZS_Index_Type;
    comp_size    : Zip.Zip_32_Data_Size_Type;
    uncomp_size  : Zip.Zip_32_Data_Size_Type;
    crc_32 : Interfaces.Unsigned_32;
    work_password : Ada.Strings.Unbounded.Unbounded_String :=
      Ada.Strings.Unbounded.To_Unbounded_String (password);
    dummy_name_encoding : Zip.Zip_name_encoding;

  begin
    if Ignore_Directory then
      Zip.Find_offset_without_directory (
        info          => from,
        name          => what,
        name_encoding => dummy_name_encoding,
        file_index    => header_index,
        comp_size     => comp_size,
        uncomp_size   => uncomp_size,
        crc_32        => crc_32
      );
    else
      Zip.Find_offset (
        info          => from,
        name          => what,
        name_encoding => dummy_name_encoding,
        file_index    => header_index,
        comp_size     => comp_size,
        uncomp_size   => uncomp_size,
        crc_32        => crc_32
      );
    end if;
    UnZipFile (
      zip_stream      => zip_stream,
      header_index    => header_index,
      mem_ptr         => mem_ptr,
      out_stream_ptr  => out_stream_ptr,
      password        => work_password,
      hint_comp_size  => comp_size,
      hint_crc_32     => crc_32,
      cat_uncomp_size => uncomp_size
    );
  end S_Extract;

  -------------------- for exportation:

  procedure Close (File : in out Zipped_File_Type) is
  begin
    if File = null or else File.state = uninitialized then
       raise Use_Error;
    end if;
    Dispose (File.file_name);
    Dispose (File.uncompressed);
    Dispose (File);
    File := null;
  end Close;

  function Name (File : in Zipped_File_Type) return String is
  begin
    return File.file_name.all;
  end Name;

  function Is_Open (File : in Zipped_File_Type) return Boolean is
  begin
    return File /= null and then File.state /= uninitialized;
  end Is_Open;

  function End_Of_File (File : in Zipped_File_Type) return Boolean is
  begin
    if File = null or else File.state = uninitialized then
       raise Use_Error;
    end if;
    return File.state = end_of_zip;
  end End_Of_File;

  procedure Open
     (File             : in out Zipped_File_Type; -- File-in-archive handle
      Archive_Info     : in Zip.Zip_info;         -- Archive's Zip_info
      Name             : in String;               -- Name of zipped entry
      Password         : in String  := "";        -- Decryption password
      Ignore_Directory : in Boolean := False      -- True: will open Name in first directory found
     )
  is
    use Zip_Streams, Ada.Streams;
    zip_stream   : aliased File_Zipstream;
    input_stream : Zipstream_Class_Access;
    use_a_file   : constant Boolean := Zip.Zip_stream (Archive_Info) = null;
  begin
    if File = null then
      File := new UnZip_Stream_Type;
    elsif File.state /= uninitialized then  --  forgot to close last time!
      raise Use_Error;
    end if;
    if use_a_file then
      input_stream := zip_stream'Unchecked_Access;
      Set_Name (zip_stream, Zip.Zip_name (Archive_Info));
      Open (zip_stream, In_File);
    else -- use the given stream
      input_stream := Zip.Zip_stream (Archive_Info);
    end if;
    --
    File.archive_info := Archive_Info;  --  Full clone. Now a copy is safely with File.
    File.file_name := new String'(Name);
    begin
      S_Extract (
        File.archive_info,
        input_stream.all,
        Name,
        Password,
        File.uncompressed,
        null,
        Ignore_Directory
      );
      if use_a_file then
        Close (zip_stream);
      end if;
    exception
      when others =>
        if use_a_file then
          Close (zip_stream);
        end if;
        raise;
    end;
    File.index := File.uncompressed'First;
    File.state := data_uncompressed;
    --  Bug fix for data of size 0 - 29-Nov-2002
    if File.uncompressed'Last < File.index then -- (1..0) array
      File.state := end_of_zip;
    end if;
  end Open;

  procedure Open
     (File             : in out Zipped_File_Type; -- File-in-archive handle
      Archive_Name     : in String;               -- Name of archive file
      Name             : in String;               -- Name of zipped entry
      Password         : in String  := "";        -- Decryption password
      Case_sensitive   : in Boolean := False;
      Ignore_Directory : in Boolean := False      -- True: will open Name in first directory found
     )
   is
    temp_info : Zip.Zip_info;
  begin
    Zip.Load (temp_info, Archive_Name, Case_sensitive);
    Open (File, temp_info, Name, Password, Ignore_Directory);
  end Open;

  procedure Open
     (File             : in out Zipped_File_Type; -- File-in-archive handle
      Archive_Stream   : in out Zip_Streams.Root_Zipstream_Type'Class; -- Archive's stream
      Name             : in String;               -- Name of zipped entry
      Password         : in String  := "";        -- Decryption password
      Case_sensitive   : in Boolean := False;
      Ignore_Directory : in Boolean := False      -- True: will open Name in first directory found
     )
  is
    temp_info : Zip.Zip_info;
  begin
    Zip.Load (temp_info, Archive_Stream, Case_sensitive);
    Open (File, temp_info, Name, Password, Ignore_Directory);
  end Open;

  ------------------------------------------
  -- Read procedure for Unzip_Stream_Type --
  ------------------------------------------

  overriding procedure Read
    (Self   : in out UnZip_Stream_Type;
     Item   :    out Ada.Streams.Stream_Element_Array;
     Last   :    out Ada.Streams.Stream_Element_Offset)
  is
    use Ada.Streams;
  begin
    if Self.state = uninitialized then
      raise Use_Error;
    end if;
    if Self.state = end_of_zip then
      --  Zero transfer -> Last:= Item'First - 1, see RM 13.13.1(8)
      --  No End_Error here, T'Read will raise it: RM 13.13.2(37)
      if Item'First > Stream_Element_Offset'First then
        Last := Item'First - 1;
        return;
      else
        --  Well, we cannot return Item'First - 1...
        raise Constraint_Error; -- RM 13.13.1(11) requires this.
      end if;
    end if;
    if Item'Length = 0 then
      --  Nothing to be read actually.
      Last := Item'Last; -- this is < Item'First
      return;
    end if;
    --  From now on, we can assume Item'Length > 0.

    if Self.index + Item'Length <= Self.uncompressed'Last then
      --  * Normal case: even after reading, the index will be in the range
      Last := Item'Last;
      Item :=
        Self.uncompressed (Self.index .. Self.index + Item'Length - 1);
      Self.index := Self.index + Item'Length;
      --  Now: Stream.index <= Stream.uncompressed'Last,
      --  then at least one element is left to be read, end_of_zip not possible
    else
      --  * Special case: we exhaust the buffer
      Last := Item'First + (Self.uncompressed'Last - Self.index);
      Item (Item'First .. Last) :=
        Self.uncompressed (Self.index .. Self.uncompressed'Last);
      Self.state := end_of_zip;
      --  If Last < Item'Last, the T'Read attribute raises End_Error
      --  because of the incomplete reading.
    end if;
  end Read;

  function Stream (File : Zipped_File_Type) return Stream_Access is
  begin
    return Stream_Access (File);
  end Stream;

  function Size (File : in Zipped_File_Type) return Count is
    comp_size   : Zip.Zip_32_Data_Size_Type;
    uncomp_size : Zip.Zip_32_Data_Size_Type;
  begin
    Zip.Get_sizes (File.archive_info, File.file_name.all, comp_size, uncomp_size);
    return Count (uncomp_size);
  end Size;

  overriding procedure Write
    (Self   : in out UnZip_Stream_Type;
     Item   : in     Ada.Streams.Stream_Element_Array)
  is
    write_not_supported : exception;
  begin
    raise write_not_supported;
  end Write;

  procedure Extract (
     Destination      : in out Ada.Streams.Root_Stream_Type'Class;
     Archive_Info     : in Zip.Zip_info;         -- Archive's Zip_info
     Entry_Name       : in String;               -- Name of zipped entry
     Password         : in String  := "";        -- Decryption password
     Ignore_Directory : in Boolean := False      -- True: will open Name in first directory found
   )
  is
    use Zip_Streams;
    zip_stream   : aliased File_Zipstream;
    input_stream : Zipstream_Class_Access;
    use_a_file   : constant Boolean := Zip.Zip_stream (Archive_Info) = null;
  begin
    if use_a_file then
      input_stream := zip_stream'Unchecked_Access;
      Set_Name (zip_stream, Zip.Zip_name (Archive_Info));
      Open (zip_stream, In_File);
    else -- use the given stream
      input_stream := Zip.Zip_stream (Archive_Info);
    end if;
    declare
      dummy_mem_ptr : p_Stream_Element_Array;
    begin
      S_Extract (
        Archive_Info,
        input_stream.all,
        Entry_Name,
        Password,
        dummy_mem_ptr,
        Destination'Unchecked_Access,
        Ignore_Directory
      );
      if use_a_file then
        Close (zip_stream);
      end if;
    exception
      when others =>
        if use_a_file then
          Close (zip_stream);
        end if;
        raise;
    end;
  end Extract;

end UnZip.Streams;
