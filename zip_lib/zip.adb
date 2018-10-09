-- Legal licensing note:

--  Copyright (c) 1999 .. 2018 Gautier de Montmollin
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

-- NB: this is the MIT License, as found 12-Sep-2007 on the site
-- http://www.opensource.org/licenses/mit-license.php

with Zip.Headers;

with Ada.Characters.Handling;
with Ada.Exceptions;                    use Ada.Exceptions;
with Ada.IO_Exceptions;
with Ada.Strings.Fixed;
with Ada.Strings.Unbounded;

package body Zip is

  use Interfaces;

  -- 19-Jun-2001: Enhanced file name identification
  --              a) when case insensitive  -> all UPPER (current)
  --              b) '\' and '/' identified -> all '/'   (new)

  function Normalize( s: String; case_sensitive: Boolean ) return String is
    sn: String( s'Range );
  begin
    if case_sensitive then
      sn:= s;
    else
      sn:= Ada.Characters.Handling.To_Upper(s);
    end if;
    for i in sn'Range loop
      if sn(i)= '\' then
        sn(i):='/';
      end if;
    end loop;
    return sn;
  end Normalize;

  boolean_to_encoding: constant array(Boolean) of Zip_name_encoding:=
    (False => IBM_437, True => UTF_8);

  -------------------------------------------------------------
  -- Load Zip_info from a stream containing the .zip archive --
  -------------------------------------------------------------

  procedure Load(
    info            :    out Zip_info;
    from            : in out Zip_Streams.Root_Zipstream_Type'Class;
    case_sensitive  : in     Boolean:= False;
    duplicate_names : in     Duplicate_name_policy:= error_on_duplicate
  )
  is
    procedure Insert(
      dico_name        : String; -- UPPER if case-insensitive search
      file_name        : String;
      file_index       : Zip_Streams.ZS_Index_Type;
      comp_size,
      uncomp_size      : File_size_type;
      crc_32           : Unsigned_32;
      date_time        : Time;
      method           : PKZip_method;
      name_encoding    : Zip_name_encoding;
      read_only        : Boolean;
      encrypted_2_x    : Boolean
      )
    is
      node : constant Dir_node :=
            ( (dico_name         => To_Unbounded_String (dico_name),
               file_name         => To_Unbounded_String (file_name),
               file_index        => file_index,
               comp_size         => comp_size,
               uncomp_size       => uncomp_size,
               crc_32            => crc_32,
               date_time         => date_time,
               method            => method,
               name_encoding     => name_encoding,
               read_only         => read_only,
               encrypted_2_x     => encrypted_2_x,
               user_code         => 0
               )
            );
      pos : Dir_node_mapping.Cursor;
      success : Boolean;
    begin
      info.directory.Insert (node.dico_name, node, pos, success);
      if not success then -- A.18.4. 45/2
        --  Here we have a case where the entry name already exists in the dictionary.
        case duplicate_names is
          when error_on_duplicate =>
            Ada.Exceptions.Raise_Exception
              (Duplicate_name'Identity,
               "Same full entry name (in dictionary: " & dico_name &
               ") appears twice in archive directory; " &
               "procedure Load was called with strict name policy, without duplicates."
              );
          when admit_duplicates =>
            null;
        end case;
      end if;
    end Insert;

    the_end: Zip.Headers.End_of_Central_Dir;
    header : Zip.Headers.Central_File_Header;
    zip_info_already_loaded: exception;
    main_comment: Unbounded_String;
  begin -- Load Zip_info
    if info.loaded then
      raise zip_info_already_loaded;
    end if; -- 15-Apr-2002
    Zip.Headers.Load(from, the_end);
    -- We take the opportunity to read the main comment, which is right
    -- after the end-of-central-directory block.
    declare
      main_comment_fixed : String (1 .. Integer (the_end.main_comment_length));
    begin
      String'Read(from'Access, main_comment_fixed);
      main_comment := To_Unbounded_String (main_comment_fixed);
    end;
    -- Process central directory:
    Zip_Streams.Set_Index(
      from,
      Zip_Streams.ZS_Index_Type(1 + the_end.central_dir_offset) + the_end.offset_shifting
    );

    for i in 1..the_end.total_entries loop
      Zip.Headers.Read_and_check(from, header );
      declare
        this_name: String(1..Natural(header.short_info.filename_length));
        use Zip_Streams;
      begin
        String'Read(from'Access, this_name);
        -- Skip extra field and entry comment.
        Set_Index(
          from,
          Index( from ) +
          ZS_Size_Type (
            header.short_info.extra_field_length +
            header.comment_length
          )
        );
        -- Now the whole i_th central directory entry is behind
        Insert( dico_name   => Normalize(this_name, case_sensitive),
                file_name   => Normalize(this_name, True),
                file_index  => Zip_Streams.ZS_Index_Type (1 + header.local_header_offset) +
                               the_end.offset_shifting,
                comp_size   => header.short_info.dd.compressed_size,
                uncomp_size => header.short_info.dd.uncompressed_size,
                crc_32      => header.short_info.dd.crc_32,
                date_time   => header.short_info.file_timedate,
                method      => Method_from_code(header.short_info.zip_type),
                name_encoding =>
                  boolean_to_encoding(
                   (header.short_info.bit_flag and
                    Zip.Headers.Language_Encoding_Flag_Bit) /= 0),
                read_only   => header.made_by_version / 256 = 0 and -- DOS-like
                               (header.external_attributes and 1) = 1,
                encrypted_2_x => (header.short_info.bit_flag and Zip.Headers.Encryption_Flag_Bit) /= 0);
      end;
    end loop;
    info.loaded             := True;
    info.case_sensitive     := case_sensitive;
    info.zip_file_name      := To_Unbounded_String ("This is a stream, no direct file!");
    info.zip_input_stream   := from'Unchecked_Access;
    info.total_entries      := Integer(the_end.total_entries);
    info.zip_file_comment   := main_comment;
    info.zip_archive_format := Zip_32;
  exception
    when Zip.Headers.bad_end =>
      Raise_Exception (Zip.Archive_corrupted'Identity, "Bad (or no) end-of-central-directory");
    when Zip.Headers.bad_central_header =>
      Raise_Exception (Zip.Archive_corrupted'Identity, "Bad central directory entry header");
  end Load;

  -----------------------------------------------------------
  -- Load Zip_info from a file containing the .zip archive --
  -----------------------------------------------------------

  procedure Load(
    info            : out Zip_info;
    from            : in  String; -- Zip file name
    case_sensitive  : in  Boolean:= False;
    duplicate_names : in  Duplicate_name_policy:= error_on_duplicate
  )
  is
    use Zip_Streams;
    MyStream   : aliased File_Zipstream;
  begin
    Set_Name (MyStream, from);
    begin
      Open (MyStream, In_File);
    exception
      when others =>
        Raise_Exception (Zip_file_open_error'Identity, "Archive: [" & from & ']');
    end;
    -- Call the stream version of Load(...)
    Load(
      info,
      MyStream,
      case_sensitive,
      duplicate_names
    );
    Close (MyStream);
    info.zip_file_name := To_Unbounded_String (from);
    info.zip_input_stream := null; -- forget about the stream!
  exception
    when others =>
      if Is_Open(MyStream) then
        Close(MyStream);
      end if;
      raise;
  end Load;

  function Is_loaded (info: in Zip_info) return Boolean is
  begin
    return info.loaded;
  end Is_loaded;

  function Zip_name( info: in Zip_info ) return String is
  begin
    if not info.loaded then
      raise Forgot_to_load_zip_info;
    end if;
    return To_String (info.zip_file_name);
  end Zip_name;

  function Zip_comment( info: in Zip_info ) return String is
  begin
    if not info.loaded then
      raise Forgot_to_load_zip_info;
    end if;
    return To_String (info.zip_file_comment);
  end Zip_comment;

  function Zip_stream( info: in Zip_info ) return Zip_Streams.Zipstream_Class_Access
  is
  begin
    if not info.loaded then
      raise Forgot_to_load_zip_info;
    end if;
    return info.zip_input_stream;
  end Zip_stream;

  function Entries( info: in Zip_info ) return Natural is
  begin
    return info.total_entries;
  end Entries;

  ------------
  -- Delete --
  ------------

  procedure Delete (info : in out Zip_info) is

  begin
    if not info.loaded then
      raise Forgot_to_load_zip_info;
    end if;
    info.loaded:= False; -- <-- added 14-Jan-2002
  end Delete;

  -- Traverse a whole Zip_info directory, giving the
  -- name for each entry to an user-defined "Action" procedure.

  generic
    with procedure Action_private( dn: in out Dir_node );  --  in out : user_code can be changed
    -- Dir_node is private: only known to us, contents subject to change
  procedure Traverse_private_read_write( z: in out Zip_info );

  procedure Traverse_private_read_write( z: in out Zip_info ) is
    use Dir_node_mapping;
    procedure Process_E (Key : Unbounded_String; Element : in out Dir_node) is
    pragma Unreferenced (Key);
    begin
      Action_private (Element);
    end Process_E;
    procedure Process_C (Position : Cursor) is
    begin
      z.directory.Update_Element (Position, Process_E'Access);
    end Process_C;
  begin
    z.directory.Iterate (Process_C'Access);
  end Traverse_private_read_write;

  generic
    with procedure Action_private( dn: Dir_node );  --  in out : user_code can be changed
    -- Dir_node is private: only known to us, contents subject to change
  procedure Traverse_private_read_only( z: Zip_info );

  procedure Traverse_private_read_only( z: Zip_info ) is
    use Dir_node_mapping;
    procedure Process_C (Position : Cursor) is
    begin
      Action_private (Element (Position));
    end Process_C;
  begin
    z.directory.Iterate (Process_C'Access);
  end Traverse_private_read_only;

  -----------------------
  --  Public versions  --
  -----------------------

  procedure Traverse( z: Zip_info ) is
    procedure My_Action_private( dn: Dir_node ) is
    pragma Inline(My_Action_private);
    begin
      Action ( To_String (dn.file_name));
    end My_Action_private;
    procedure My_Traverse_private is new Traverse_private_read_only (My_Action_private);
  begin
    My_Traverse_private(z);
  end Traverse;

  procedure Traverse_Unicode( z: Zip_info ) is
    procedure My_Action_private( dn: Dir_node ) is
    pragma Inline(My_Action_private);
    begin
      Action (To_String (dn.file_name), dn.name_encoding);
    end My_Action_private;
    procedure My_Traverse_private is new Traverse_private_read_only (My_Action_private);
  begin
    My_Traverse_private(z);
  end Traverse_Unicode;

  procedure Traverse_verbose( z: Zip_info ) is
    procedure My_Action_private( dn: Dir_node ) is
    pragma Inline(My_Action_private);
    begin
      Action(
        To_String (dn.file_name),
        dn.file_index,
        dn.comp_size,
        dn.uncomp_size,
        dn.crc_32,
        dn.date_time,
        dn.method,
        dn.name_encoding,
        dn.read_only,
        dn.encrypted_2_x,
        dn.user_code
      );
    end My_Action_private;
    procedure My_Traverse_private is new Traverse_private_read_only (My_Action_private);
  begin
    My_Traverse_private(z);
  end Traverse_verbose;

  procedure Traverse_verbose_altering( z: in out Zip_info ) is
    procedure My_Action_private( dn: in out Dir_node ) is
    pragma Inline(My_Action_private);
    begin
      Action(
        To_String (dn.file_name),
        dn.file_index,
        dn.comp_size,
        dn.uncomp_size,
        dn.crc_32,
        dn.date_time,
        dn.method,
        dn.name_encoding,
        dn.read_only,
        dn.encrypted_2_x,
        dn.user_code
      );
    end My_Action_private;
    procedure My_Traverse_private is new Traverse_private_read_write (My_Action_private);
  begin
    My_Traverse_private(z);
  end Traverse_verbose_altering;

  -- 13-May-2001: Find_first_offset

  -- For an all-files unzipping of an appended (e.g. self-extracting) archive
  -- (not beginning with ZIP contents), we cannot start with
  -- index 1 in file.
  -- But the offset of first entry in ZIP directory is not valid either,
  -- as this excerpt of appnote.txt states:

  -- "   4)  The entries in the central directory may not necessarily
  --         be in the same order that files appear in the zipfile.    "

  procedure Find_first_offset(
    file           : in out Zip_Streams.Root_Zipstream_Type'Class;
    file_index     :    out Zip_Streams.ZS_Index_Type
  )
  is
    the_end   : Zip.Headers.End_of_Central_Dir;
    header    : Zip.Headers.Central_File_Header;
    min_offset: File_size_type;
    use Zip_Streams;
  begin
    Zip.Headers.Load(file, the_end);
    Set_Index(
      file,
      ZS_Index_Type (1 + the_end.central_dir_offset) + the_end.offset_shifting
    );

    min_offset:= the_end.central_dir_offset; -- will be lowered if the archive is not empty.

    if the_end.total_entries = 0 then
      raise Archive_is_empty;
    end if;

    for i in 1..the_end.total_entries loop
      Zip.Headers.Read_and_check(file, header );
      Set_Index( file,
        Index( file ) +
        ZS_Size_Type
             ( header.short_info.filename_length +
               header.short_info.extra_field_length +
               header.comment_length
              )
      );
      -- Now the whole i_th central directory entry is behind

      if header.local_header_offset < min_offset then
        min_offset:= header.local_header_offset;
      end if;
    end loop;

    file_index:= Zip_Streams.ZS_Index_Type (1 + min_offset) + the_end.offset_shifting;

  exception
    when Zip.Headers.bad_end | Ada.IO_Exceptions.End_Error =>
      Raise_Exception (Zip.Archive_corrupted'Identity, "Bad (or no) end-of-central-directory");
    when Zip.Headers.bad_central_header =>
      Raise_Exception (Zip.Archive_corrupted'Identity, "Bad central directory entry header");
  end Find_first_offset;

  -- Internal: find offset of a zipped file by reading sequentially the
  -- central directory :-(

  procedure Find_offset(
    file           : in out Zip_Streams.Root_Zipstream_Type'Class;
    name           : in     String;
    case_sensitive : in     Boolean;
    file_index     :    out Zip_Streams.ZS_Index_Type;
    comp_size      :    out File_size_type;
    uncomp_size    :    out File_size_type;
    crc_32         :    out Interfaces.Unsigned_32
  )
  is
    the_end: Zip.Headers.End_of_Central_Dir;
    header : Zip.Headers.Central_File_Header;
    use Zip_Streams;
  begin
    Zip.Headers.Load(file, the_end);
    Set_Index(file, ZS_Index_Type(1 + the_end.central_dir_offset) + the_end.offset_shifting);
    for i in 1..the_end.total_entries loop
      Zip.Headers.Read_and_check(file, header);
      declare
        this_name: String(1..Natural(header.short_info.filename_length));
      begin
        String'Read(file'Access, this_name);
        Set_Index( file,
          Index( file ) +
          ZS_Size_Type(
                  header.short_info.extra_field_length +
                  header.comment_length
          )
        );
        -- Now the whole i_th central directory entry is behind
        if Normalize(this_name,case_sensitive) =
           Normalize(name,case_sensitive)
        then
          -- Name found in central directory !
          file_index := Zip_Streams.ZS_Index_Type (1 + header.local_header_offset) + the_end.offset_shifting;
          comp_size  := File_size_type(header.short_info.dd.compressed_size);
          uncomp_size:= File_size_type(header.short_info.dd.uncompressed_size);
          crc_32     := header.short_info.dd.crc_32;
          return;
        end if;
      end;
    end loop;
    Raise_Exception (File_name_not_found'Identity, "Entry: [" & name & ']');
  exception
    when Zip.Headers.bad_end =>
      Raise_Exception (Zip.Archive_corrupted'Identity, "Bad (or no) end-of-central-directory");
    when Zip.Headers.bad_central_header =>
      Raise_Exception (Zip.Archive_corrupted'Identity, "Bad central directory entry header");
  end Find_offset;

  -- Internal: find offset of a zipped file using the zip_info tree 8-)

  procedure Find_offset(
    info           : in     Zip_info;
    name           : in     String;
    name_encoding  :    out Zip_name_encoding;
    file_index     :    out Zip_Streams.ZS_Index_Type;
    comp_size      :    out File_size_type;
    uncomp_size    :    out File_size_type;
    crc_32         :    out Interfaces.Unsigned_32
  )
  is
    up_name: constant String:= Normalize(name, info.case_sensitive);
    use Dir_node_mapping;
    cm : Cursor;
  begin
    if not info.loaded then
      raise Forgot_to_load_zip_info;
    end if;
    cm := info.directory.Find (To_Unbounded_String (up_name));
    if cm = Dir_node_mapping.No_Element then
      Ada.Exceptions.Raise_Exception(
        File_name_not_found'Identity,
        "Archive: [" & To_String (info.zip_file_name) & "], entry: [" & name & ']'
      );
    end if;
    --  Entry found !
    declare
      dn : constant Dir_node := Element (cm);
    begin
      name_encoding := dn.name_encoding;
      file_index    := dn.file_index;
      comp_size     := dn.comp_size;
      uncomp_size   := dn.uncomp_size;
      crc_32        := dn.crc_32;
    end;
  end Find_offset;

  procedure Find_offset_without_directory(
    info           : in     Zip_info;
    name           : in     String;
    name_encoding  :    out Zip_name_encoding;
    file_index     :    out Zip_Streams.ZS_Index_Type;
    comp_size      :    out File_size_type;
    uncomp_size    :    out File_size_type;
    crc_32         :    out Interfaces.Unsigned_32
  )
  is
    function Trash_dir( n: String ) return String is
      idx: Integer:= n'First - 1;
    begin
      for i in n'Range loop
        if n(i)= '/' or n(i)='\' then
          idx:= i;
        end if;
      end loop;
      -- idx points on the index just before the interesting part
      return Normalize(n( idx+1 .. n'Last ), info.case_sensitive);
    end Trash_dir;

    simple_name: constant String:= Trash_dir(name);

    Found: exception;

    procedure Check_entry(
      entry_name          : String; -- 'name' is compressed entry's name
      entry_index         : Zip_Streams.ZS_Index_Type;
      entry_comp_size     : File_size_type;
      entry_uncomp_size   : File_size_type;
      entry_crc_32        : Interfaces.Unsigned_32;
      date_time           : Time;
      method              : PKZip_method;
      entry_name_encoding : Zip_name_encoding;
      read_only           : Boolean;
      encrypted_2_x       : Boolean; -- PKZip 2.x encryption
      user_code           : Integer
    )
    is
    pragma Unreferenced (date_time, method, read_only, encrypted_2_x, user_code);
    begin
      if Trash_dir(entry_name) = simple_name then
        name_encoding := entry_name_encoding;
        file_index    := entry_index;
        comp_size     := entry_comp_size;
        uncomp_size   := entry_uncomp_size;
        crc_32        := entry_crc_32;
        raise Found;
      end if;
    end Check_entry;
    --
    procedure Search is new Traverse_verbose(Check_entry);
    --
  begin
    begin
      Search(info);
    exception
      when Found =>
        return;
    end;
    raise File_name_not_found;
  end Find_offset_without_directory;

  function Exists(
    info           : in     Zip_info;
    name           : in     String
  )
  return Boolean
  is
    up_name: constant String:= Normalize(name, info.case_sensitive);
    use Dir_node_mapping;
    cm : Cursor;
  begin
    if not info.loaded then
      raise Forgot_to_load_zip_info;
    end if;
    cm := info.directory.Find (To_Unbounded_String (up_name));
    return cm /= Dir_node_mapping.No_Element;
  end Exists;

  procedure Set_user_code(
    info           : in out Zip_info;
    name           : in     String;
    code           : in     Integer
  )
  is
    up_name: constant String:= Normalize(name, info.case_sensitive);
    use Dir_node_mapping;
    cm : Cursor;
    procedure Process (Key : Unbounded_String; Element : in out Dir_node) is
    pragma Unreferenced (Key);
    begin
      Element.user_code := code;
    end Process;
  begin
    if not info.loaded then
      raise Forgot_to_load_zip_info;
    end if;
    cm := info.directory.Find (To_Unbounded_String (up_name));
    if cm = Dir_node_mapping.No_Element then
      Ada.Exceptions.Raise_Exception(
        File_name_not_found'Identity,
        "Archive: [" & To_String (info.zip_file_name) & "], entry: [" & name & ']'
      );
    end if;
    info.directory.Update_Element (cm, Process'Access);
  end Set_user_code;

  function User_code(
    info           : in Zip_info;
    name           : in String
  )
  return Integer
  is
    up_name: constant String:= Normalize(name, info.case_sensitive);
    use Dir_node_mapping;
    cm : Cursor;
  begin
    if not info.loaded then
      raise Forgot_to_load_zip_info;
    end if;
    cm := info.directory.Find (To_Unbounded_String (up_name));
    if cm = Dir_node_mapping.No_Element then
      Ada.Exceptions.Raise_Exception(
        File_name_not_found'Identity,
        "Archive: [" & To_String (info.zip_file_name) & "], entry: [" & name & ']'
      );
    end if;
    return Element (cm).user_code;
  end User_code;

  procedure Get_sizes(
    info           : in     Zip_info;
    name           : in     String;
    comp_size      :    out File_size_type;
    uncomp_size    :    out File_size_type
  )
  is
    dummy_file_index: Zip_Streams.ZS_Index_Type;
    dummy_name_encoding: Zip_name_encoding;
    dummy_crc_32: Interfaces.Unsigned_32;
  begin
    Find_offset(
      info, name, dummy_name_encoding, dummy_file_index,
      comp_size, uncomp_size, dummy_crc_32
    );
  end Get_sizes;

  -- Workaround for the severe xxx'Read xxx'Write performance
  -- problems in the GNAT and ObjectAda compilers (as in 2009)
  -- This is possible if and only if Byte = Stream_Element and
  -- arrays types are both packed and aligned the same way.
  --
  subtype Size_test_a is Byte_Buffer(1..19);
  subtype Size_test_b is Ada.Streams.Stream_Element_Array(1..19);
  workaround_possible: constant Boolean:=
    Size_test_a'Size = Size_test_b'Size and
    Size_test_a'Alignment = Size_test_b'Alignment;

  -- BlockRead - general-purpose procedure (nothing really specific
  -- to Zip / UnZip): reads either the whole buffer from a file, or
  -- if the end of the file lays inbetween, a part of the buffer.

  procedure BlockRead(
    file         : in     Ada.Streams.Stream_IO.File_Type;
    buffer       :    out Byte_Buffer;
    actually_read:    out Natural
  )
  is
    use Ada.Streams, Ada.Streams.Stream_IO;
    SE_Buffer   : Stream_Element_Array (1 .. buffer'Length);
    for SE_Buffer'Address use buffer'Address;
    pragma Import (Ada, SE_Buffer);
    Last_Read   : Stream_Element_Offset;
  begin
    if workaround_possible then
      Read(Stream(file).all, SE_Buffer, Last_Read);
      actually_read:= Natural(Last_Read);
    else
      if End_Of_File(file) then
        actually_read:= 0;
      else
        actually_read:=
          Integer'Min( buffer'Length, Integer(Size(file) - Index(file) + 1) );
        Byte_Buffer'Read(
          Stream(file),
          buffer(buffer'First .. buffer'First + actually_read - 1)
        );
      end if;
    end if;
  end BlockRead;

  procedure BlockRead(
    stream       : in out Zip_Streams.Root_Zipstream_Type'Class;
    buffer       :    out Byte_Buffer;
    actually_read:    out Natural
  )
  is
    use Ada.Streams, Zip_Streams;
    SE_Buffer   : Stream_Element_Array (1 .. buffer'Length);
    for SE_Buffer'Address use buffer'Address;
    pragma Import (Ada, SE_Buffer);
    Last_Read   : Stream_Element_Offset;
  begin
    if workaround_possible then
      Read(stream, SE_Buffer, Last_Read);
      actually_read:= Natural(Last_Read);
    else
      if End_Of_Stream(stream) then
        actually_read:= 0;
      else
        actually_read:=
          Integer'Min( buffer'Length, Integer(Size(stream) - Index(stream) + 1) );
        Byte_Buffer'Read(
          stream'Access,
          buffer(buffer'First .. buffer'First + actually_read - 1)
        );
      end if;
    end if;
  end BlockRead;

  procedure BlockRead(
    stream : in out Zip_Streams.Root_Zipstream_Type'Class;
    buffer :    out Byte_Buffer
  )
  is
    actually_read: Natural;
  begin
    BlockRead(stream, buffer, actually_read);
    if actually_read < buffer'Length then
      raise Ada.IO_Exceptions.End_Error;
    end if;
  end BlockRead;

  procedure BlockWrite(
    stream : in out Ada.Streams.Root_Stream_Type'Class;
    buffer : in     Byte_Buffer
  )
  is
    use Ada.Streams;
    SE_Buffer   : Stream_Element_Array (1 .. buffer'Length);
    for SE_Buffer'Address use buffer'Address;
    pragma Import (Ada, SE_Buffer);
  begin
    if workaround_possible then
      Ada.Streams.Write(stream, SE_Buffer);
    else
      Byte_Buffer'Write(stream'Access, buffer);
      -- ^This is 30x to 70x slower on GNAT 2009 !
    end if;
  end BlockWrite;

  function Image(m: PKZip_method) return String is
  begin
    case m is
      when store     => return "Store";
      when shrink    => return "Shrink";
      when reduce_1  => return "Reduce 1";
      when reduce_2  => return "Reduce 2";
      when reduce_3  => return "Reduce 3";
      when reduce_4  => return "Reduce 4";
      when implode   => return "Implode";
      when tokenize  => return "Tokenize";
      when deflate   => return "Deflate";
      when deflate_e => return "Deflate64";
      when bzip2     => return "BZip2";
      when lzma_meth => return "LZMA";
      when ppmd      => return "PPMd";
      when unknown   => return "(unknown)";
    end case;
  end Image;

  function Method_from_code(x: Natural) return PKZip_method is
    -- An enumeration clause might be more elegant, but needs
    -- curiously an Unchecked_Conversion... (RM 13.4)
  begin
    case x is
      when compression_format_code.store      => return store;
      when compression_format_code.shrink     => return shrink;
      when compression_format_code.reduce     => return reduce_1;
      when compression_format_code.reduce + 1 => return reduce_2;
      when compression_format_code.reduce + 2 => return reduce_3;
      when compression_format_code.reduce + 3 => return reduce_4;
      when compression_format_code.implode    => return implode;
      when compression_format_code.tokenize   => return tokenize;
      when compression_format_code.deflate    => return deflate;
      when compression_format_code.deflate_e  => return deflate_e;
      when compression_format_code.bzip2      => return bzip2;
      when compression_format_code.lzma       => return lzma_meth;
      when compression_format_code.ppmd       => return ppmd;
      when others => return unknown;
    end case;
  end Method_from_code;

  function Method_from_code(x: Interfaces.Unsigned_16) return PKZip_method is
  begin
    return Method_from_code(Natural(x));
  end Method_from_code;

  -- Copy a chunk from a stream into another one, using a temporary buffer
  procedure Copy_chunk (
    from       : in out Zip_Streams.Root_Zipstream_Type'Class;
    into       : in out Ada.Streams.Root_Stream_Type'Class;
    bytes      : Natural;
    buffer_size: Positive:= 1024*1024;
    Feedback   : Feedback_proc:= null
  )
  is
    buf: Zip.Byte_Buffer(1..buffer_size);
    actually_read, remains: Natural;
    user_abort: Boolean:= False;
  begin
    remains:= bytes;
    while remains > 0 loop
      if Feedback /= null then
        Feedback(
          100 - Integer(100.0 * Float(remains) / Float(bytes)),
          False,
          user_abort
        );
        -- !! do something if user_abort = True !!
      end if;
      Zip.BlockRead(from, buf(1..Integer'Min(remains, buf'Last)), actually_read);
      if actually_read = 0 then -- premature end, unexpected
        raise Zip.Archive_corrupted;
      end if;
      remains:= remains - actually_read;
      Zip.BlockWrite(into, buf(1..actually_read));
    end loop;
  end Copy_chunk;

  -- Copy a whole file into a stream, using a temporary buffer
  procedure Copy_file(
    file_name  : String;
    into       : in out Ada.Streams.Root_Stream_Type'Class;
    buffer_size: Positive:= 1024*1024
  )
  is
    use Ada.Streams.Stream_IO;
    f: File_Type;
    buf: Zip.Byte_Buffer(1..buffer_size);
    actually_read: Natural;
  begin
    Open(f, In_File, file_name);
    loop
      Zip.BlockRead(f, buf, actually_read);
      exit when actually_read = 0; -- this is expected
      Zip.BlockWrite(into, buf(1..actually_read));
    end loop;
    Close(f);
  end Copy_file;

  -- This does the same as Ada 2005's Ada.Directories.Exists
  -- Just there as helper for Ada 95 only systems
  --
  function Exists(name:String) return Boolean is
    use Ada.Text_IO, Ada.Strings.Fixed;
    f: File_Type;
  begin
    if Index(name, "*") > 0 then
      return False;
    end if;
    Open(f,In_File,name, Form => Ada.Strings.Unbounded.To_String (Zip_Streams.Form_For_IO_Open_and_Create));
    Close(f);
    return True;
  exception
    when Name_Error =>
      return False; -- The file cannot exist !
    when Use_Error =>
      return True;  -- The file exist and is already opened !
  end Exists;

  procedure Put_Multi_Line(
    out_file :        Ada.Text_IO.File_Type;
    text     :        String
  )
  is
    last_char: Character:= ' ';
    c: Character;
  begin
    for i in text'Range loop
      c:= text(i);
      case c is
        when ASCII.CR =>
          Ada.Text_IO.New_Line(out_file);
        when ASCII.LF =>
          if last_char /= ASCII.CR then Ada.Text_IO.New_Line(out_file); end if;
        when others =>
          Ada.Text_IO.Put(out_file, c);
      end case;
      last_char:= c;
    end loop;
  end Put_Multi_Line;

  procedure Write_as_text(
    out_file :        Ada.Text_IO.File_Type;
    buffer   :        Byte_Buffer;
    last_char: in out Character -- track line-ending characters across writes
  )
  is
    c: Character;
  begin
    for i in buffer'Range loop
      c:= Character'Val(buffer(i));
      case c is
        when ASCII.CR =>
          Ada.Text_IO.New_Line(out_file);
        when ASCII.LF =>
          if last_char /= ASCII.CR then Ada.Text_IO.New_Line(out_file); end if;
        when others =>
          Ada.Text_IO.Put(out_file, c);
      end case;
      last_char:= c;
    end loop;
  end Write_as_text;

  function Hexadecimal(x: Interfaces.Unsigned_32) return String
  is
    package MIO is new Ada.Text_IO.Modular_IO(Interfaces.Unsigned_32);
    str: String(1..12);
    use Ada.Strings.Fixed;
  begin
    MIO.Put(str, x, 16);
    return str(Index(str,"#")+1..11);
  end Hexadecimal;

end Zip;
