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

--  NB: this is the MIT License, as found 12-Sep-2007 on the site
--  http://www.opensource.org/licenses/mit-license.php

with Zip.Headers;

with Ada.Characters.Handling;
with Ada.Unchecked_Deallocation;
with Ada.IO_Exceptions;
with Ada.Strings.Fixed;
with Ada.Strings.Unbounded;

package body Zip is

  use Interfaces;

  procedure Dispose is new Ada.Unchecked_Deallocation (Dir_node, p_Dir_node);
  procedure Dispose is new Ada.Unchecked_Deallocation (String, p_String);

  package Binary_tree_rebalancing is
    procedure Rebalance (root : in out p_Dir_node);
  end Binary_tree_rebalancing;

  package body Binary_tree_rebalancing is

    --------------------------------------------------------------------
    --  Tree Rebalancing in Optimal Time and Space                    --
    --  QUENTIN F. STOUT and BETTE L. WARREN                          --
    --  Communications of the ACM September 1986 Volume 29 Number 9   --
    --------------------------------------------------------------------
    --  http://www.eecs.umich.edu/~qstout/pap/CACM86.pdf
    --
    --  Translated by (New) P2Ada v. 15-Nov-2006

    procedure Tree_to_vine (root : p_Dir_node; size : out Integer)
      --  transform the tree with pseudo-root
      --   "root^" into a vine with pseudo-root
      --   node "root^", and store the number of
      --   nodes in "size"
    is
      vine_tail, remainder, temp : p_Dir_node;
    begin
      vine_tail := root;
      remainder := vine_tail.right;
      size := 0;
      while remainder /= null loop
        if remainder.left = null then
          --  move vine-tail down one:
          vine_tail := remainder;
          remainder := remainder.right;
          size := size + 1;
        else
          --  rotate:
          temp := remainder.left;
          remainder.left := temp.right;
          temp.right := remainder;
          remainder := temp;
          vine_tail.right := temp;
        end if;
      end loop;
    end Tree_to_vine;

    procedure Vine_to_tree (root : p_Dir_node; size_given : Integer) is
      --  convert the vine with "size" nodes and pseudo-root
      --  node "root^" into a balanced tree
      leaf_count : Integer;
      size : Integer := size_given;

      procedure Compression (root_compress : p_Dir_node; count : Integer) is
        --  Compress "count" spine nodes in the tree with pseudo-root "root_compress^"
        scanner, child : p_Dir_node;
      begin
        scanner := root_compress;
        for counter in reverse 1 .. count loop
          child         := scanner.right;
          scanner.right := child.right;
          scanner       := scanner.right;
          child.right   := scanner.left;
          scanner.left  := child;
        end loop;
      end Compression;

      --  Returns n - 2 ** Integer( Float'Floor( log( Float(n) ) / log(2.0) ) )
      --  without Float-Point calculation and rounding errors with too short floats
      function Remove_leading_binary_1 (n : Integer) return Integer is
        x : Integer := 2**16;  --  supposed maximum
      begin
        if n < 1 then
          return n;
        end if;
        while n mod x = n loop
          x := x / 2;
        end loop;
        return n mod x;
      end Remove_leading_binary_1;

    begin --  Vine_to_tree
      leaf_count := Remove_leading_binary_1 (size + 1);
      Compression (root, leaf_count);  --  create deepest leaves
      --  use Perfect_leaves instead for a perfectly balanced tree
      size := size - leaf_count;
      while size > 1 loop
        Compression (root, size / 2);
        size := size / 2;
      end loop;
    end Vine_to_tree;

    procedure Rebalance (root : in out p_Dir_node) is
      --  Rebalance the binary search tree with root "root.all",
      --  with the result also rooted at "root.all".
      --  Uses the Tree_to_vine and Vine_to_tree procedures.
      pseudo_root : p_Dir_node;
      size : Integer;
    begin
      pseudo_root := new Dir_node (name_len => 0);
      pseudo_root.right := root;
      Tree_to_vine (pseudo_root, size);
      Vine_to_tree (pseudo_root, size);
      root := pseudo_root.right;
      Dispose (pseudo_root);
    end Rebalance;

  end Binary_tree_rebalancing;

  --  19-Jun-2001: Enhanced file name identification
  --               a) when case insensitive  -> all UPPER (current)
  --               b) '\' and '/' identified -> all '/'   (new)

  function Normalize (s : String; case_sensitive : Boolean) return String is
    sn : String (s'Range);
  begin
    if case_sensitive then
      sn := s;
    else
      sn := Ada.Characters.Handling.To_Upper (s);
    end if;
    for i in sn'Range loop
      if sn (i) = '\' then
        sn (i) := '/';
      end if;
    end loop;
    return sn;
  end Normalize;

  boolean_to_encoding : constant array (Boolean) of Zip_name_encoding :=
    (False => IBM_437, True => UTF_8);

  -------------------------------------------------------------
  -- Load Zip_info from a stream containing the .zip archive --
  -------------------------------------------------------------

  procedure Load (
    info            :    out Zip_info;
    from            : in out Zip_Streams.Root_Zipstream_Type'Class;
    case_sensitive  : in     Boolean := False;
    duplicate_names : in     Duplicate_name_policy := error_on_duplicate
  )
  is
    procedure Insert (
      dico_name        : String; -- UPPER if case-insensitive search
      file_name        : String;
      file_index       : Zip_Streams.ZS_Index_Type;
      comp_size,
      uncomp_size      : Zip_32_Data_Size_Type;
      crc_32           : Unsigned_32;
      date_time        : Time;
      method           : PKZip_method;
      name_encoding    : Zip_name_encoding;
      read_only        : Boolean;
      encrypted_2_x    : Boolean;
      root_node        : in out p_Dir_node
      )
    is
      procedure Insert_into_tree (node : in out p_Dir_node) is
      begin
        if node = null then
          node := new Dir_node'
            ((name_len          => file_name'Length,
               left              => null,
               right             => null,
               dico_name         => dico_name,
               file_name         => file_name,
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
        elsif dico_name > node.dico_name then
          Insert_into_tree (node.right);
        elsif dico_name < node.dico_name then
          Insert_into_tree (node.left);
        else
          --  Here we have a case where the entry name already exists in the dictionary.
          case duplicate_names is
            when error_on_duplicate =>
              raise Duplicate_name with
                 "Same full entry name (in dictionary: " & dico_name &
                 ") appears twice in archive directory; " &
                 "procedure Load was called with strict name policy.";
            when admit_duplicates =>
              if file_index > node.file_index then
                Insert_into_tree (node.right);
              elsif file_index < node.file_index then
                Insert_into_tree (node.left);
              else
                raise Duplicate_name with
                   "Archive directory corrupt: same full entry name (in dictionary: " &
                   dico_name & "), with same data position, appear twice.";
              end if;
          end case;
        end if;
      end Insert_into_tree;
      --
    begin
      Insert_into_tree (root_node);
    end Insert;

    the_end : Zip.Headers.End_of_Central_Dir;
    header  : Zip.Headers.Central_File_Header;
    p       : p_Dir_node := null;
    main_comment : p_String;
  begin  --  Load Zip_info
    if info.loaded then
      Delete (info);
    end if;
    Zip.Headers.Load (from, the_end);
    --  We take the opportunity to read the main comment, which is right
    --  after the end-of-central-directory block.
    main_comment := new String (1 .. Integer (the_end.main_comment_length));
    String'Read (from'Access, main_comment.all);
    --  Process central directory:
    Zip_Streams.Set_Index (
      from,
      Zip_Streams.ZS_Index_Type (1 + the_end.central_dir_offset) + the_end.offset_shifting
    );

    for i in 1 .. the_end.total_entries loop
      Zip.Headers.Read_and_check (from, header);
      declare
        this_name : String (1 .. Natural (header.short_info.filename_length));
        use Zip_Streams;
      begin
        String'Read (from'Access, this_name);
        --  Skip extra field and entry comment.
        Set_Index (
          from,
          Index (from) +
          ZS_Size_Type (
            header.short_info.extra_field_length +
            header.comment_length
          )
        );
        --  Now the whole i_th central directory entry is behind
        Insert (dico_name   => Normalize (this_name, case_sensitive),
                file_name   => Normalize (this_name, True),
                file_index  => Zip_Streams.ZS_Index_Type (1 + header.local_header_offset) +
                               the_end.offset_shifting,
                comp_size   => header.short_info.dd.compressed_size,
                uncomp_size => header.short_info.dd.uncompressed_size,
                crc_32      => header.short_info.dd.crc_32,
                date_time   => header.short_info.file_timedate,
                method      => Method_from_code (header.short_info.zip_type),
                name_encoding =>
                  boolean_to_encoding (
                   (header.short_info.bit_flag and
                    Zip.Headers.Language_Encoding_Flag_Bit) /= 0),
                read_only   => header.made_by_version / 256 = 0 and -- DOS-like
                               (header.external_attributes and 1) = 1,
                encrypted_2_x => (header.short_info.bit_flag and Zip.Headers.Encryption_Flag_Bit) /= 0,
                root_node     => p);
        --  Since the files are usually well ordered, the tree as inserted
        --  is very unbalanced; we need to rebalance it from time to time
        --  during loading, otherwise the insertion slows down dramatically
        --  for zip files with plenty of files - converges to
        --  O(total_entries ** 2)...
        if i mod 256 = 0 then
          Binary_tree_rebalancing.Rebalance (p);
        end if;
      end;
    end loop;
    Binary_tree_rebalancing.Rebalance (p);
    info.loaded             := True;
    info.case_sensitive     := case_sensitive;
    info.zip_file_name      := new String'("This is a stream, no direct file!");
    info.zip_input_stream   := from'Unchecked_Access;
    info.dir_binary_tree    := p;
    info.total_entries      := Integer (the_end.total_entries);
    info.zip_file_comment   := main_comment;
    info.zip_archive_format := Zip_32;
  exception
    when Zip.Headers.bad_end =>
      raise Zip.Archive_corrupted with "Bad (or no) end-of-central-directory";
    when Zip.Headers.bad_central_header =>
      raise Zip.Archive_corrupted with "Bad central directory entry header";
  end Load;

  -----------------------------------------------------------
  -- Load Zip_info from a file containing the .zip archive --
  -----------------------------------------------------------

  procedure Load (
    info            : out Zip_info;
    from            : in  String;  --  Zip file name
    case_sensitive  : in  Boolean := False;
    duplicate_names : in  Duplicate_name_policy := error_on_duplicate
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
        raise Archive_open_error with "Archive: [" & from & ']';
    end;
    --  Call the stream version of Load(...)
    Load (
      info,
      MyStream,
      case_sensitive,
      duplicate_names
    );
    Close (MyStream);
    Dispose (info.zip_file_name);
    info.zip_file_name := new String'(from);
    info.zip_input_stream := null; -- forget about the stream!
  exception
    when others =>
      if Is_Open (MyStream) then
        Close (MyStream);
      end if;
      raise;
  end Load;

  function Is_loaded (info : in Zip_info) return Boolean is
  begin
    return info.loaded;
  end Is_loaded;

  function Zip_name (info : in Zip_info) return String is
  begin
    if not info.loaded then
      raise Forgot_to_load_zip_info;
    end if;
    return info.zip_file_name.all;
  end Zip_name;

  function Zip_comment (info : in Zip_info) return String is
  begin
    if not info.loaded then
      raise Forgot_to_load_zip_info;
    end if;
    return info.zip_file_comment.all;
  end Zip_comment;

  function Zip_stream (info : in Zip_info) return Zip_Streams.Zipstream_Class_Access
  is
  begin
    if not info.loaded then
      raise Forgot_to_load_zip_info;
    end if;
    return info.zip_input_stream;
  end Zip_stream;

  function Entries (info : in Zip_info) return Natural is
  begin
    return info.total_entries;
  end Entries;

  ------------
  -- Delete --
  ------------

  procedure Delete (info : in out Zip_info) is

    procedure Delete (p : in out p_Dir_node) is
    begin
      if p /= null then
         Delete (p.left);
         Delete (p.right);
         Dispose (p);
         p := null;
      end if;
    end Delete;

  begin
    Delete (info.dir_binary_tree);
    Dispose (info.zip_file_name);
    Dispose (info.zip_file_comment);
    info.loaded := False;  --  <-- added 14-Jan-2002
  end Delete;

  --  Traverse a whole Zip_info directory in sorted order, giving the
  --  name for each entry to an user-defined "Action" procedure.

  generic
    with procedure Action_private (dn : in out Dir_node);
    --  Dir_node is private: only known to us, contents subject to change
  procedure Traverse_private (z : Zip_info);

  procedure Traverse_private (z : Zip_info) is

    procedure Traverse_tree (p : p_Dir_node) is
    begin
      if p /= null then
        Traverse_tree (p.left);
        Action_private (p.all);
        Traverse_tree (p.right);
      end if;
    end Traverse_tree;

  begin
    Traverse_tree (z.dir_binary_tree);
  end Traverse_private;

  -----------------------
  --  Public versions  --
  -----------------------

  procedure Traverse (z : Zip_info) is
    procedure My_Action_private (dn : in out Dir_node) is
    pragma Inline (My_Action_private);
    begin
      Action (dn.file_name);
    end My_Action_private;
    procedure My_Traverse_private is new Traverse_private (My_Action_private);
  begin
    My_Traverse_private (z);
  end Traverse;

  procedure Traverse_Unicode (z : Zip_info) is
    procedure My_Action_private (dn : in out Dir_node) is
    pragma Inline (My_Action_private);
    begin
      Action (dn.file_name, dn.name_encoding);
    end My_Action_private;
    procedure My_Traverse_private is new Traverse_private (My_Action_private);
  begin
    My_Traverse_private (z);
  end Traverse_Unicode;

  procedure Traverse_verbose (z : Zip_info) is
    procedure My_Action_private (dn : in out Dir_node) is
    pragma Inline (My_Action_private);
    begin
      Action (
        dn.file_name,
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
    procedure My_Traverse_private is new Traverse_private (My_Action_private);
  begin
    My_Traverse_private (z);
  end Traverse_verbose;

  procedure Tree_stat (
    z         : in     Zip_info;
    total     :    out Natural;
    max_depth :    out Natural;
    avg_depth :    out Float
  )
  is
    sum_depth : Natural := 0;

    procedure Traverse_tree (p : p_Dir_node; depth : Natural) is
    begin
      if p /= null then
        total := total + 1;
        if depth > max_depth then
          max_depth := depth;
        end if;
        sum_depth := sum_depth + depth;
        Traverse_tree (p.left, depth + 1);
        Traverse_tree (p.right, depth + 1);
      end if;
    end Traverse_tree;

  begin
    total := 0;
    max_depth := 0;
    Traverse_tree (z.dir_binary_tree, 0);
    if total = 0 then
      avg_depth := 0.0;
    else
      avg_depth := Float (sum_depth) / Float (total);
    end if;
  end Tree_stat;

  --  13-May-2001: Find_first_offset

  --  For an all-files unzipping of an appended (e.g. self-extracting) archive
  --  (not beginning with ZIP contents), we cannot start with
  --  index 1 in file.
  --  But the offset of first entry in ZIP directory is not valid either,
  --  as this excerpt of appnote.txt states:

  --  "   4)  The entries in the central directory may not necessarily
  --          be in the same order that files appear in the zipfile.    "

  procedure Find_first_offset (
    file           : in out Zip_Streams.Root_Zipstream_Type'Class;
    file_index     :    out Zip_Streams.ZS_Index_Type
  )
  is
    the_end    : Zip.Headers.End_of_Central_Dir;
    header     : Zip.Headers.Central_File_Header;
    min_offset : Zip_32_Data_Size_Type;
    use Zip_Streams;
  begin
    Zip.Headers.Load (file, the_end);
    Set_Index (
      file,
      ZS_Index_Type (1 + the_end.central_dir_offset) + the_end.offset_shifting
    );

    min_offset := the_end.central_dir_offset; -- will be lowered if the archive is not empty.

    if the_end.total_entries = 0 then
      raise Archive_is_empty;
    end if;

    for i in 1 .. the_end.total_entries loop
      Zip.Headers.Read_and_check (file, header);
      Set_Index (file,
        Index (file) +
        ZS_Size_Type
              (header.short_info.filename_length +
               header.short_info.extra_field_length +
               header.comment_length
              )
      );
      --  Now the whole i_th central directory entry is behind

      if header.local_header_offset < min_offset then
        min_offset := header.local_header_offset;
      end if;
    end loop;

    file_index := Zip_Streams.ZS_Index_Type (1 + min_offset) + the_end.offset_shifting;

  exception
    when Zip.Headers.bad_end | Ada.IO_Exceptions.End_Error =>
      raise Zip.Archive_corrupted with "Bad (or no) end-of-central-directory";
    when Zip.Headers.bad_central_header =>
      raise Zip.Archive_corrupted with "Bad central directory entry header";
  end Find_first_offset;

  --  Internal: find offset of a zipped file by reading sequentially the
  --  central directory :-(

  procedure Find_offset (
    file           : in out Zip_Streams.Root_Zipstream_Type'Class;
    name           : in     String;
    case_sensitive : in     Boolean;
    file_index     :    out Zip_Streams.ZS_Index_Type;
    comp_size      :    out Zip_32_Data_Size_Type;
    uncomp_size    :    out Zip_32_Data_Size_Type;
    crc_32         :    out Interfaces.Unsigned_32
  )
  is
    the_end : Zip.Headers.End_of_Central_Dir;
    header  : Zip.Headers.Central_File_Header;
    use Zip_Streams;
  begin
    Zip.Headers.Load (file, the_end);
    Set_Index (file, ZS_Index_Type (1 + the_end.central_dir_offset) + the_end.offset_shifting);
    for i in 1 .. the_end.total_entries loop
      Zip.Headers.Read_and_check (file, header);
      declare
        this_name : String (1 .. Natural (header.short_info.filename_length));
      begin
        String'Read (file'Access, this_name);
        Set_Index (file,
          Index (file) +
          ZS_Size_Type (
                  header.short_info.extra_field_length +
                  header.comment_length
          )
        );
        --  Now the whole i_th central directory entry is behind
        if Normalize (this_name, case_sensitive) =
           Normalize (name, case_sensitive)
        then
          --  Name found in central directory !
          file_index  := Zip_Streams.ZS_Index_Type (1 + header.local_header_offset) + the_end.offset_shifting;
          comp_size   := Zip_32_Data_Size_Type (header.short_info.dd.compressed_size);
          uncomp_size := Zip_32_Data_Size_Type (header.short_info.dd.uncompressed_size);
          crc_32      := header.short_info.dd.crc_32;
          return;
        end if;
      end;
    end loop;
    raise Entry_name_not_found with "Entry: [" & name & ']';
  exception
    when Zip.Headers.bad_end =>
      raise Zip.Archive_corrupted with "Bad (or no) end-of-central-directory";
    when Zip.Headers.bad_central_header =>
      raise Zip.Archive_corrupted with "Bad central directory entry header";
  end Find_offset;

  --  Internal: find offset of a zipped file using the zip_info tree 8-)

  procedure Find_offset (
    info           : in     Zip_info;
    name           : in     String;
    name_encoding  :    out Zip_name_encoding;
    file_index     :    out Zip_Streams.ZS_Index_Type;
    comp_size      :    out Zip_32_Data_Size_Type;
    uncomp_size    :    out Zip_32_Data_Size_Type;
    crc_32         :    out Interfaces.Unsigned_32
  )
  is
    aux : p_Dir_node := info.dir_binary_tree;
    up_name : constant String := Normalize (name, info.case_sensitive);
  begin
    if not info.loaded then
      raise Forgot_to_load_zip_info;
    end if;
    while aux /= null loop
      if up_name > aux.dico_name then
        aux := aux.right;
      elsif up_name < aux.dico_name then
        aux := aux.left;
      else  -- entry found !
        name_encoding := aux.name_encoding;
        file_index    := aux.file_index;
        comp_size     := aux.comp_size;
        uncomp_size   := aux.uncomp_size;
        crc_32        := aux.crc_32;
        return;
      end if;
    end loop;
    raise Entry_name_not_found with "Archive: [" & info.zip_file_name.all & "], entry: [" & name & ']';
  end Find_offset;

  procedure Find_offset_without_directory (
    info           : in     Zip_info;
    name           : in     String;
    name_encoding  :    out Zip_name_encoding;
    file_index     :    out Zip_Streams.ZS_Index_Type;
    comp_size      :    out Zip_32_Data_Size_Type;
    uncomp_size    :    out Zip_32_Data_Size_Type;
    crc_32         :    out Interfaces.Unsigned_32
  )
  is
    function Trash_dir (n : String) return String is
      idx : Integer := n'First - 1;
    begin
      for i in n'Range loop
        if n (i) in '/' | '\' then
          idx := i;
        end if;
      end loop;
      --  idx points on the index just before the interesting part
      return Normalize (n (idx + 1 .. n'Last), info.case_sensitive);
    end Trash_dir;

    simple_name : constant String := Trash_dir (name);

    Found : exception;

    procedure Check_entry (
      entry_name          : String; -- 'name' is compressed entry's name
      entry_index         : Zip_Streams.ZS_Index_Type;
      entry_comp_size     : Zip_32_Data_Size_Type;
      entry_uncomp_size   : Zip_32_Data_Size_Type;
      entry_crc_32        : Interfaces.Unsigned_32;
      date_time           : Time;
      method              : PKZip_method;
      entry_name_encoding : Zip_name_encoding;
      read_only           : Boolean;
      encrypted_2_x       : Boolean; -- PKZip 2.x encryption
      entry_user_code     : in out Integer
    )
    is
    pragma Unreferenced (date_time, method, read_only, encrypted_2_x, entry_user_code);
    begin
      if Trash_dir (entry_name) = simple_name then
        name_encoding := entry_name_encoding;
        file_index    := entry_index;
        comp_size     := entry_comp_size;
        uncomp_size   := entry_uncomp_size;
        crc_32        := entry_crc_32;
        raise Found;
      end if;
    end Check_entry;
    --
    procedure Search is new Traverse_verbose (Check_entry);
    --
  begin
    begin
      Search (info);
    exception
      when Found =>
        return;
    end;
    raise Entry_name_not_found with "Archive: [" & info.zip_file_name.all & "], entry: [" & name & ']';
  end Find_offset_without_directory;

  function Exists (
    info           : in     Zip_info;
    name           : in     String
  )
  return Boolean
  is
    aux : p_Dir_node := info.dir_binary_tree;
    up_name : constant String := Normalize (name, info.case_sensitive);
  begin
    if not info.loaded then
      raise Forgot_to_load_zip_info;
    end if;
    while aux /= null loop
      if up_name > aux.dico_name then
        aux := aux.right;
      elsif up_name < aux.dico_name then
        aux := aux.left;
      else  --  entry found !
        return True;
      end if;
    end loop;
    return False;
  end Exists;

  procedure Set_user_code (
    info           : in Zip_info;
    name           : in String;
    code           : in Integer
  )
  is
    aux : p_Dir_node := info.dir_binary_tree;
    up_name : constant String := Normalize (name, info.case_sensitive);
  begin
    if not info.loaded then
      raise Forgot_to_load_zip_info;
    end if;
    while aux /= null loop
      if up_name > aux.dico_name then
        aux := aux.right;
      elsif up_name < aux.dico_name then
        aux := aux.left;
      else  --  entry found !
        aux.user_code := code;
        return;
      end if;
    end loop;
    raise Entry_name_not_found with "Archive: [" & info.zip_file_name.all & "], entry: [" & name & ']';
  end Set_user_code;

  function User_code (
    info           : in Zip_info;
    name           : in String
  )
  return Integer
  is
    aux : p_Dir_node := info.dir_binary_tree;
    up_name : constant String := Normalize (name, info.case_sensitive);
  begin
    if not info.loaded then
      raise Forgot_to_load_zip_info;
    end if;
    while aux /= null loop
      if up_name > aux.dico_name then
        aux := aux.right;
      elsif up_name < aux.dico_name then
        aux := aux.left;
      else  --  entry found !
        return aux.user_code;
      end if;
    end loop;
    raise Entry_name_not_found with "Archive: [" & info.zip_file_name.all & "], entry: [" & name & ']';
    return 0;  --  Fake, since exception has been raised just before. Removes an OA warning.
  end User_code;

  procedure Get_sizes (
    info           : in     Zip_info;
    name           : in     String;
    comp_size      :    out Zip_32_Data_Size_Type;
    uncomp_size    :    out Zip_32_Data_Size_Type
  )
  is
    dummy_file_index : Zip_Streams.ZS_Index_Type;
    dummy_name_encoding : Zip_name_encoding;
    dummy_crc_32 : Interfaces.Unsigned_32;
  begin
    Find_offset (
      info, name, dummy_name_encoding, dummy_file_index,
      comp_size, uncomp_size, dummy_crc_32
    );
  end Get_sizes;

  --  Workaround for the severe xxx'Read xxx'Write performance
  --  problems in the GNAT and ObjectAda compilers (as in 2009)
  --  This is possible if and only if Byte = Stream_Element and
  --  arrays types are both packed and aligned the same way.
  --
  subtype Size_test_a is Byte_Buffer (1 .. 19);
  subtype Size_test_b is Ada.Streams.Stream_Element_Array (1 .. 19);
  workaround_possible : constant Boolean :=
    Size_test_a'Size = Size_test_b'Size and
    Size_test_a'Alignment = Size_test_b'Alignment;

  --  Block_Read - general-purpose procedure (nothing really specific
  --  to Zip / UnZip): reads either the whole buffer from a file, or
  --  if the end of the file lays inbetween, a part of the buffer.

  procedure Block_Read (
    file          : in     Ada.Streams.Stream_IO.File_Type;
    buffer        :    out Byte_Buffer;
    actually_read :    out Natural
  )
  is
    use Ada.Streams, Ada.Streams.Stream_IO;
    SE_Buffer   : Stream_Element_Array (1 .. buffer'Length);
    for SE_Buffer'Address use buffer'Address;
    pragma Import (Ada, SE_Buffer);
    Last_Read   : Stream_Element_Offset;
  begin
    if workaround_possible then
      Read (Stream (file).all, SE_Buffer, Last_Read);
      actually_read := Natural (Last_Read);
    else
      if End_Of_File (file) then
        actually_read := 0;
      else
        actually_read :=
          Integer'Min (buffer'Length, Integer (Size (file) - Index (file) + 1));
        Byte_Buffer'Read (
          Stream (file),
          buffer (buffer'First .. buffer'First + actually_read - 1)
        );
      end if;
    end if;
  end Block_Read;

  procedure Block_Read (
    stream        : in out Zip_Streams.Root_Zipstream_Type'Class;
    buffer        :    out Byte_Buffer;
    actually_read :    out Natural
  )
  is
    use Ada.Streams, Zip_Streams;
    SE_Buffer   : Stream_Element_Array (1 .. buffer'Length);
    for SE_Buffer'Address use buffer'Address;
    pragma Import (Ada, SE_Buffer);
    Last_Read   : Stream_Element_Offset;
  begin
    if workaround_possible then
      Read (stream, SE_Buffer, Last_Read);
      actually_read := Natural (Last_Read);
    else
      if End_Of_Stream (stream) then
        actually_read := 0;
      else
        actually_read :=
          Integer'Min (buffer'Length, Integer (Size (stream) - Index (stream) + 1));
        Byte_Buffer'Read (
          stream'Access,
          buffer (buffer'First .. buffer'First + actually_read - 1)
        );
      end if;
    end if;
  end Block_Read;

  procedure Block_Read (
    stream : in out Zip_Streams.Root_Zipstream_Type'Class;
    buffer :    out Byte_Buffer
  )
  is
    actually_read : Natural;
  begin
    Block_Read (stream, buffer, actually_read);
    if actually_read < buffer'Length then
      raise Ada.IO_Exceptions.End_Error;
    end if;
  end Block_Read;

  procedure Block_Write (
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
      Ada.Streams.Write (stream, SE_Buffer);
    else
      Byte_Buffer'Write (stream'Access, buffer);
      --  ^This is 30x to 70x slower on GNAT 2009 !
    end if;
  end Block_Write;

  function Image (m : PKZip_method) return String is
  begin
    case m is
      when store       => return "Store";
      when shrink      => return "Shrink";
      when reduce_1    => return "Reduce 1";
      when reduce_2    => return "Reduce 2";
      when reduce_3    => return "Reduce 3";
      when reduce_4    => return "Reduce 4";
      when implode     => return "Implode";
      when tokenize    => return "Tokenize";
      when deflate     => return "Deflate";
      when deflate_e   => return "Deflate64";
      when bzip2       => return "BZip2";
      when lzma_meth   => return "LZMA";
      when zstandard   => return "Zstandard";
      when mp3_recomp  => return "MP3 recompression";
      when xz_recomp   => return "XZ recompression";
      when jpeg_recomp => return "JPEG recompression";
      when wavpack     => return "WAVE recompression";
      when ppmd        => return "PPMd";
      when unknown     => return "(unknown)";
    end case;
  end Image;

  function Method_from_code (x : Natural) return PKZip_method is
    --  An enumeration clause might be more elegant instead of this function,
    --  but would need curiously an Unchecked_Conversion... (RM 13.4)
    use Compression_format_code;
  begin
    case x is
      when store_code      => return store;
      when shrink_code     => return shrink;
      when reduce_code     => return reduce_1;
      when reduce_code + 1 => return reduce_2;
      when reduce_code + 2 => return reduce_3;
      when reduce_code + 3 => return reduce_4;
      when implode_code    => return implode;
      when tokenize_code   => return tokenize;
      when deflate_code    => return deflate;
      when deflate_e_code  => return deflate_e;
      when bzip2_code      => return bzip2;
      when lzma_code       => return lzma_meth;
      when zstandard_code  => return zstandard;
      when mp3_code        => return mp3_recomp;
      when xz_code         => return xz_recomp;
      when jpeg_code       => return jpeg_recomp;
      when wavpack_code    => return wavpack;
      when ppmd_code       => return ppmd;
      when others          => return unknown;
    end case;
  end Method_from_code;

  function Method_from_code (x : Interfaces.Unsigned_16) return PKZip_method is
  begin
    return Method_from_code (Natural (x));
  end Method_from_code;

  --  Copy a chunk from a stream into another one, using a temporary buffer
  procedure Copy_chunk (
    from        : in out Zip_Streams.Root_Zipstream_Type'Class;
    into        : in out Ada.Streams.Root_Stream_Type'Class;
    bytes       : Natural;
    buffer_size : Positive := 1024 * 1024;
    Feedback    : Feedback_proc := null
  )
  is
    buf : Zip.Byte_Buffer (1 .. buffer_size);
    actually_read, remains : Natural;
    user_abort : Boolean := False;
  begin
    remains := bytes;
    while remains > 0 loop
      if Feedback /= null then
        Feedback (
          100 - Integer (100.0 * Float (remains) / Float (bytes)),
          False,
          user_abort
        );
        --  !! do something if user_abort = True !!
      end if;
      Zip.Block_Read (from, buf (1 .. Integer'Min (remains, buf'Last)), actually_read);
      if actually_read = 0 then -- premature end, unexpected
        raise Zip.Archive_corrupted;
      end if;
      remains := remains - actually_read;
      Zip.Block_Write (into, buf (1 .. actually_read));
    end loop;
  end Copy_chunk;

  --  Copy a whole file into a stream, using a temporary buffer
  procedure Copy_file (
    file_name   : String;
    into        : in out Ada.Streams.Root_Stream_Type'Class;
    buffer_size : Positive := 1024 * 1024
  )
  is
    use Ada.Streams.Stream_IO;
    f : File_Type;
    buf : Zip.Byte_Buffer (1 .. buffer_size);
    actually_read : Natural;
  begin
    Open (f, In_File, file_name);
    loop
      Zip.Block_Read (f, buf, actually_read);
      exit when actually_read = 0; -- this is expected
      Zip.Block_Write (into, buf (1 .. actually_read));
    end loop;
    Close (f);
  end Copy_file;

  --  This does the same as Ada 2005's Ada.Directories.Exists
  --  Just there as helper for Ada 95 only systems
  --
  function Exists (name : String) return Boolean is
    use Ada.Text_IO, Ada.Strings.Fixed;
    f : File_Type;
  begin
    if Index (name, "*") > 0 then
      return False;
    end if;
    Open (f, In_File, name, Form => Ada.Strings.Unbounded.To_String (Zip_Streams.Form_For_IO_Open_and_Create));
    Close (f);
    return True;
  exception
    when Name_Error =>
      return False;  --  The file cannot exist !
    when Use_Error =>
      return True;   --  The file exists and is already opened !
  end Exists;

  procedure Put_Multi_Line (
    out_file :        Ada.Text_IO.File_Type;
    text     :        String
  )
  is
    last_char : Character := ' ';
    c : Character;
  begin
    for i in text'Range loop
      c := text (i);
      case c is
        when ASCII.CR =>
          Ada.Text_IO.New_Line (out_file);
        when ASCII.LF =>
          if last_char /= ASCII.CR then Ada.Text_IO.New_Line (out_file); end if;
        when others =>
          Ada.Text_IO.Put (out_file, c);
      end case;
      last_char := c;
    end loop;
  end Put_Multi_Line;

  procedure Write_as_text (
    out_file  :        Ada.Text_IO.File_Type;
    buffer    :        Byte_Buffer;
    last_char : in out Character  --  track line-ending characters across writes
  )
  is
    c : Character;
  begin
    for i in buffer'Range loop
      c := Character'Val (buffer (i));
      case c is
        when ASCII.CR =>
          Ada.Text_IO.New_Line (out_file);
        when ASCII.LF =>
          if last_char /= ASCII.CR then Ada.Text_IO.New_Line (out_file); end if;
        when others =>
          Ada.Text_IO.Put (out_file, c);
      end case;
      last_char := c;
    end loop;
  end Write_as_text;

  function Hexadecimal (x : Interfaces.Unsigned_32) return String
  is
    package MIO is new Ada.Text_IO.Modular_IO (Interfaces.Unsigned_32);
    str : String (1 .. 12);
    use Ada.Strings.Fixed;
  begin
    MIO.Put (str, x, 16);
    return str (Index (str, "#") + 1 .. 11);
  end Hexadecimal;

  overriding procedure Adjust (info : in out Zip_info) is

    function Tree_Clone (p : in p_Dir_node) return p_Dir_node is
      q : p_Dir_node;
    begin
      if p = null then
        return null;
      else
        q := new Dir_node'(p.all);
        q.left  := Tree_Clone (p.left);
        q.right := Tree_Clone (p.right);
        return q;
      end if;
    end Tree_Clone;

  begin
    info.dir_binary_tree  := Tree_Clone (info.dir_binary_tree);
    info.zip_file_name    := new String'(info.zip_file_name.all);
    info.zip_file_comment := new String'(info.zip_file_comment.all);
  end Adjust;

  overriding procedure Finalize (info : in out Zip_info) is
  begin
    Delete (info);
  end Finalize;

end Zip;
