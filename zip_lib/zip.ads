--  ________  ___   ______       ______     ___
-- /___..._/  |.|   |.___.\     /. __ .\  __|.|   ____
--    /../    |.|   |.____/     |.|__|.| /....|  __\..\
--  _/../___  |.|   |.|    ===  |..__..||. = .| | = ..|
-- /_______/  |_|  /__|        /__|  |_| \__\_|  \__\_|

-- Zip library
--------------
-- Library for manipulating archive files in the Zip format
--
-- Pure Ada 95 code, 100% portable: OS-, CPU- and compiler- independent.

-- Legal licensing note:

--  Copyright (c) 1999..2009 Gautier de Montmollin

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

with Zip_Streams;
with Ada.Streams.Stream_IO, Ada.Text_IO;
with Interfaces;

package Zip is

  version   : constant String:= "33";
  reference : constant String:= "18-Jun-2009";
  web       : constant String:= "http://unzip-ada.sf.net/";

  --------------
  -- Zip_info --
  --------------

  -- Zip_info contains the Zip file name or input stream,
  -- and the archive's sorted directory
  type Zip_info is private;

  -----------------------------------------------------------------------
  -- Load the whole .zip directory in archive (from) into a tree, for  --
  -- fast searching                                                    --
  -----------------------------------------------------------------------

  -- from file version

  procedure Load(
    info           : out Zip_info;
    from           : in  String;
    case_sensitive : in  Boolean:= False
  );

  -- from stream version

  procedure Load(
    info           : out Zip_info;
    from           : in  Zip_Streams.Zipstream_Class;
    case_sensitive : in  Boolean:= False
  );

  Zip_file_Error,
  Zip_file_open_Error,
  Duplicate_name: exception;

  function Is_loaded( info: in Zip_info ) return Boolean;

  function Zip_name( info: in Zip_info ) return String;

  function Zip_comment( info: in Zip_info ) return String;

  function Zip_stream( info: in Zip_info ) return Zip_Streams.Zipstream_Class;

  function Entries( info: in Zip_info ) return Natural;

  procedure Delete( info : in out Zip_info );

  Forgot_to_load_zip_info: exception;

  -- Traverse a whole Zip_info directory in sorted order, giving the
  -- name for each entry to an user-defined "Action" procedure.
  generic
    with procedure Action( name: String );
  procedure Traverse( z: Zip_info );

  -- Academic: see how well the name tree is balanced
  procedure Tree_stat(
    z        : in     Zip_info;
    total    :    out Natural;
    max_depth:    out Natural;
    avg_depth:    out Float
  );

  ---------

  -- Data sizes in archive
  subtype File_size_type is Interfaces.Unsigned_32;

  -- Compression methods or formats in the "official" PKWARE Zip format.
  -- Details in appnote.txt, part V.J
  --   C: supported for compressing
  --   D: supported for decompressing

  type PKZip_method is
   ( store,     -- C,D
     shrink,    -- C,D
     reduce_1,  -- C,D
     reduce_2,  -- C,D
     reduce_3,  -- C,D
     reduce_4,  -- C,D
     implode,   --   D
     tokenize,
     deflate,   --   D
     deflate_e, --   D - Enhanced deflate
     bzip2,
     lzma,
     ppmd,
     unknown
   );

  -- Technical: translates the method code as set in zip archives
  function Method_from_code(x: Interfaces.Unsigned_16) return PKZip_method;
  function Method_from_code(x: Natural) return PKZip_method;

  --------------------------------------------------------------------------
  -- Offsets - various procedures giving 1-based indexes to local headers --
  --------------------------------------------------------------------------

  -- Find 1st offset in a Zip stream

  procedure Find_first_offset(
    file           : in     Zip_Streams.Zipstream_Class;
    file_index     :    out Positive );

  -- Find offset of a certain compressed file
  -- in a Zip file (file opened and kept open)

  procedure Find_offset(
    file           : in     Zip_Streams.Zipstream_Class;
    name           : in     String;
    case_sensitive : in     Boolean;
    file_index     :    out Positive;
    comp_size      :    out File_size_type;
    uncomp_size    :    out File_size_type
  );

  -- Find offset of a certain compressed file in a Zip_info data

  procedure Find_offset(
    info           : in     Zip_info;
    name           : in     String;
    case_sensitive : in     Boolean;
    file_index     :    out Ada.Streams.Stream_IO.Positive_Count;
    comp_size      :    out File_size_type;
    uncomp_size    :    out File_size_type
  );

  File_name_not_found: exception;

  procedure Get_sizes(
    info           : in     Zip_info;
    name           : in     String;
    case_sensitive : in     Boolean;
    comp_size      :    out File_size_type;
    uncomp_size    :    out File_size_type
  );

  -- User-defined procedure for feedback occuring during
  -- compression or decompression (entry_skipped meaningful
  -- only for the latter)

  type Feedback_proc is access
    procedure (
      percents_done:  in Natural;  -- %'s completed
      entry_skipped:  in Boolean;  -- indicates one can show "skipped", no %'s
      user_abort   : out Boolean   -- e.g. transmit a "click on Cancel" here
    );

  -------------------------------------------------------------------------
  -- Goodies - things used internally but that might be generally useful --
  -------------------------------------------------------------------------

  -- General-purpose procedure (nothing really specific to Zip / UnZip):
  -- reads either the whole buffer from a file, or if the end of the file
  -- lays inbetween, a part of the buffer.
  --
  -- The procedure's names and parameters match Borland Pascal / Delphi

  subtype Byte is Interfaces.Unsigned_8;
  type Byte_Buffer is array(Integer range <>) of Byte;
  type p_Byte_Buffer is access Byte_Buffer;

  procedure BlockRead(
    file         : in     Ada.Streams.Stream_IO.File_Type;
    buffer       :    out Zip.Byte_Buffer;
    actually_read:    out Natural
  );

  procedure BlockRead(
    file         : in     Zip_Streams.Zipstream_Class;
    buffer       :    out Zip.Byte_Buffer;
    actually_read:    out Natural
  );

  -- This does the same as Ada 2005's Ada.Directories.Exists
  -- Just there as helper for Ada 95 only systems
  --
  function Exists(name:String) return Boolean;

  -- Write a string containing line endings (possible from another system)
  --   into a text file, with the correct native line endings.
  --   Works for displaying/saving correctly
  --   CR&LF (DOS/Win), LF (UNIX), CR (Mac OS < 9)
  --
  procedure Put_Multi_Line(
    out_file :        Ada.Text_IO.File_Type;
    text     :        String
  );

  procedure Write_as_text(
    out_file :        Ada.Text_IO.File_Type;
    buffer   :        Zip.Byte_Buffer;
    last_char: in out Character -- track line-ending characters between writes
  );

  -------------------
  -- Private items --
  -------------------

private
  -- Zip_info, 23.VI.1999.

  -- The PKZIP central directory is coded here as a binary tree
  -- to allow a fast retrieval of the searched offset in zip file.
  -- E.g. for a 1000-file archive, the offset will be found in less
  -- than 11 moves: 2**10=1024 (balanced case), without any read
  -- in the archive.

  type Dir_node;
  type p_Dir_node is access Dir_node;

  type Dir_node(name_len: Natural) is record
    left, right : p_Dir_node;
    name        : String(1..name_len);
    file_index  : Ada.Streams.Stream_IO.Positive_Count;
    comp_size   : File_size_type;
    uncomp_size : File_size_type;
  end record;

  type p_String is access String;

  type Zip_info is record
    loaded          : Boolean:= False;
    zip_file_name   : p_String;        -- a file name...
    zip_input_stream: Zip_Streams.Zipstream_Class; -- ...or an input stream
    -- ^ when not null, we use this and not zip_file_name
    dir_binary_tree : p_Dir_node;
    total_entries   : Natural;
    zip_file_comment: p_String;
  end record;

end Zip;
