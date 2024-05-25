--   ________  ___   ______       ______      ___
--  /___..._/  |.|   |.___.\     /. __ .\   __|.|   ____
--     /../    |.|   |.____/     |.|__|.|  /....|  __\..\
--   _/../___  |.|   |.|    ===  |..__..| |. = .| | = ..|
--  /_______/  |_|  /__|        /__|  |_|  \__\_|  \__\_|

--  UnZip
---------
--
--  This library allows to uncompress deflated, enhanced deflated, bzip2-ed, lzma-ed,
--  imploded, reduced, shrunk and stored streams from a Zip archive stream.
--
--  Pure Ada 2005+ code, 100% portable: OS-, CPU- and compiler- independent.
--  Location on the web: see the Zip.web constant.

--  Ada translation and substantial rewriting by Gautier de Montmollin
--  based on Pascal version 2.10 by Abimbola A Olowofoyeku,
--    http://www.foyeh.org/
--  itself based on Pascal version by Christian Ghisler,
--  itself based on C code by Info-Zip group (Mark Adler et al.)
--    http://www.info-zip.org/UnZip.html

--  Technical documentation: read appnote.txt

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

with Zip;

with Ada.Calendar, Ada.Streams, Ada.Strings.Unbounded;

package UnZip is

  type Option is
    (test_only,             --  test .zip file integrity, no write
     junk_directories,      --  ignore directory info -> extract to current one
     case_sensitive_match,  --  case sensitive name matching
     extract_as_text);      --  files will be written with native line endings

  type Option_Set is array (Option) of Boolean;

  no_option : constant Option_Set := (others => False);

  --  Ada 2005's Ada.Directories.Create_Path.
  --  For Ada 95 compatibility we pass it as an optional procedure access.
  type Create_Path_Proc is access
    procedure (New_Directory : in String;
               Form          : in String := "");

  --  This is system-dependent (or in a future Ada)
  type Set_Time_Stamp_Proc is access
    procedure (file_name : String; stamp : Ada.Calendar.Time);

  --  Alternatively, you can use Zip.Time to set file time stamps
  type Set_ZTime_Stamp_Proc is access
    procedure (file_name : String; stamp : Zip.Time);
  --  NB: you can use Zip.Convert to change Ada.Calendar.Time from/to Zip.Time
  --      or use our Split to avoid using Ada.Calendar at all.

  --  This is for modifying output file names (e.g. adding a
  --  work directory, modifying the archived path, etc.)
  type Compose_Func is access function
    (File_Name     : String;
     Name_encoding : Zip.Zip_Name_Encoding)
  return String;

  --  File System dependent settings
  type FS_Routines_Type is record
    Create_Path       : Create_Path_Proc;
    Set_Time_Stamp    : Set_Time_Stamp_Proc;
    Compose_File_Name : Compose_Func;
    Set_ZTime_Stamp   : Set_ZTime_Stamp_Proc;  --  alt. to Set_Time_Stamp
  end record;

  null_routines : constant FS_Routines_Type := (null, null, null, null);

  ----------------------------------
  -- Simple extraction procedures --
  ----------------------------------

  --  Extract all files from an archive (from)

  procedure Extract (from                 : String;
                     options              : Option_Set       := no_option;
                     password             : String           := "";
                     file_system_routines : FS_Routines_Type := null_routines);

  --  Extract one precise file (what) from an archive (from)

  procedure Extract (from                 : String;
                     what                 : String;
                     options              : Option_Set       := no_option;
                     password             : String           := "";
                     file_system_routines : FS_Routines_Type := null_routines);

  --  Extract one precise file (what) from an archive (from),
  --  but save under a new name (rename)

  procedure Extract (from                 : String;
                     what                 : String;
                     rename               : String;
                     options              : Option_Set       := no_option;
                     password             : String           := "";
                     file_system_routines : FS_Routines_Type := null_routines);

  -------------------------------------------------------------------------
  -- Simple extraction procedures without re-searching central directory --
  -------------------------------------------------------------------------

  --  Extract all files from an archive (from)
  --  Needs Zip.Load(from, ...) prior to the extraction

  procedure Extract (from                 : Zip.Zip_Info;
                     options              : Option_Set       := no_option;
                     password             : String           := "";
                     file_system_routines : FS_Routines_Type := null_routines);

  --  Extract one precise file (what) from an archive (from)
  --  Needs Zip.Load(from, ...) prior to the extraction

  procedure Extract (from                 : Zip.Zip_Info;
                     what                 : String;
                     options              : Option_Set       := no_option;
                     password             : String           := "";
                     file_system_routines : FS_Routines_Type := null_routines);

  --  Extract one precise file (what) from an archive (from),
  --  but save under a new name (rename)
  --  Needs Zip.Load(from, ...) prior to the extraction

  procedure Extract (from                 : Zip.Zip_Info;
                     what                 : String;
                     rename               : String;
                     options              : Option_Set       := no_option;
                     password             : String           := "";
                     file_system_routines : FS_Routines_Type := null_routines);

  subtype PKZip_Method is Zip.PKZip_method;
  pragma Obsolescent (PKZip_method, "Better use the type: Zip.PKZip_method");

  ----------------------------------------------
  -- Extraction procedures for user interface --
  ----------------------------------------------

  --  NB: the *_proc types are accesses to procedures - their usage
  --  may require the non-standard attribute "unrestricted_access",
  --  or some changes.
  --  Read unzipada.adb for details and examples.

  type Name_Conflict_Intervention is
    (yes, no, yes_to_all, none, rename_it, abort_now);

  current_user_attitude : Name_Conflict_Intervention := yes;
  --  reset to "yes" for a new session (in case of yes_to_all / none state!)

  type Resolve_Conflict_Proc is access
    procedure (name            :  in String;
               name_encoding   :  in Zip.Zip_Name_Encoding;
               action          : out Name_Conflict_Intervention;
               new_name        : out String;
               new_name_length : out Natural);

  type Get_Password_Proc is access
    procedure (password : out Ada.Strings.Unbounded.Unbounded_String);

  --  Inform user about some archive data

  type Tell_Data_Proc is access
    procedure (name               : String;
               compressed_bytes   : Zip.Zip_64_Data_Size_Type;
               uncompressed_bytes : Zip.Zip_64_Data_Size_Type;
               method             : PKZip_Method);

  --  Extract all files from an archive (from)

  procedure Extract (from                 : String;
                     feedback             : Zip.Feedback_Proc;
                     help_the_file_exists : Resolve_Conflict_Proc;
                     tell_data            : Tell_Data_Proc;
                     get_pwd              : Get_Password_Proc;
                     options              : Option_Set       := no_option;
                     password             : String           := "";
                     file_system_routines : FS_Routines_Type := null_routines);

  --  Extract one precise file (what) from an archive (from)

  procedure Extract (from                 : String;
                     what                 : String;
                     feedback             : Zip.Feedback_Proc;
                     help_the_file_exists : Resolve_Conflict_Proc;
                     tell_data            : Tell_Data_Proc;
                     get_pwd              : Get_Password_Proc;
                     options              : Option_Set       := no_option;
                     password             : String           := "";
                     file_system_routines : FS_Routines_Type := null_routines);

  --  Extract one precise file (what) from an archive (from),
  --  but save under a new name (rename)

  procedure Extract (from                 : String;
                     what                 : String;
                     rename               : String;
                     feedback             : Zip.Feedback_Proc;
                     tell_data            : Tell_Data_Proc;
                     get_pwd              : Get_Password_Proc;
                     options              : Option_Set       := no_option;
                     password             : String           := "";
                     file_system_routines : FS_Routines_Type := null_routines);

  --  Using Zip_info structure:

  --  Extract all files from an archive (from)
  --  Needs Zip.Load(from, ...) prior to the extraction

  procedure Extract (from                 : Zip.Zip_Info;
                     feedback             : Zip.Feedback_Proc;
                     help_the_file_exists : Resolve_Conflict_Proc;
                     tell_data            : Tell_Data_Proc;
                     get_pwd              : Get_Password_Proc;
                     options              : Option_Set       := no_option;
                     password             : String           := "";
                     file_system_routines : FS_Routines_Type := null_routines);

  --  Extract one precise file (what) from an archive (from)
  --  Needs Zip.Load(from, ...) prior to the extraction

  procedure Extract (from                 : Zip.Zip_Info;
                     what                 : String;
                     feedback             : Zip.Feedback_Proc;
                     help_the_file_exists : Resolve_Conflict_Proc;
                     tell_data            : Tell_Data_Proc;
                     get_pwd              : Get_Password_Proc;
                     options              : Option_Set       := no_option;
                     password             : String           := "";
                     file_system_routines : FS_Routines_Type := null_routines);

  --  Extract one precise file (what) from an archive (from),
  --  but save under a new name (rename)
  --  Needs Zip.Load(from, ...) prior to the extraction

  procedure Extract (from                 : Zip.Zip_Info;
                     what                 : String;
                     rename               : String;
                     feedback             : Zip.Feedback_Proc;
                     tell_data            : Tell_Data_Proc;
                     get_pwd              : Get_Password_Proc;
                     options              : Option_Set       := no_option;
                     password             : String           := "";
                     file_system_routines : FS_Routines_Type := null_routines);

  --  Errors

  CRC_Error,
  Uncompressed_Size_Error,
  Write_Error,
  Read_Error,
  Wrong_password,
  User_abort,
  Not_supported,
  Unsupported_method : exception;

  tolerance_wrong_password : constant := 4;
  --  If password is wrong at the Nth attempt, Wrong_password is raised

private

  type Write_Mode_Type is
    (write_to_binary_file,
     write_to_text_file,
     write_to_memory,
     write_to_stream,
     just_test);

  subtype Write_to_file is Write_Mode_Type
    range write_to_binary_file .. write_to_text_file;

  type p_Stream is access all Ada.Streams.Root_Stream_Type'Class;

  type p_Stream_Element_Array is access all Ada.Streams.Stream_Element_Array;

end UnZip;
