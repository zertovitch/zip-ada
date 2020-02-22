--  Contributed by ITEC - NXP Semiconductors
--  June 2008
--
--  The Zip_Streams package defines an abstract stream
--  type, Root_Zipstream_Type, with name, time and an index for random access.
--
--  In addition, this package provides two ready-to-use derivations:
--
--    - Memory_Zipstream, for using in-memory streaming
--    - File_Zipstream, for accessing files
--
--  The Zip_Streams package can be used as such, independently
--  of the Zip-Ada library.
--
--  Pure Ada 95+ code, 100% portable: OS-, CPU- and compiler- independent.

--  Legal licensing note:

--  Copyright (c) 2008 .. 2020 Gautier de Montmollin (maintainer)
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

--  NB: this is the MIT License, as found 21-Aug-2016 on the site
--  http://www.opensource.org/licenses/mit-license.php

--  Change log:
--  ==========
--
--   8-Sep-2018: GdM: ZS_Size_Type is now 64-bit signed, enabling Zip.Create
--                    to capture archive size overflows.
--   5-Jul-2013: GdM: Added proper types for stream sizes and index
--  20-Nov-2012: GdM: Added Is_Open method for File_Zipstream
--  30-Oct-2012: GdM/NB: - Removed method profiles with 'access' as
--                           overriding some methods with 'access' and some without
--                           at different inheritance levels may be dangerous
--                       - renamed Zipstream_Class Zipstream_Class_Access
--                           (the right name for it)
--  25-Oct-2012: GdM: All methods also with pointer-free profiles
--                     (no more anonymous 'access', nor access types needed)
--  20-Jul-2011: GdM/JH: - Underscore in Get_Name, Set_Name, Get_Time, Set_Time
--                       - The 4 methods above are not anymore abstract
--                       - Name and Modification_Time fields moved to Root_Zipstream_Type
--                       - Unbounded_Stream becomes Memory_Zipstream
--                       - ZipFile_Stream becomes File_Zipstream
--  17-Jul-2011: JH : Added Set_Unicode_Name_Flag, Is_Unicode_Name
--  25-Nov-2009: GdM: Added an own time type -> it is possible to bypass Ada.Calendar
--  18-Jan-2009: GdM: Fixed Zip_Streams.Read which did read
--                      only Item's first element

with Ada.Streams;           use Ada.Streams;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Streams.Stream_IO;

with Ada.Calendar, Interfaces;
use Interfaces;

package Zip_Streams is

   --  We define an own Time (Ada.Calendar's body can be very time-consuming!)
   --  See subpackage Calendar below for own Split, Time_Of and Convert from/to
   --  Ada.Calendar.Time.
   type Time is private;

   default_time   : constant Time;  --  some default time
   special_time_1 : constant Time;  --  special time code (for users of Zip_Streams)
   special_time_2 : constant Time;  --  special time code (for users of Zip_Streams)

   ------------------------------------------------------
   --  Root_Zipstream_Type: root abstract stream type  --
   ------------------------------------------------------

   type Root_Zipstream_Type is abstract new Ada.Streams.Root_Stream_Type with private;
   type Zipstream_Class_Access is access all Root_Zipstream_Type'Class;

   subtype ZS_Size_Type is Integer_64 range 0 .. Integer_64'Last;
   subtype ZS_Index_Type is ZS_Size_Type range 1 .. ZS_Size_Type'Last;

   --  Set the index on the stream
   procedure Set_Index (S : in out Root_Zipstream_Type;
                        To : ZS_Index_Type) is abstract;

   --  Returns the index of the stream
   function Index (S : in Root_Zipstream_Type) return ZS_Index_Type is abstract;

   --  Returns the Size of the stream
   function Size (S : in Root_Zipstream_Type) return ZS_Size_Type is abstract;

   --  This procedure sets the name of the stream
   procedure Set_Name (S : in out Root_Zipstream_Type; Name : String);

   --  This procedure returns the name of the stream
   function Get_Name (S : in Root_Zipstream_Type) return String;

   procedure Set_Unicode_Name_Flag (S     : out Root_Zipstream_Type;
                                    Value : in Boolean);
   function Is_Unicode_Name (S : in Root_Zipstream_Type)
                             return Boolean;

   procedure Set_Read_Only_Flag (S     : out Root_Zipstream_Type;
                                 Value : in Boolean);
   function Is_Read_Only (S : in Root_Zipstream_Type)
                          return Boolean;

   --  This procedure sets the Modification_Time of the stream
   procedure Set_Time (S : in out Root_Zipstream_Type;
                       Modification_Time : Time);

   --  Set_Time again, but with the standard Ada Time type.
   --  Overriding is useless and potentially harmful, so we prevent it with
   --  a class-wide profile.
   procedure Set_Time (S : in out Root_Zipstream_Type'Class;
                       Modification_Time : Ada.Calendar.Time);

   --  This procedure returns the ModificationTime of the stream
   function Get_Time (S : in Root_Zipstream_Type)
                      return Time;

   --  Get_Time again, but with the standard Ada Time type.
   --  Overriding is useless and potentially harmful, so we prevent it with
   --  a class-wide profile.
   function Get_Time (S : in Root_Zipstream_Type'Class)
                      return Ada.Calendar.Time;

   --  Returns true if the index is at the end of the stream, else false
   function End_Of_Stream (S : in Root_Zipstream_Type)
      return Boolean is abstract;

   -----------------------------------------------------------------------
   --  Memory_Zipstream: stream based on an in-memory Unbounded_String  --
   -----------------------------------------------------------------------
   type Memory_Zipstream is new Root_Zipstream_Type with private;

   --  Get the complete value (contents) of the stream
   procedure Get (Str : Memory_Zipstream; Unb : out Unbounded_String);

   --  Set a value in the stream, the index will be set
   --  to null and old data in the stream will be lost.
   procedure Set (Str : in out Memory_Zipstream; Unb : Unbounded_String);

   ----------------------------------------------
   --  File_Zipstream: stream based on a file  --
   ----------------------------------------------
   type File_Zipstream is new Root_Zipstream_Type with private;

   type File_Mode is new Ada.Streams.Stream_IO.File_Mode;

   --  Open the File_Zipstream
   --  PRE: Str.Name must be set
   procedure Open (Str : in out File_Zipstream; Mode : File_Mode);

   --  Creates a file on the disk
   --  PRE: Str.Name must be set
   procedure Create (Str : in out File_Zipstream; Mode : File_Mode);

   --  Close the File_Zipstream
   procedure Close (Str : in out File_Zipstream);

   --  Is the File_Zipstream open ?
   function Is_Open (Str : in File_Zipstream) return Boolean;

   ----------------------------
   --  Routines around Time  --
   ----------------------------

   package Calendar is
      --
      function Convert (Date : in Ada.Calendar.Time) return Time;
      function Convert (Date : in Time) return Ada.Calendar.Time;
      --
      subtype DOS_Time is Interfaces.Unsigned_32;
      function Convert (Date : in DOS_Time) return Time;
      function Convert (Date : in Time) return DOS_Time;
      --
      Time_Error : exception;
      --
      use Ada.Calendar;
      --
      procedure Split
        (Date       : Time;
         To_Year    : out Year_Number;
         To_Month   : out Month_Number;
         To_Day     : out Day_Number;
         To_Seconds : out Day_Duration);
      --
      function Time_Of
        (From_Year    : Year_Number;
         From_Month   : Month_Number;
         From_Day     : Day_Number;
         From_Seconds : Day_Duration := 0.0) return Time;
      --
      function ">" (Left, Right : Time) return Boolean;
   end Calendar;

  --  Parameter Form added to *_IO.[Open|Create]
  --  See RM A.8.2: File Management
  --  Example: "encoding=8bits", "encoding=utf8"
  --
  Form_For_IO_Open_and_Create : Ada.Strings.Unbounded.Unbounded_String
    := Ada.Strings.Unbounded.Null_Unbounded_String;

private

   --  Time. Currently, DOS format (pkzip appnote.txt: part V., J.), as stored
   --  in Zip archives. Subject to change, this is why this type is private.
   type Time is new Interfaces.Unsigned_32;

   default_time   : constant Time := 16789 * 65536;
   special_time_1 : constant Time := default_time + 1;
   special_time_2 : constant Time := default_time + 2;

   type Root_Zipstream_Type is abstract new Ada.Streams.Root_Stream_Type with
      record
         Name              : Unbounded_String;
         Modification_Time : Time := default_time;
         Is_Unicode_Name   : Boolean := False;
         Is_Read_Only      : Boolean := False;  --  only indicative
      end record;

   --  Memory_Zipstream spec
   type Memory_Zipstream is new Root_Zipstream_Type with
      record
         Unb : Unbounded_String;
         Loc : Integer := 1;
      end record;
   --  Read data from the stream.
   overriding procedure Read
     (Stream : in out Memory_Zipstream;
      Item   : out Stream_Element_Array;
      Last   : out Stream_Element_Offset);

   --  Write data to the stream, starting from the current index.
   --  Data will be overwritten from index if already available.
   overriding procedure Write
     (Stream : in out Memory_Zipstream;
      Item   : Stream_Element_Array);

   --  Set the index on the stream
   overriding procedure Set_Index (S : in out Memory_Zipstream; To : ZS_Index_Type);

   --  Returns the index of the stream
   overriding function Index (S : in Memory_Zipstream) return ZS_Index_Type;

   --  Returns the Size of the stream
   overriding function Size (S : in Memory_Zipstream) return ZS_Size_Type;

   --  Returns true if the index is at the end of the stream
   overriding function End_Of_Stream (S : in Memory_Zipstream) return Boolean;

   --  File_Zipstream spec
   type File_Zipstream is new Root_Zipstream_Type with
      record
         File : Ada.Streams.Stream_IO.File_Type;
      end record;
   --  Read data from the stream.
   overriding procedure Read
     (Stream : in out File_Zipstream;
      Item   : out Stream_Element_Array;
      Last   : out Stream_Element_Offset);

   --  Write data to the stream, starting from the current index.
   --  Data will be overwritten from index if already available.
   overriding procedure Write
     (Stream : in out File_Zipstream;
      Item   : Stream_Element_Array);

   --  Set the index on the stream
   overriding procedure Set_Index (S : in out File_Zipstream; To : ZS_Index_Type);

   --  Returns the index of the stream
   overriding function Index (S : in File_Zipstream) return ZS_Index_Type;

   --  Returns the Size of the stream
   overriding function Size (S : in File_Zipstream) return ZS_Size_Type;

   --  Returns true if the index is at the end of the stream
   overriding function End_Of_Stream (S : in File_Zipstream) return Boolean;

end Zip_Streams;
