-- Contributed by ITEC - NXP Semiconductors
-- June 2008
--
-- The Zip_Streams package defines an abstract stream
-- type, Root_Zipstream_Type, with name, time and an index for random access.
-- In addition, this package provides two ready-to-use derivations:
--
--   - Memory_Zipstream, for using in-memory streaming
--
--   - File_Zipstream, for accessing files
--
-- Change log:
-- ==========
--
-- 20-Jul-2011: GdM/JH: - Underscore in Get_Name, Set_Name, Get_Time, Set_Time
--                      - The 4 methods above are not anymore abstract
--                      - Name and Modification_Time fields moved to Root_Zipstream_Type
--                      - Unbounded_Stream becomes Memory_Zipstream
--                      - ZipFile_Stream becomes File_Zipstream
-- 17-Jul-2011: JH : Added Set_Unicode_Name_Flag, Is_Unicode_Name
-- 25-Nov-2009: GdM: Added an own time type -> it is possible to bypass Ada.Calendar
-- 18-Jan-2009: GdM: Fixed Zip_Streams.Read which did read
--                     only Item's first element

with Ada.Streams;           use Ada.Streams;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Streams.Stream_IO; use Ada.Streams.Stream_IO;

with Ada.Calendar, Interfaces;

package Zip_Streams is

   type Time is private;
   -- ^ we define an own Time (Ada.Calendar's body can be very time-consuming!)
   -- See subpackage Calendar below for own Split, Time_Of and Convert from/to
   -- Ada.Calendar.Time.

   ----------------------------------------------------
   -- Root_Zipstream_Type: root abstract stream type --
   ----------------------------------------------------

   type Root_Zipstream_Type is abstract new Ada.Streams.Root_Stream_Type with private;
   type Zipstream_Class is access all Root_Zipstream_Type'Class;

   -- Set the index on the stream
   procedure Set_Index (S : access Root_Zipstream_Type;
                        To : Positive) is abstract;

   -- returns the index of the stream
   function Index (S : access Root_Zipstream_Type) return Integer is abstract;

   -- returns the Size of the stream
   function Size (S : access Root_Zipstream_Type) return Integer is abstract;

   -- this procedure sets the name of the stream
   procedure Set_Name(S : access Root_Zipstream_Type; Name : String);

   procedure SetName(S : access Root_Zipstream_Type; Name : String) renames Set_Name;
   pragma Obsolescent (SetName);

   -- this procedure returns the name of the stream
   function Get_Name(S : access Root_Zipstream_Type) return String;

   function GetName(S : access Root_Zipstream_Type) return String renames Get_Name;
   pragma Obsolescent (GetName);

   procedure Set_Unicode_Name_Flag (S     : access Root_Zipstream_Type;
                                    Value : in Boolean);
   function Is_Unicode_Name(S : access Root_Zipstream_Type)
                            return Boolean;

   -- this procedure sets the Modification_Time of the stream
   procedure Set_Time(S : access Root_Zipstream_Type;
                      Modification_Time : Time);

   procedure SetTime(S : access Root_Zipstream_Type;
                      Modification_Time : Time) renames Set_Time;
   pragma Obsolescent (SetTime);

   -- same, with the standard Time type
   procedure Set_Time(S : Zipstream_Class;
                      Modification_Time : Ada.Calendar.Time);

   procedure SetTime(S : Zipstream_Class;
                      Modification_Time : Ada.Calendar.Time) renames Set_Time;
   pragma Obsolescent (SetTime);

   -- this procedure returns the ModificationTime of the stream
   function Get_Time(S : access Root_Zipstream_Type)
                     return Time;

   function GetTime(S : access Root_Zipstream_Type)
                    return Time renames Get_Time;
   pragma Obsolescent (GetTime);

   -- same, with the standard Time type
   function Get_Time(S : Zipstream_Class)
                     return Ada.Calendar.Time;

   function GetTime(S : Zipstream_Class)
                    return Ada.Calendar.Time renames Get_Time;
   pragma Obsolescent (GetTime);

   -- returns true if the index is at the end of the stream, else false
   function End_Of_Stream (S : access Root_Zipstream_Type)
      return Boolean is abstract;

   ---------------------------------------------------------------------
   -- Unbounded_Stream: stream based on an in-memory Unbounded_String --
   ---------------------------------------------------------------------
   type Memory_Zipstream is new Root_Zipstream_Type with private;
   subtype Unbounded_Stream is Memory_Zipstream;
   pragma Obsolescent (Unbounded_Stream);

   -- Get the complete value of the stream
   procedure Get (Str : Memory_Zipstream; Unb : out Unbounded_String);

   -- Set a value in the stream, the index will be set
   -- to null and old data in the stream will be lost.
   procedure Set (Str : in out Memory_Zipstream; Unb : Unbounded_String);

   --------------------------------------------
   -- File_Zipstream: stream based on a file --
   --------------------------------------------
   type File_Zipstream is new Root_Zipstream_Type with private;
   subtype ZipFile_Stream is File_Zipstream;
   pragma Obsolescent (ZipFile_Stream);

   -- Open the File_Zipstream
   -- PRE: Str.Name must be set
   procedure Open (Str : in out File_Zipstream; Mode : File_Mode);

   -- Creates a file on the disk
   -- PRE: Str.Name must be set
   procedure Create (Str : in out File_Zipstream; Mode : File_Mode);

   -- Close the File_Zipstream
   procedure Close (Str : in out File_Zipstream);

   --------------------------
   -- Routines around Time --
   --------------------------

   package Calendar is
      --
      function Convert(date : in Ada.Calendar.Time) return Time;
      function Convert(date : in Time) return Ada.Calendar.Time;
      --
      subtype DOS_Time is Interfaces.Unsigned_32;
      function Convert(date : in DOS_Time) return Time;
      function Convert(date : in Time) return DOS_Time;
      --
      use Ada.Calendar;
      --
      procedure Split
        (Date    : Time;
         Year    : out Year_Number;
         Month   : out Month_Number;
         Day     : out Day_Number;
         Seconds : out Day_Duration);
      --
      function Time_Of
        (Year    : Year_Number;
         Month   : Month_Number;
         Day     : Day_Number;
         Seconds : Day_Duration := 0.0) return Time;
      --
   end Calendar;

private

   type Time is new Interfaces.Unsigned_32;
   -- Currently: DOS format (pkzip appnote.txt: part V., J.), as stored
   -- in zip archives. Subject to change, this is why this type is private.

   some_time: constant Time:= 16789 * 65536;

   type Root_Zipstream_Type is abstract new Ada.Streams.Root_Stream_Type with
      record
         Name              : Unbounded_String;
         Modification_Time : Time := some_time;
         Is_Unicode_Name   : Boolean := False;
      end record;

   -- Memory_Zipstream spec
   type Memory_Zipstream is new Root_Zipstream_Type with
      record
         Unb : Unbounded_String;
         Loc : Integer := 1;
      end record;
   -- Read data from the stream.
   procedure Read
     (Stream : in out Memory_Zipstream;
      Item   : out Stream_Element_Array;
      Last   : out Stream_Element_Offset);

   -- write data to the stream, starting from the current index.
   -- Data will be overwritten from index is already available.
   procedure Write
     (Stream : in out Memory_Zipstream;
      Item   : Stream_Element_Array);

   -- Set the index on the stream
   procedure Set_Index (S : access Memory_Zipstream; To : Positive);

   -- returns the index of the stream
   function Index (S : access Memory_Zipstream) return Integer;

   -- returns the Size of the stream
   function Size (S : access Memory_Zipstream) return Integer;

   -- returns true if the index is at the end of the stream
   function End_Of_Stream (S : access Memory_Zipstream) return Boolean;


   -- File_Zipstream spec
   type File_Zipstream is new Root_Zipstream_Type with
      record
         File : File_Type;
      end record;
   -- Read data from the stream.
   procedure Read
     (Stream : in out File_Zipstream;
      Item   : out Stream_Element_Array;
      Last   : out Stream_Element_Offset);

   -- write data to the stream, starting from the current index.
   -- Data will be overwritten from index is already available.
   procedure Write
     (Stream : in out File_Zipstream;
      Item   : Stream_Element_Array);

   -- Set the index on the stream
   procedure Set_Index (S : access File_Zipstream; To : Positive);

   -- returns the index of the stream
   function Index (S : access File_Zipstream) return Integer;

   -- returns the Size of the stream
   function Size (S : access File_Zipstream) return Integer;

   -- returns true if the index is at the end of the stream
   function End_Of_Stream (S : access File_Zipstream) return Boolean;

end Zip_Streams;
