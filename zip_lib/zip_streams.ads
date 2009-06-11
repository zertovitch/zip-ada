-- Contributed by ITEC - NXP Semiconductors
-- June 2008
--
-- Change log:
-- ==========
-- 18-Jan-2009: GdM: Fixed Zip_Streams.Read which did read
--                     only Item's first element

with Ada.Calendar;          use Ada.Calendar;
with Ada.Streams;           use Ada.Streams;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Streams.Stream_IO; use Ada.Streams.Stream_IO;

package Zip_Streams is

  type Root_Zipstream_Type is abstract new Ada.Streams.Root_Stream_Type with null record;
  type Zipstream_Class is access all Root_Zipstream_Type'Class;

   -- Set the index on the stream
   procedure Set_Index (S : access Root_Zipstream_Type;
                        To : Positive) is abstract;

   -- returns the index of the stream
   function Index (S : access Root_Zipstream_Type) return Integer is abstract;

   -- returns the Size of the stream
   function Size (S : access Root_Zipstream_Type) return Integer is abstract;

   -- this procedure sets the name of the stream
   procedure SetName(S : access Root_Zipstream_Type;
                     Name : String) is abstract;

   -- this procedure returns the name of the stream
   function GetName(S : access Root_Zipstream_Type)
                    return String is abstract;

   -- this procedure sets the ModificationTime of the stream
   procedure SetTime(S : access Root_Zipstream_Type;
                     ModificationTime : Time) is abstract;

   -- this procedure returns the ModificationTime of the stream
   function GetTime(S : access Root_Zipstream_Type)
                    return Time is abstract;

   -- returns true if the index is at the end of the stream, else false
   function End_Of_Stream (S : access Root_Zipstream_Type)
      return Boolean is abstract;

   ---------------------------------------------------------------------
   -- Unbounded_Stream: stream based on an in-memory Unbounded_String --
   ---------------------------------------------------------------------
   type Unbounded_Stream is new Root_Zipstream_Type with private;

   -- Get the complete value of the stream
   procedure Get (Str : Unbounded_Stream; Unb : out Unbounded_String);

   -- Set a value in the stream, the index will be set
   -- to null and old data in the stream will be lost.
   procedure Set (Str : in out Unbounded_Stream; Unb : Unbounded_String);

   --------------------------------------------
   -- ZipFile_Stream: stream based on a file --
   --------------------------------------------
   type ZipFile_Stream is new Root_Zipstream_Type with private;

   -- Open the ZipFile_Stream
   -- PRE: Str.Name must be set
   procedure Open (Str : in out ZipFile_Stream; Mode : File_Mode);

   -- Creates a file on the disk
   -- PRE: Str.Name must be set
   procedure Create (Str : in out ZipFile_Stream; Mode : File_Mode);

   -- Close the ZipFile_Stream
   procedure Close (Str : in out ZipFile_Stream);

private
   -- Unbounded Stream spec
   type Unbounded_Stream is new Root_Zipstream_Type with
      record
         Unb : Unbounded_String;
         Loc : Integer := 1;
         Name : Unbounded_String;
         ModificationTime : Time := Ada.Calendar.Clock;
      end record;
   -- Read data from the stream.
   procedure Read
     (Stream : in out Unbounded_Stream;
      Item   : out Stream_Element_Array;
      Last   : out Stream_Element_Offset);

   -- write data to the stream, starting from the current index.
   -- Data will be overwritten from index is already available.
   procedure Write
     (Stream : in out Unbounded_Stream;
      Item   : Stream_Element_Array);

   -- Set the index on the stream
   procedure Set_Index (S : access Unbounded_Stream; To : Positive);

   -- returns the index of the stream
   function Index (S : access Unbounded_Stream) return Integer;

   -- returns the Size of the stream
   function Size (S : access Unbounded_Stream) return Integer;

   -- sets the name of the stream
   procedure SetName (S : access Unbounded_Stream; Name : String);

   -- returns the name of the stream
   function GetName(S : access Unbounded_Stream) return String;

      -- this procedure sets the ModificationTime of the stream
   procedure SetTime(S : access Unbounded_Stream;
                     ModificationTime : Time);

   -- this procedure returns the ModificationTime of the stream
   function GetTime(S : access Unbounded_Stream) return Time;

   -- returns true if the index is at the end of the stream
   function End_Of_Stream (S : access Unbounded_Stream) return Boolean;


   -- ZipFile_Stream spec
   type ZipFile_Stream is new Root_Zipstream_Type with
      record
         File : File_Type;
         Name : Unbounded_String;
         ModificationTime : Time := Ada.Calendar.Clock;
      end record;
   -- Read data from the stream.
   procedure Read
     (Stream : in out ZipFile_Stream;
      Item   : out Stream_Element_Array;
      Last   : out Stream_Element_Offset);

   -- write data to the stream, starting from the current index.
   -- Data will be overwritten from index is already available.
   procedure Write
     (Stream : in out ZipFile_Stream;
      Item   : Stream_Element_Array);

   -- Set the index on the stream
   procedure Set_Index (S : access ZipFile_Stream; To : Positive);

   -- returns the index of the stream
   function Index (S : access ZipFile_Stream) return Integer;

   -- returns the Size of the stream
   function Size (S : access ZipFile_Stream) return Integer;

   -- sets the name of the stream
   procedure SetName (S : access ZipFile_Stream; Name : String);

   -- returns the name of the stream
   function GetName(S : access ZipFile_Stream) return String;

      -- this procedure sets the ModificationTime of the stream
   procedure SetTime(S : access ZipFile_Stream;
                     ModificationTime : Time);

   -- this procedure returns the ModificationTime of the stream
   function GetTime(S : access ZipFile_Stream) return Time;

   -- returns true if the index is at the end of the stream
   function End_Of_Stream (S : access ZipFile_Stream) return Boolean;

end Zip_Streams;
