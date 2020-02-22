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

----------------
--  Some changes
--
--  11-Nov-2009 (GdM): Unbounded_Stream.Write and .Set_Index are buffered
--  18-Jan-2009 (GdM): Fixed Read(Stream, Item...) which read
--                       only 1st element of Item

package body Zip_Streams is

   procedure Set_Name (S : in out Root_Zipstream_Type; Name : String) is
   begin
      S.Name := To_Unbounded_String (Name);
   end Set_Name;

   function Get_Name (S : in Root_Zipstream_Type) return String is
   begin
      return To_String (S.Name);
   end Get_Name;

   procedure Set_Time (S : in out Root_Zipstream_Type; Modification_Time : Time) is
   begin
      S.Modification_Time := Modification_Time;
   end Set_Time;

   function Get_Time (S : in Root_Zipstream_Type) return Time is
   begin
      return S.Modification_Time;
   end Get_Time;

   --  Ada.Calendar versions

   procedure Set_Time (S : in out Root_Zipstream_Type'Class;
                       Modification_Time : Ada.Calendar.Time) is
   begin
     Set_Time (S, Calendar.Convert (Modification_Time));
   end Set_Time;

   function Get_Time (S : in Root_Zipstream_Type'Class)
                      return Ada.Calendar.Time is
   begin
     return Calendar.Convert (Get_Time (S));
   end Get_Time;

   procedure Set_Unicode_Name_Flag (S     : out Root_Zipstream_Type;
                                    Value : in Boolean)
   is
   begin
     S.Is_Unicode_Name := Value;
   end Set_Unicode_Name_Flag;

   function Is_Unicode_Name (S : in Root_Zipstream_Type)
                             return Boolean
   is
   begin
     return S.Is_Unicode_Name;
   end Is_Unicode_Name;

   procedure Set_Read_Only_Flag (S     : out Root_Zipstream_Type;
                                 Value : in Boolean)
   is
   begin
     S.Is_Read_Only := Value;
   end Set_Read_Only_Flag;

   function Is_Read_Only (S : in Root_Zipstream_Type)
                          return Boolean
   is
   begin
     return S.Is_Read_Only;
   end Is_Read_Only;

   ---------------------------------------------------------------------
   -- Unbounded_Stream: stream based on an in-memory Unbounded_String --
   ---------------------------------------------------------------------
   procedure Get (Str : Memory_Zipstream; Unb : out Unbounded_String) is
   begin
      Unb := Str.Unb;
   end Get;

   procedure Set (Str : in out Memory_Zipstream; Unb : Unbounded_String) is
   begin
      Str.Unb := Null_Unbounded_String; -- clear the content of the stream
      Str.Unb := Unb;
      Str.Loc := 1;
   end Set;

   overriding procedure Read
     (Stream : in out Memory_Zipstream;
      Item   : out Stream_Element_Array;
      Last   : out Stream_Element_Offset) is
   begin
      --  Item is read from the stream. If (and only if) the stream is
      --  exhausted, Last will be < Item'Last. In that case, T'Read will
      --  raise an End_Error exception.
      --
      --  Cf: RM 13.13.1(8), RM 13.13.1(11), RM 13.13.2(37) and
      --  explanations by Tucker Taft
      --
      Last := Item'First - 1;
      --  if Item is empty, the following loop is skipped; if Stream.Loc
      --  is already indexing out of Stream.Unb, that value is also appropriate
      for i in Item'Range loop
         Item (i) := Character'Pos (Element (Stream.Unb, Stream.Loc));
         Stream.Loc := Stream.Loc + 1;
         Last := i;
      end loop;
   exception
      when Ada.Strings.Index_Error =>
         null; -- what could be read has been read; T'Read will raise End_Error
   end Read;

   max_chunk_size : constant := 16 * 1024;

   overriding procedure Write
     (Stream : in out Memory_Zipstream;
      Item   : Stream_Element_Array)
   is
     I : Stream_Element_Offset := Item'First;
     chunk_size : Integer;
     tmp : String (1 .. max_chunk_size);
   begin
     while I <= Item'Last loop
       chunk_size := Integer'Min (Integer (Item'Last - I + 1), max_chunk_size);
       if Stream.Loc > Length (Stream.Unb) then
         --  ...we are off the string's bounds, we need to extend it.
         for J in 1 .. chunk_size loop
           tmp (J) := Character'Val (Item (I));
           I := I + 1;
         end loop;
         Append (Stream.Unb, tmp (1 .. chunk_size));
       else
         --  ...we can work (at least for a part) within the string's bounds.
         chunk_size := Integer'Min (chunk_size, Length (Stream.Unb) - Stream.Loc + 1);
         for J in 0 .. chunk_size - 1 loop
           Replace_Element (Stream.Unb, Stream.Loc + J, Character'Val (Item (I)));
           --  GNAT 2008's Replace_Slice does something very general
           --  even in the trivial case where one can make:
           --  Source.Reference(Low..High):= By;
           --  -> still faster with elem by elem replacement
           --  Anyway, this place is not critical for zipping: only the
           --  local header before compressed data is rewritten after
           --  compression. So usually, we are off bounds.
           I := I + 1;
         end loop;
       end if;
       Stream.Loc := Stream.Loc + chunk_size;
     end loop;
   end Write;

   overriding procedure Set_Index (S : in out Memory_Zipstream; To : ZS_Index_Type) is
     I, chunk_size : ZS_Size_Type;
   begin
     if To > ZS_Size_Type (Length (S.Unb)) then
       --  ...we are off the string's bounds, we need to extend it.
       I := ZS_Size_Type (Length (S.Unb)) + 1;
       while I <= To loop
         chunk_size := ZS_Size_Type'Min (To - I + 1, ZS_Size_Type (max_chunk_size));
         Append (S.Unb, (1 .. Integer (chunk_size) => ASCII.NUL));
         I := I + chunk_size;
       end loop;
     end if;
     S.Loc := Integer (To);
   end Set_Index;

   overriding function Size (S : in Memory_Zipstream) return ZS_Size_Type is
   begin
      return ZS_Size_Type (Length (S.Unb));
   end Size;

   overriding function Index (S : in Memory_Zipstream) return ZS_Index_Type is
   begin
      return ZS_Index_Type (S.Loc);
   end Index;

   overriding function End_Of_Stream (S : in Memory_Zipstream) return Boolean is
   begin
      if Size (S) < Index (S) then
         return True;
      else
         return False;
      end if;
   end End_Of_Stream;

   --------------------------------------------
   -- File_Zipstream: stream based on a file --
   --------------------------------------------
   procedure Open (Str : in out File_Zipstream; Mode : File_Mode) is
   begin
      Ada.Streams.Stream_IO.Open (
        Str.File,
        Ada.Streams.Stream_IO.File_Mode (Mode),
        To_String (Str.Name),
        Form => To_String (Form_For_IO_Open_and_Create)
      );
      --  NB: we could have here a call to Set_Time using
      --  Ada.Directories.Modification_Time if the latter
      --  was able to accept the Form as above for Open
      --  (this is needed for Unicode names).
   end Open;

   procedure Create (Str : in out File_Zipstream; Mode : File_Mode) is
   begin
      Ada.Streams.Stream_IO.Create (
        Str.File,
        Ada.Streams.Stream_IO.File_Mode (Mode),
        To_String (Str.Name),
        Form => To_String (Form_For_IO_Open_and_Create)
      );
      --  NB: we could have here a call to Set_Time using
      --  Ada.Directories.Modification_Time if the latter
      --  was able to accept the Form as above for Create
      --  (this is needed for Unicode names).
   end Create;

   procedure Close (Str : in out File_Zipstream) is
   begin
      Ada.Streams.Stream_IO.Close (Str.File);
   end Close;

   function Is_Open (Str : in File_Zipstream) return Boolean is
   begin
      return Ada.Streams.Stream_IO.Is_Open (Str.File);
   end Is_Open;

   overriding procedure Read
     (Stream : in out File_Zipstream;
      Item   : out Stream_Element_Array;
      Last   : out Stream_Element_Offset)
   is
   begin
      Ada.Streams.Stream_IO.Read (Stream.File, Item, Last);
   end Read;

   overriding procedure Write
     (Stream : in out File_Zipstream;
      Item   : Stream_Element_Array) is
   begin
      Ada.Streams.Stream_IO.Write (Stream.File, Item);
   end Write;

   overriding procedure Set_Index (S : in out File_Zipstream; To : ZS_Index_Type) is
   begin
      Ada.Streams.Stream_IO.Set_Index (
        S.File,
        Ada.Streams.Stream_IO.Positive_Count (To)
      );
   end Set_Index;

   overriding function Size (S : in File_Zipstream) return ZS_Size_Type is
   begin
      return ZS_Size_Type (Ada.Streams.Stream_IO.Size (S.File));
   end Size;

   overriding function Index (S : in File_Zipstream) return ZS_Index_Type is
   begin
      return ZS_Index_Type (Ada.Streams.Stream_IO.Index (S.File));
   end Index;

   overriding function End_Of_Stream (S : in File_Zipstream) return Boolean is
   begin
      return Ada.Streams.Stream_IO.End_Of_File (S.File);
   end End_Of_Stream;

   package body Calendar is

      -----------------------------------------------
      -- Time = DOS Time. Valid through Year 2107. --
      -----------------------------------------------

      procedure Split
        (Date       : Time;
         To_Year    : out Year_Number;
         To_Month   : out Month_Number;
         To_Day     : out Day_Number;
         To_Seconds : out Day_Duration)
      is
         d_date : constant Integer := Integer (Date  /  65536);
         d_time : constant Integer := Integer (Date and 65535);
         x            : Integer;
         hours        : Integer;
         minutes      : Integer;
         seconds_only : Integer;
      begin
         To_Year := 1980 + d_date / 512;
         x := (d_date / 32) mod 16;
         if x not in Month_Number then  --  that is 0, or in 13..15
           raise Time_Error;
         end if;
         To_Month := x;
         x := d_date mod 32;
         if x not in Day_Number then  --  that is 0
           raise Time_Error;
         end if;
         To_Day := x;
         hours   := d_time / 2048;
         minutes := (d_time / 32) mod 64;
         seconds_only := 2 * (d_time mod 32);
         if hours not in 0 .. 23 or
           minutes not in 0 .. 59 or
           seconds_only not in 0 .. 59
         then
           raise Time_Error;
         end if;
         To_Seconds := Day_Duration (hours * 3600 + minutes * 60 + seconds_only);
      end Split;
      --
      function Time_Of
        (From_Year    : Year_Number;
         From_Month   : Month_Number;
         From_Day     : Day_Number;
         From_Seconds : Day_Duration := 0.0) return Time
      is
         year_2          : Integer := From_Year;
         hours           : Unsigned_32;
         minutes         : Unsigned_32;
         seconds_only    : Unsigned_32;
         seconds_day     : Unsigned_32;
         result : Unsigned_32;
      begin
         if year_2 < 1980 then  --  avoid invalid DOS date
           year_2 := 1980;
         end if;
         seconds_day := Unsigned_32 (From_Seconds);
         hours := seconds_day / 3600;
         minutes :=  (seconds_day / 60) mod 60;
         seconds_only := seconds_day mod 60;
         result :=
           --  MSDN formula for encoding:
             Unsigned_32 ((year_2 - 1980) * 512 + From_Month * 32 + From_Day) * 65536  --  Date
           +
             hours * 2048 + minutes * 32 + seconds_only / 2;  --  Time
         return Time (result);
      end Time_Of;

      function ">"  (Left, Right : Time) return Boolean is
      begin
        return Unsigned_32 (Left) > Unsigned_32 (Right);
      end ">";

      function Convert (Date : in Ada.Calendar.Time) return Time is
         year_temp       : Year_Number;
         month_temp      : Month_Number;
         day_temp        : Day_Number;
         seconds_day_dur : Day_Duration;
      begin
         Split (Date, year_temp, month_temp, day_temp, seconds_day_dur);
         return Time_Of (year_temp, month_temp, day_temp, seconds_day_dur);
      end Convert;

      function Convert (Date : in Time) return Ada.Calendar.Time is
         year_temp       : Year_Number;
         month_temp      : Month_Number;
         day_temp        : Day_Number;
         seconds_day_dur : Day_Duration;
      begin
         Split (Date, year_temp, month_temp, day_temp, seconds_day_dur);
         return Time_Of (year_temp, month_temp, day_temp, seconds_day_dur);
      end Convert;

      function Convert (Date : in DOS_Time) return Time is
      begin
         return Time (Date);      --  currently a trivial conversion
      end Convert;

      function Convert (Date : in Time) return DOS_Time is
      begin
         return DOS_Time (Date);  --  currently a trivial conversion
      end Convert;

   end Calendar;

end Zip_Streams;
