-- Changes
--
-- 18-Jan-2009 (GdM): Fixed Read(Stream, Item...) which read
--                      only 1st element of Item

--with Ada.Strings.Unbounded.Text_IO; use Ada.Strings.Unbounded.Text_IO;
--with Ada.Text_IO; use Ada.Text_IO;
with Zip;
package body Zip_Streams is

   ---------------------------------------------------------------------
   -- Unbounded_Stream: stream based on an in-memory Unbounded_String --
   ---------------------------------------------------------------------
   procedure Get (Str : Unbounded_Stream; Unb : out Unbounded_String) is
   begin
      Unb := Str.Unb;
   end Get;

   procedure Set (Str : in out Unbounded_Stream; Unb : Unbounded_String) is
   begin
      Str.Unb := Null_Unbounded_String; -- clear the content of the stream
      Str.Unb := Unb;
      Str.Loc := 1;
   end Set;

   procedure Read
     (Stream : in out Unbounded_Stream;
      Item   : out Stream_Element_Array;
      Last   : out Stream_Element_Offset) is
   begin
      -- Item is read from the stream. If (and only if) the stream is
      -- exhausted, Last will be < Item'Last. In that case, T'Read will
      -- raise an End_Error exception.
      --
      -- Cf: RM 13.13.1(8), RM 13.13.1(11), RM 13.13.2(37) and
      -- explanations by Tucker Taft
      --
      Last:= Item'First - 1;
      -- if Item is empty, the following loop is skipped; if Stream.Loc
      -- is already indexing out of Stream.Unb, that value is also appropriate
      for i in Item'Range loop
         Item(i) := Character'Pos (Element(Stream.Unb, Stream.Loc));
         Stream.Loc := Stream.Loc + 1;
         Last := i;
      end loop;
   exception
      when Ada.Strings.Index_Error =>
         null; -- what could be read has been read; T'Read will raise End_Error
   end Read;

   procedure Write
     (Stream : in out Unbounded_Stream;
      Item   : Stream_Element_Array) is
   begin
      for I in Item'Range loop
         if Length(Stream.Unb) < Stream.Loc then
            Append(Stream.Unb, Character'Val(Item(I)));
         else
            Replace_Element(Stream.Unb, Stream.Loc, Character'Val(Item(I)));
         end if;
         Stream.Loc := Stream.Loc + 1;
      end loop;
   end Write;

   procedure Set_Index (S : access Unbounded_Stream; To : Positive) is
   begin
      if Length(S.Unb) < To then
         for I in Length(S.Unb) .. To loop
            Append(S.Unb, ASCII.NUL);
         end loop;
      end if;
     S.Loc := To;
   end Set_Index;

   function Size (S : access Unbounded_Stream) return Integer is
   begin
      return Length(S.Unb);
   end Size;

   function Index (S : access Unbounded_Stream) return Integer is
   begin
      return S.Loc;
   end Index;

   procedure SetName (S: access Unbounded_Stream; Name: String) is
   begin
      S.Name := To_Unbounded_String(Name);
   end SetName;

   function GetName (S: access Unbounded_Stream) return String is
   begin
      return To_String(S.Name);
   end GetName;

   procedure SetTime (S: access Unbounded_Stream; ModificationTime: Time) is
   begin
      S.ModificationTime := ModificationTime;
   end SetTime;

   function GetTime (S: access Unbounded_Stream) return Time is
   begin
      return S.ModificationTime;
   end GetTime;

   function End_Of_Stream (S : access Unbounded_Stream) return Boolean is
   begin
      if Size(S) < Index(S) then
         return True;
      else
         return False;
      end if;
   end End_Of_Stream;


   --------------------------------------------
   -- ZipFile_Stream: stream based on a file --
   --------------------------------------------
   procedure Open (Str : in out ZipFile_Stream; Mode : File_Mode) is
   begin
      Ada.Streams.Stream_IO.Open(Str.File, Mode, To_String(Str.Name),
                                 Form => To_String (Zip.Form_For_IO_Open_N_Create));
   end Open;

   procedure Create (Str : in out ZipFile_Stream; Mode : File_Mode) is
   begin
      Ada.Streams.Stream_IO.Create(Str.File, Mode, To_String (Str.Name),
                                 Form => To_String (Zip.Form_For_IO_Open_N_Create));
   end Create;

   procedure Close (Str : in out ZipFile_Stream) is
   begin
      Ada.Streams.Stream_IO.Close(Str.File);
   end Close;

   procedure Read
     (Stream : in out ZipFile_Stream;
      Item   : out Stream_Element_Array;
      Last   : out Stream_Element_Offset)
   is
   begin
      Ada.Streams.Stream_IO.Read (Stream.File, Item, Last);
   end Read;

   procedure Write
     (Stream : in out ZipFile_Stream;
      Item   : Stream_Element_Array) is
   begin
      Ada.Streams.Stream_IO.Write( Stream.File, Item);
   end Write;

   procedure Set_Index (S : access ZipFile_Stream; To : Positive) is
   begin
      Ada.Streams.Stream_IO.Set_Index ( S.File, Positive_Count(To));
   end Set_Index;

   function Size (S : access ZipFile_Stream) return Integer is
   begin
      return Integer (Ada.Streams.Stream_IO.Size(S.File));
   end Size;

   function Index (S : access ZipFile_Stream) return Integer is
   begin
      return Integer (Ada.Streams.Stream_IO.Index(S.File));
   end Index;

   procedure SetName (S: access ZipFile_Stream; Name: String) is
   begin
      S.Name := To_Unbounded_String(Name);
   end SetName;

   function GetName (S: access ZipFile_Stream) return String is
   begin
      return To_String(S.Name);
   end GetName;

   procedure SetTime (S: access ZipFile_Stream; ModificationTime: Time) is
   begin
      S.ModificationTime := ModificationTime;
   end SetTime;

   function GetTime (S: access ZipFile_Stream) return Time is
   begin
      return S.ModificationTime;
   end GetTime;

   function End_Of_Stream (S : access ZipFile_Stream) return Boolean is
   begin
      return Ada.Streams.Stream_IO.End_Of_File(S.File);
   end End_Of_Stream;

end Zip_Streams;
