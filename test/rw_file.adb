with Ada.Sequential_IO;

package body RW_File is

   package Character_Sequential_IO is new Ada.Sequential_IO (Character);

   procedure Read_File (Filename : String; Content : out Unbounded_String) is
      package Io renames Character_Sequential_IO;
      use Io;
      F : Io.File_Type;
      Ch : Character;
   begin
      Content := Null_Unbounded_String;
      Open (F, In_File, Filename);
      while not End_Of_File (F) loop
         Read (F, Ch);
         Append (Content, Ch);
      end loop;
      Close (F);
   exception when others =>
         if Is_Open (F) then
            Close (F);
         end if;
         raise;
   end Read_File;

   procedure Write_File (Filename : String;
                         Content : Unbounded_String) is
      package Io renames Character_Sequential_IO;
      use Io;
      F : Io.File_Type;
   begin
      Create (F, Out_File, Filename);
      for I in 1 .. Length (Content) loop
         Io.Write (F, Element (Content, I));
      end loop;
      Close (F);
   exception when others =>
         if Is_Open (F) then
            Close (F);
         end if;
         raise;
   end Write_File;

   procedure Process_Lines (S : Unbounded_String;
                            Process_Line : Line_processing) is

      function Is_Text (Ch : Character) return Boolean is
      begin
         return Ch >= ' ' or else Ch = ASCII.HT;
      end Is_Text;

      Last : constant Integer := Length (S);
      From : Integer := 1;
      Upto1, Upto2 : Integer;
   begin
      while From < Last loop
         Upto1 := From;
         while Upto1 < Last and then
               Is_Text (Ada.Strings.Unbounded.Element (S, Upto1 + 1)) loop
            Upto1 := Upto1 + 1;
         end loop;
         Upto2 := Upto1;
         while Upto2 < Last and then
               not Is_Text (Ada.Strings.Unbounded.Element (S, Upto2 + 1))
         loop
            Upto2 := Upto2 + 1;
         end loop;
         Process_Line (Slice (S, From, Upto1));
         From := Upto2 + 1;
      end loop;
   end Process_Lines;

end RW_File;
