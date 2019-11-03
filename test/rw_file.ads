--  Contributed by ITEC - NXP Semiconductors
--
--  June 2008
--

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package RW_File is

   procedure Read_File (Filename : String;
                        Content : out Unbounded_String);

   procedure Write_File (Filename : String;
                         Content : Unbounded_String);

   type Line_processing is access procedure (Line : String);

   procedure Process_Lines (S : Unbounded_String;
                            Process_Line : Line_processing);

end RW_File;
