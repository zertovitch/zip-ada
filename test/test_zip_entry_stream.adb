--  This demo reads text files (file names given as command-line parameters)
--  and stores them into a zip archive using the Zip_Entry_Stream_Type
--  output stream type.

with Zip.Compress, Zip.Create;

with Ada.Calendar, Ada.Command_Line, Ada.Text_IO;

procedure Test_Zip_Entry_Stream is
  use Zip.Create;
  use Ada.Calendar, Ada.Command_Line, Ada.Text_IO;

  Archive_Info  : Zip_Create_Info;
  Archive_File  : aliased Zip_File_Stream;
  Archive_Entry : aliased Zip_Entry_Stream_Type;

  Text : File_Type;
  T_Start, T_Stop : Time;

begin
  Create_Archive (Archive_Info, Archive_File'Unchecked_Access, "test_zes.zip");
  for I in 1 .. Argument_Count loop
    Set (Archive_Info, Zip.Compress.LZMA_3);  --  We set a compression method.
    T_Start := Clock;
    --
    Open (Archive_Entry);
    Open (Text, In_File, Argument (I));
    while not End_Of_File (Text) loop
      --  Of course, we could here have something less
      --  trivial than simply copying a file.
      String'Write (Archive_Entry'Access, Get_Line (Text));
      Character'Write (Archive_Entry'Access, ASCII.LF);  --  UNIX end-of-line
    end loop;
    Close (Text);
    Close (Archive_Entry, "zes_" & Argument (I), use_clock, Archive_Info);
    --
    T_Stop := Clock;
    Put_Line (
      "File """ & Argument (I) & """ added as text to archive """ &
      Name (Archive_Info) &
      """ in" & Duration'Image (T_Stop - T_Start) & " seconds."
    );
    --  We also add the file as binary, for checking the CRC32 value:
    Set (Archive_Info, Zip.Compress.Deflate_1);  --  Quick compression method
    Add_File (
      Archive_Info,
      Argument (I),
      Modification_time => use_file_modification_time
    );
  end loop;
  Finish (Archive_Info);
end Test_Zip_Entry_Stream;
