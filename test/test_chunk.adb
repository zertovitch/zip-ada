--  Testing End_Error
--  TT's version has a forgiving behaviour for
--  reading arrays beyond the file's end

with Ada.Streams.Stream_IO;             use Ada.Streams.Stream_IO;
with Ada.IO_Exceptions;
with Ada.Text_IO;
with UnZip.Streams;                     use UnZip.Streams;

procedure Test_Chunk is

  procedure Consume_Chunks (s : UnZip.Streams.Stream_Access) is
    chunk : String (1 .. 950); -- Length(f2) = 961 = 31**2
  begin
    for i in 1 .. 1000 loop
      String'Read (s, chunk);
      Ada.Text_IO.Put ('[' & chunk & ']');
    end loop;
  exception
    when Ada.IO_Exceptions.End_Error =>
      Ada.Text_IO.Put ("[=== End_Error (RM 13.13.2(37) T'Read) ===]");
    when Constraint_Error =>
      Ada.Text_IO.Put ("[=== Constraint_Error (OA 7.2.2 on T'Read) ===]");
  end Consume_Chunks;

  f1 : Zipped_File_Type;
  f2 : Ada.Streams.Stream_IO.File_Type;
  a : constant String := "Test_UnZ_Streams.zip";
  n : constant String := "Test_UnZ_Streams.adb";

begin
  --  Test zipped file:
  Open (
        File         => f1,
        Archive_Name => a,
        Name         => n
  );
  Consume_Chunks (Stream (f1));
  Close (f1);
  --  Test unzipped file:
  Open (
        File => f2,
        Mode => In_File,
        Name => n
  );
  Consume_Chunks (UnZip.Streams.Stream_Access (Stream (f2)));
  Close (f2);
end Test_Chunk;
