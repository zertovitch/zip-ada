--  Gather some statistics about LZ77 DLE codes.
--  May help answering questions like:
--   - which distances and lengths are used, and how often ?
--   - which distance and length *pairs* are used, and how often ?

with Ada.Integer_Text_IO;               use Ada.Integer_Text_IO;
with Ada.Text_IO;                       use Ada.Text_IO;

procedure LZ77_Stats is
  LZ77_dump : File_Type;
  tag: String(1..3);
  wrong_LZ77_tag: exception;
  a, b: Integer;
  stat        : File_Type;
  stat_name   : constant String:= "LZ77_Stats.csv";
  sep         : constant Character:= ';';
begin
  Open(LZ77_dump, In_File, "dump.lz77");  --  File from UnZip.Decompress, some_trace mode
  while not End_Of_File(LZ77_dump) loop
    Get(LZ77_dump, tag);
    if tag = "Lit" then
      Get(LZ77_dump, a);
    elsif tag = "DLE" then
      Get(LZ77_dump, a);
      Get(LZ77_dump, b);
      --
      --  !! stub - do something here :-)
      --
    else
      raise wrong_LZ77_tag;
    end if;
    Skip_Line(LZ77_dump);
  end loop;
  Close(LZ77_dump);
  --
  --  Write stats
  --
  Create(stat, Out_File, stat_name);
  --  !! stub - do something here :-)
  Close(stat);
end LZ77_Stats;
