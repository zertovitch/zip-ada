--  Gather some statistics about LZ77 DLE codes from UnZip.Decompress
--  (some_trace mode), and output it in a matrix form.
--
--  May help answering questions like:
--   - which distances and lengths are used, and how often ?
--   - which distance and length *pairs* are used, and how often ?

with Ada.Integer_Text_IO, Ada.Text_IO;

procedure LZ77_Stats is
  use Ada.Integer_Text_IO, Ada.Text_IO;
  LZ77_dump : File_Type;
  tag : String (1 .. 3);
  wrong_LZ77_tag : exception;
  a, b : Integer;
  stat        : File_Type;
  stat_name   : constant String := "LZ77_Stats.csv";
  sep         : constant Character := ';';

  subtype Distance_range is Integer range 1 .. 32768;
  subtype Length_range is Integer range 3 .. 258;

  d_stats : array (Distance_range) of Natural := (others => 0);
  l_stats : array (Length_range) of Natural := (others => 0);
  type DL_matrix is array (Distance_range, Length_range) of Natural;
  type p_DL_matrix is access DL_matrix;
  dl_stats : constant p_DL_matrix := new DL_matrix'(others => (others => 0));
  total : Natural := 0;
begin
  --
  --  Read file from UnZip.Decompress, some_trace mode
  --
  Open (LZ77_dump, In_File, "dump.lz77");
  while not End_Of_File (LZ77_dump) loop
    Get (LZ77_dump, tag);
    if tag = "Lit" then
      Get (LZ77_dump, a);
    elsif tag = "DLE" then
      Get (LZ77_dump, a);
      Get (LZ77_dump, b);
      d_stats (a) := d_stats (a) + 1;
      l_stats (b) := l_stats (b) + 1;
      dl_stats (a, b) := dl_stats (a, b) + 1;
      total := total + 1;
    else
      raise wrong_LZ77_tag;
    end if;
    Skip_Line (LZ77_dump);
  end loop;
  Close (LZ77_dump);
  --
  --  Write stats
  --
  Create (stat, Out_File, stat_name);
  Put (stat, sep & sep & sep & "Length" & sep & sep);
  for l in Length_range loop
    Put (stat, l);
    Put (stat, sep);
  end loop;
  New_Line (stat, 2);
  Put (stat, sep & sep);
  Put (stat, total);
  Put (stat, sep & "stats" & sep & sep);
  for l in Length_range loop
    Put (stat, l_stats (l));
    Put (stat, sep);
  end loop;
  New_Line (stat);
  Put_Line (stat, "Distance" & sep & sep & "stats");
  New_Line (stat);
  for d in Distance_range loop
    Put (stat, d);
    Put (stat, sep & sep);
    Put (stat, d_stats (d));
    Put (stat, sep & sep & sep);
    for l in Length_range loop
      Put (stat, dl_stats (d, l));
      Put (stat, sep);
    end loop;
    New_Line (stat);
  end loop;
  Close (stat);
end LZ77_Stats;
