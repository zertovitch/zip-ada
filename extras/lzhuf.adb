with Ada.Command_Line;                  use Ada.Command_Line;
with Ada.Calendar;                      use Ada.Calendar;
with Ada.Text_IO;                       use Ada.Text_IO;
with Ada.Direct_IO;

with Interfaces;                        use Interfaces;

with LZH;

procedure LZHuf is

  package Byte_IO is new Ada.Direct_IO (Unsigned_8);
  use Byte_IO;
  package CIO is new Integer_IO (Byte_IO.Count);
  package FIO is new Float_IO (Float);

  Infile  : Byte_IO.File_Type;
  Outfile : Byte_IO.File_Type;

  Isize, Osize : Byte_IO.Count;

  Dots : constant := 16;
  Done_Dots : Natural := 0;
  Idot : Byte_IO.Count;

  procedure Display_Progress (Done : Float) is
    New_Done_Dots : constant Natural :=
      Natural (Float (Dots) * Done);
  begin
    for I in Done_Dots + 1 .. New_Done_Dots loop
      Put ('.');
    end loop;
    Done_Dots := New_Done_Dots;
  end Display_Progress;

  function Read_IO_Byte return Unsigned_8 is
  pragma Inline (Read_IO_Byte);
    B : Unsigned_8;
    I : constant Byte_IO.Count := Index (Infile);
  begin
    Read (Infile, B);
    if I = 1 then
      Display_Progress (0.0);
    elsif I = Isize then
      Display_Progress (1.0);
    elsif Idot = 0 or else I mod Idot = 0 then
      Display_Progress (Float (I) / Float (Isize));
    end if;
    return B;
  end Read_IO_Byte;

  function File_More_bytes return Boolean is
  begin
    return not End_Of_File (Infile);
  end File_More_bytes;

  procedure Write_IO_Byte (B : Unsigned_8) is
  pragma Inline (Write_IO_Byte);
  begin
    Write (Outfile, B);
  end Write_IO_Byte;

  package File_LZH is
    new LZH (
      Read_byte  => Read_IO_Byte,
      More_bytes => File_More_bytes,
      Write_byte => Write_IO_Byte
    );
  use File_LZH;

  type T_Action is (Do_Encode, Do_Decode);
  Action : T_Action;

  T0, T1 : Time;
  seconds_elapsed : Duration;

begin
  if Argument_Count /= 3 then
    Put_Line (
      "Usage: lzhuf e(ncode-compress)|d(ecode-decompress) infile outfile");
    return;
  end if;
  declare
    S : constant String := Argument (1);
  begin
    case S (1) is
      when 'e' | 'E' => Action := Do_Encode;
      when 'd' | 'D' => Action := Do_Decode;
      when others =>
        Put_Line (
          "! Use [d] for decoding-decompression or" &
          " [e] for encoding-compression"
        );
        return;
    end case;
  end;
  Byte_IO.Open (Infile, Byte_IO.In_File, Argument (2));
  Isize := Byte_IO.Size (Infile);
  Idot := Isize / Dots;
  Byte_IO.Create (Outfile, Name => Argument (3));
  Put (" In:"); CIO.Put (Isize); Put ("  [");
  T0 := Clock;
  --
  case Action is
    when Do_Encode => Encode;
    when Do_Decode => Decode;
  end case;
  --
  T1 := Clock;
  Byte_IO.Close (Infile);
  Osize := Byte_IO.Size (Outfile);
  Byte_IO.Close (Outfile);
  Put ("] Out:"); CIO.Put (Osize);
  if Isize /= 0 and Osize /= 0 then
    Put ("  ");
    case Action is
      when Do_Encode => CIO.Put ((100 * Osize) / Isize, 0);
      when Do_Decode => CIO.Put ((100 * Isize) / Osize, 0);
    end case;
    Put ("%,");
  end if;
  seconds_elapsed := T1 - T0;
  FIO.Put (Float (seconds_elapsed), 4, 2, 0);
  Put_Line (" seconds.");
end LZHuf;
