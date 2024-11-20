--  Output LZ statistics into a .csv file for research purposes.
--  To do: compute here moving average (size n steps) and regular samples (each n/k step).

with Ada.Command_Line,
     Ada.Directories,
     Ada.Sequential_IO,
     Ada.Text_IO;

with LZ77;

procedure LZ77_Segmentation is

  package BIO is new Ada.Sequential_IO (LZ77.Byte);

  use LZ77, Ada.Directories, Ada.Text_IO, BIO;

  f_in : BIO.File_Type;
  f_dump : Ada.Text_IO.File_Type;

  function Read_byte return Byte is
    b : Byte;
  begin
    Read (f_in, b);
    return b;
  end Read_byte;

  function More_bytes return Boolean is
  begin
    return not End_Of_File (f_in);
  end More_bytes;

  ----- LZSS Parameters -----
  String_buffer_size : constant := 2**15;  --  2**15 for Deflate
  Look_Ahead_Test    : constant := 258;    --  258   for Deflate
  Threshold          : constant := 2;      --  2     for Deflate

  sep : constant Character := ';';

  procedure Emit_literal (literal : Byte) is
  begin
    Put (f_dump, '0' & sep & '0' & sep & Byte'Image (literal));
    if literal in 32 .. 126 then
      Put (f_dump, sep & sep & Character'Val (literal));
    end if;
    New_Line (f_dump);
  end Emit_literal;

  procedure Emit_DL_code (distance, length : Natural) is
  begin
    Put_Line (f_dump, distance'Image & sep & length'Image);
  end Emit_DL_code;

  procedure Dummy_Estimate_DL_Codes
    (matches          : in out LZ77.Matches_Array;
     old_match_index  : in     Natural;
     prefixes         : in     LZ77.Byte_Array;
     best_score_index :    out Positive;
     best_score_set   :    out LZ77.Prefetch_Index_Type;
     match_trace      :    out LZ77.DLP_Array)
  is null;

  m : constant Method_Type := IZ_10;

  use Ada.Command_Line;

  procedure My_LZ77 is
    new LZ77.Encode
      (String_buffer_size, Look_Ahead_Test, Threshold,
       m,
       Read_byte, More_bytes,
       Emit_literal, Emit_DL_code,
       False, Dummy_Estimate_DL_Codes);

begin
  Put_Line ("LZ77_Segmentation");
  New_Line;
  Put_Line ("lz77_segmentation.adb [input file]");
  New_Line;
  Put_Line ("Outputs:");
  Put_Line ("          *.csv: LZ77 stream");

  if Argument_Count < 1 then
    Open (f_in, In_File, "zip_lib/lz77.adb");
  else
    Open (f_in, In_File, Argument (1));
  end if;

  Create (f_dump, Out_File, "dump_" & Simple_Name (Name (f_in)) & ".csv");
  Put_Line (f_dump, "Distance" & sep & "Length" & sep & "Literal");
  New_Line;
  Put_Line ("Method: " & Method_Type'Image (m) & "  -  Encoding file " & Name (f_in));
  My_LZ77;
  Close (f_in);
  Close (f_dump);
end LZ77_Segmentation;
