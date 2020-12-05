--  Test_LZ77 runs a choice of LZ77 encoders for comparing them.
--
--  - Output of the encoder is dumped as readable text with DLE codes
--      or Literals (.lz77)
--  - LZ77 stream is decoded and written as .out for checking integrity:
--      should be identical to input file.

with Ada.Characters.Handling;           use Ada.Characters.Handling;
with Ada.Command_Line;                  use Ada.Command_Line;
with Ada.Sequential_IO;
with Ada.Text_IO;                       use Ada.Text_IO;
with LZ77;

procedure Test_LZ77 is

  package BIO is new Ada.Sequential_IO (LZ77.Byte);

  use LZ77, BIO;

  f_in, f_out : BIO.File_Type;
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

  type Text_Buffer is array (0 .. String_buffer_size + Look_Ahead_Test - 1) of Byte;
  Text_Buf : Text_Buffer;
  R : Natural;

  procedure Write_and_record_literal (literal : Byte) is
  begin
    Write (f_out, literal);
    Text_Buf (R) := literal;
    R := (R + 1) mod String_buffer_size;
  end Write_and_record_literal;

  procedure Emit_literal (literal : Byte) is
  begin
    Write_and_record_literal (literal);
    --
    Put (f_dump, "Lit" & Byte'Image (literal));
    if literal in 32 .. 126 then
      Put (f_dump, " '" & Character'Val (literal) & ''');
    end if;
    New_Line (f_dump);
  end Emit_literal;

  min_dis, min_len, max_dis, max_len : Natural;
  sum_dist, dl_count, len_258 : Natural;
  has_DL : Boolean;

  procedure Emit_DL_code (distance, length : Natural) is
    b : Byte;
    I : Natural;
  begin
    has_DL := True;
    dl_count := dl_count + 1;
    sum_dist := sum_dist + distance;
    max_dis := Natural'Max (max_dis, distance);
    max_len := Natural'Max (max_len, length);
    min_dis := Natural'Min (min_dis, distance);
    min_len := Natural'Min (min_len, length);
    if length = 258 then
      len_258 := len_258 + 1;
    end if;
    --
    Put_Line (f_dump, "DLE" & Integer'Image (distance) & Integer'Image (length));
    --
    --  Expand DL code:
    I := (R - distance) mod String_buffer_size;
    for K in 0 .. length - 1 loop
      b := Text_Buf ((I + K) mod String_buffer_size);
      Write_and_record_literal (b);
    end loop;
  end Emit_DL_code;

  procedure Dummy_Estimate_DL_Codes (
    matches          : in out LZ77.Matches_Array;
    old_match_index  : in     Natural;
    prefixes         : in     LZ77.Byte_Array;
    best_score_index :    out Positive;
    best_score_set   :    out LZ77.Prefetch_Index_Type;
    match_trace      :    out LZ77.DLP_Array
  )
  is null;

  type Method_Set is array (Method_Type) of Boolean;

  --  consider: constant Method_Set:= (others => True);
  --  consider: constant Method_Set:= (IZ_5 | IZ_7 | IZ_8 => False, others => True);
  consider : constant Method_Set := (LZHuf | IZ_10 | Rich | BT4 => True, others => False);

begin
  Put_Line ("Test_LZ77");
  New_Line;
  Put_Line ("test_lz77 [input file]");
  New_Line;
  Put_Line ("Outputs:");
  Put_Line ("         *.lz77 are dump of the LZ77 stream");
  Put_Line ("         *.out are decoded streams, should match input file");
  for m in Method_Type loop
    if consider (m) then
      max_dis  := 0;
      max_len  := 0;
      min_dis  := Natural'Last;
      min_len  := Natural'Last;
      has_DL   := False;
      dl_count := 0;
      sum_dist := 0;
      len_258  := 0;
      declare
        procedure My_LZ77 is
          new LZ77.Encode (
            String_buffer_size, Look_Ahead_Test, Threshold,
            m,
            Read_byte, More_bytes,
            Emit_literal, Emit_DL_code,
            False, Dummy_Estimate_DL_Codes
          );
      begin
        if Argument_Count < 1 then
          Open (f_in, In_File, "test/test_lz77.adb");
        else
          Open (f_in, In_File, Argument (1));
        end if;
        R := String_buffer_size - Look_Ahead_Test;
        Create (f_out, Out_File, "lz77_" & To_Lower (Method_Type'Image (m)) & "_decode.out");

        --  The dump.lz77 file is used in various places:
        --
        --  - Input:
        --       LZ77_Stats
        --       Zip.Compress.Deflate (bypass_LZ77 mode)
        --
        --  - Output:
        --       Test_LZ77 (this program)
        --       UnZip.Decompress
        --
        Create (f_dump, Out_File, "dump_" & To_Lower (Method_Type'Image (m)) & ".lz77");
        New_Line;
        Put_Line ("Method: " & Method_Type'Image (m) & "  -  Encoding file " & Name (f_in));
        My_LZ77;
        Close (f_in);
        Close (f_out);
        Close (f_dump);
        if has_DL then
          Put_Line ("  DL codes:" & Integer'Image (dl_count));
          Put_Line ("  Min distance:" & Integer'Image (min_dis));
          Put_Line ("  Max distance:" & Integer'Image (max_dis));
          Put_Line ("  Average distance:" & Integer'Image (sum_dist / dl_count));
          Put_Line ("  Cases where length is exactly 258 (Deflate-friendly):" &
                   Integer'Image (len_258));
          Put_Line ("  Min length:" & Integer'Image (min_len));
          Put_Line ("  Max length:" & Integer'Image (max_len));
        end if;
      end;
    end if;
  end loop;
end Test_LZ77;
