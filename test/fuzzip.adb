--  Fuzzing for the various Zip-Ada compression methods.
--
--  What is tested here:
--
--      x --[compression]--> zip --[decompression]--> x'
--
--      1         2           3           4           5
--
--  x should be equal to x' - and of course
--  there should be no error inbetween.
--
--  What is *not* tested here: random data sent to decompression,
--  which triggers Zip.Archive_Corrupted or similar.

with Zip;
with Zip.Compress; use Zip.Compress;
with Zip.Create;
with UnZip.Streams;

with Zip_Streams; use Zip_Streams;

with RW_File; use RW_File;

with Ada.Command_Line;                  use Ada.Command_Line;
with Ada.Numerics.Float_Random;         use Ada.Numerics.Float_Random;
with Ada.Strings.Unbounded;             use Ada.Strings.Unbounded;
with Ada.Text_IO;                       use Ada.Text_IO;

procedure Fuzzip is

  dump : constant Boolean:= False;
  max_trace: constant := 4;
  trace : Natural := max_trace;
  test_counter: Natural := 0;

  Seed : Generator;
  function R (N : Positive) return Positive is
    S : constant Natural := Integer (Float (N) * Random (Seed));
  begin
    if S >= N then
      return 1;
    else
      return 1 + S;
    end if;
  end R;

  procedure Test_all_methods_single_data (data_for_testing : Unbounded_String) is

    procedure Single_test (method : Compression_Method) is
      mem_stream_Zip_archive,
      mem_stream_content,
      mem_stream_unpacked : aliased Memory_Zipstream;
      zci : Zip.Create.Zip_Create_info;
      zi: Zip.Zip_info;
      unpacked : Unbounded_String;
      name_for_data_for_testing : constant String := "data_for_testing.dat";
      name_in_zip: constant String := "packed_" & Compression_Method'Image(method) & ".dat";
      check_ok : Boolean;
      use Zip.Create;
    begin
      test_counter := test_counter + 1;
      if trace >= max_trace then
        Put (
          "      Method " & Compression_Method'Image(method) &
          " size:" & Integer'Image(Length (data_for_testing))
        );
      end if;
      --  Step 2
      Create (zci, mem_stream_Zip_archive'Unchecked_Access, "to_memo.zip", method);
      Set (mem_stream_content, data_for_testing);
      Set_Name (mem_stream_content, name_in_zip);
      Add_Stream (zci, mem_stream_content);
      Finish (zci);

      --  State 3 : we have a Zip archive in memory, in mem_stream_Zip_archive
      if trace >= max_trace then
        Put (
          " - Zip size:" & ZS_Size_Type'Image(Size (mem_stream_Zip_archive)) & ',' &
          Integer'Image (Integer (
            100.0 * Float(Size (mem_stream_Zip_archive)) /
            Float(Length (data_for_testing))
          )) & '%'
        );
      end if;

      --  Step 4
      Zip.Load (zi, mem_stream_Zip_archive);
      UnZip.Streams.Extract (mem_stream_unpacked, zi, name_in_zip);
      if trace >= max_trace then
        Put (" - unpacked");
      end if;

      --  Step 5 : retrieve unpacked data to string x' = packed
      Get (mem_stream_unpacked, unpacked);

      check_ok := unpacked = data_for_testing;
      if trace >= max_trace then
        Put_Line (" - compared.");
      end if;

      if dump or not check_ok then
        RW_File.Write_File (name_for_data_for_testing, data_for_testing);
        RW_File.Write_File (name_in_zip, unpacked);
      end if;
      if not check_ok then
        raise Constraint_Error with "Unpacked data /= Test data !";
      end if;
    end Single_test;

  begin
    for m in Single_Method loop
      Single_test (m);
    end loop;
  end Test_all_methods_single_data;

  -------------------------------------
  --  Here is the part with fuzzing  --
  -------------------------------------

  patches               : constant Integer :=  15;  --  successive patches
  alterations           : constant Integer :=  10;  --  occurs within a patch
  noises                : constant Integer :=   2;  --  occurs within a patch
  slices                : constant Integer :=   5;  --  test various random slices of each patch
  --
  alteration_max_length : constant Integer :=  50;
  alteration_max_ampl   : constant Integer :=   3;
  noise_max_length      : constant Integer := 300;

  procedure Slicing (original : Unbounded_String) is
    i1, i2, mi, ma: Natural;
  begin
    if trace > 2 then
      Put_Line ("    No Slicing");
    end if;
    Test_all_methods_single_data (original);
    for i in 1 .. slices loop
      if trace > 2 then
        Put_Line ("    Slicing" & Integer'Image(i) & " /" & Integer'Image(slices));
      end if;
      i1 := R (Length (original));
      i2 := R (Length (original));
      mi := Integer'Min (i1, i2);
      ma := Integer'Max (i1, i2);
      Test_all_methods_single_data (
        To_Unbounded_String (Slice (original, mi, ma))
      );
    end loop;
  end Slicing;

  procedure Patchwork (original : Unbounded_String) is
    copy : Unbounded_String := original;
    i1, i2, mi, ma : Natural;
    c : Character;
  begin
    if trace > 1 then
      Put_Line ("  No Patchwork");
    end if;
    Slicing (original);
    for i in 1 .. patches loop
      if trace > 1 then
        Put_Line ("  Patchwork" & Integer'Image(i) & " /" & Integer'Image(patches));
      end if;
      --  Insert a random slice at a random place:
      i1 := R (Length (copy));
      i2 := R (Length (copy));
      mi := Integer'Min (i1, i2);
      ma := Integer'Max (i1, i2);
      Insert (copy, R (Length (copy)), Slice (copy, mi, ma));
      --  Alter some stripes of the contents:
      for j in 1 .. alterations loop
        i1 := R (Length (copy));
        i2 := i1 + R (alteration_max_length);
        for k in i1 .. Integer'Min (i2, Length (copy)) loop
          c := Element (copy, k);
          c := Character'Val (
                 (Character'Pos (c) +
                  R (alteration_max_ampl * 2) - alteration_max_ampl
                 ) mod 256
               );
          Replace_Element (copy, k, c);
        end loop;
      end loop;
      --  Replace some stripes of the contents by pure noise:
      for j in 1 .. noises loop
        i1 := R (Length (copy));
        i2 := i1 + R (noise_max_length);
        for k in i1 .. Integer'Min (i2, Length (copy)) loop
          Replace_Element (copy, k, Character'Val (R (256) - 1));
        end loop;
      end loop;
      --  Further test with current copy:
      Slicing (copy);
    end loop;
  end Patchwork;

  procedure All_tests_single_initial_data (file_name : String) is
    the_original : Unbounded_String;
  begin
    if trace > 0 then
      Put_Line ("Reading initial data...");
    end if;
    --  Step 1 : store data to string x = the_original
    RW_File.Read_File (file_name, the_original);
    --
    if trace > 0 then
      Put_Line ("Fuzzing and testing...");
    end if;
    Patchwork (the_original);
    if trace > 0 then
      Put_Line (
        "Completed" & Integer'Image(test_counter) &
        " compress-decompress-compare tests successfully"
      );
    end if;
  end All_tests_single_initial_data;

  default_file_name : constant String := "test/fuzzip.adb";

begin
  if Argument_Count > 1 then
    trace := Integer'Value (Argument (2));
  end if;
  if trace > 0 then
    Put_Line ("Fuzzip - testing compress-decompress-compare on randomized data");
    Put_Line ("Syntax: fuzzip [original file] [trace level = 0, 1, 2,...]");
    Put_Line ("Default: fuzzip " & default_file_name & Integer'Image(trace));
  end if;
  Reset (Seed, 1);  --  Fixed seed for reproducibility
  if Argument_Count > 0 then
    All_tests_single_initial_data (Argument (1));
  else
    All_tests_single_initial_data (default_file_name);
  end if;
end Fuzzip;
