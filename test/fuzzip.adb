--  Fuzzing for the various Zip-Ada compression methods.
--  Test is derived from ZipTest.
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

--  WIP (proper fuzzing TBD) !!

with Zip;
with Zip.Compress; use Zip.Compress;
with Zip.Create;
with UnZip.Streams;

with Zip_Streams; use Zip_Streams;

with RW_File; use RW_File;

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO; use Ada.Text_IO;

procedure Fuzzip is

  dump : constant Boolean:= False;
  trace : constant := 1;
  test_counter: Natural := 0;

  procedure Test_all_methods_single_data (the_original : Unbounded_String) is

    procedure Single_test (method : Compression_Method) is
      mem_stream_Zip_archive,
      mem_stream_content,
      mem_stream_unpacked : aliased Memory_Zipstream;
      zci : Zip.Create.Zip_Create_info;
      zi: Zip.Zip_info;
      some_copy : Unbounded_String;
      name_for_original : constant String := "test/original.dat";
      name_in_zip: constant String := "test/some_copy_" & method'Image & ".dat";
      check_ok : Boolean;
      use Zip.Create;
    begin
      test_counter := test_counter + 1;
      if trace > 1 then
        Put_Line ("    Testing method " & method'Image);
      end if;
      --  Step 2
      Create (zci, mem_stream_Zip_archive'Unchecked_Access, "to_memo.zip", method);
      Set (mem_stream_content, the_original);
      Set_Name (mem_stream_content, name_in_zip);
      Add_Stream (zci, mem_stream_content);
      Finish (zci);

      --  State 3 : we have a Zip archive in memory, in mem_stream_Zip_archive

      --  Step 4
      Zip.Load (zi, mem_stream_Zip_archive);
      UnZip.Streams.Extract (mem_stream_unpacked, zi, name_in_zip);

      --  Step 5 : retrieve unpacked data to string x' = some_copy
      Get (mem_stream_unpacked, some_copy);

      check_ok := some_copy = the_original;

      if dump or not check_ok then
        RW_File.Write_File (name_for_original, the_original);
        RW_File.Write_File (name_in_zip, some_copy);
      end if;
      if not check_ok then
        raise Constraint_Error with "Copy /= Original !";
      end if;
    end Single_test;

  begin
    for m in Single_Method loop
      Single_test (m);
    end loop;
  end Test_all_methods_single_data;

  procedure All_tests_single_initial_data (file_name : String) is
    the_original : Unbounded_String;
  begin
    if trace > 0 then
      Put_Line ("Reading initial data");
    end if;
    --  Step 1 : store data to string x = the_original
    RW_File.Read_File (file_name, the_original);
    --
    Test_all_methods_single_data (the_original);
    if trace > 0 then
      Put_Line (
        "Completed" & test_counter'Image &
        " compress-decompress-compare tests successfully"
      );
    end if;
  end All_tests_single_initial_data;

begin
  All_tests_single_initial_data ("test/fuzzip.adb");
end Fuzzip;
