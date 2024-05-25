--  Time the loading of a Zip archive catalogue.

with Zip, Zip_with_many_files;
with Ada.Calendar, Ada.Text_IO;

procedure Test_Zip_Info_Timing is

  tests : constant := 50;

  use Ada.Text_IO;

  procedure Test_1_Archive (archive_name : String) is
    zi : array (1 .. tests) of Zip.Zip_Info;
    t1, t2 : Ada.Calendar.Time;
    d : Duration := 0.0;
    n : Integer;
    use Ada.Calendar;
  begin
    if Zip.Exists (archive_name) then
      for test in 1 .. tests loop
        t1 := Ada.Calendar.Clock;
        --  NB: for loading, the value of parameter "duplicate_names" won't impact performance.
        Zip.Load (zi (test), archive_name);
        t2 := Ada.Calendar.Clock;
        d := d + (t2 - t1);
      end loop;
      Put_Line ("Zip archive: " & Zip.Zip_Name (zi (1)));
      Put_Line ("   Entries:" & Integer'Image (Zip.Entries (zi (1))));
      d := d / tests;
      Put_Line
        ("   Average time elapsed for loading the Zip archive's catalogue:" &
         Duration'Image (d));
      d := d / Zip.Entries (zi (1));
      n := Integer (1.0 / d);
      Put_Line ("   Average number of entry information loaded per second:" & Integer'Image (n));
      New_Line;
    end if;
  end Test_1_Archive;

begin
  if not Zip.Exists ("many_4096.zip") then
    Zip_with_many_files;
    New_Line;
  end if;
  Put_Line ("Number of tests for each Zip archive: " & Integer'Image (tests));
  New_Line;
  --  Zip files created by the test Zip_with_many_files.
  Test_1_Archive ("many_65535.zip");
  Test_1_Archive ("many_16384.zip");
  Test_1_Archive ("many_4096.zip");
  --  rt.jar: More than 17,000 entries, that is more than 510,000 bytes, not counting file names
  Test_1_Archive ("rt.jar");
end Test_Zip_Info_Timing;
