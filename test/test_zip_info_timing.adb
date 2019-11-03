--  Time the loading of a Zip archive catalogue.

with Zip, Ada.Calendar, Ada.Text_IO;

procedure Test_Zip_Info_timing is

  procedure Test_1_Archive (name : String) is
    zi : Zip.Zip_info;
    t1, t2 : Ada.Calendar.Time;
    use Ada.Calendar;
  begin
    if Zip.Exists (name) then
      t1 := Ada.Calendar.Clock;
      --  NB: for loading, the value of parameter "duplicate_names" won't impact performance.
      Zip.Load (zi, name);
      t2 := Ada.Calendar.Clock;
      Ada.Text_IO.Put_Line ("Zip Archive: " & Zip.Zip_name (zi));
      Ada.Text_IO.Put_Line ("Entries:" & Integer'Image (Zip.Entries (zi)));
      Ada.Text_IO.Put_Line ("Time elapsed for loading the Zip catalogue:" & Duration'Image (t2 - t1));
      Ada.Text_IO.New_Line;
    end if;
  end Test_1_Archive;

begin
  --  Zip files created by the test Zip_with_many_files.
  Test_1_Archive ("65535.zip");
  Test_1_Archive ("32768.zip");
  Test_1_Archive ("16384.zip");
  --  rt.jar: More than 17,000 entries, that is more than 510,000 bytes, not counting file names
  Test_1_Archive ("rt.jar");
end Test_Zip_Info_timing;
