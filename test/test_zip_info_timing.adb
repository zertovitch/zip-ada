with Zip, Ada.Calendar, Ada.Text_IO;

procedure Test_Zip_Info_timing is
  zi: Zip.Zip_info;
  t1, t2: Ada.Calendar.Time;
  use Ada.Calendar;
begin
  t1:= Ada.Calendar.Clock;
  Zip.Load(zi, "rt.jar");
  -- More than 17,000 entries, that is more than 510,000 bytes, not counting file names
  t2:= Ada.Calendar.Clock;
  Ada.Text_IO.Put_Line("Time elapsed for loading Zip catalogue:" & Duration'Image(t2-t1));
  Ada.Text_IO.Put_Line("Entries:" & Integer'Image(Zip.Entries(zi)));
end Test_Zip_Info_timing;
