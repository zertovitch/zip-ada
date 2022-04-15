--  Reverse operation to the function Ada.Directories.Modification_Time.

with Ada.Calendar;
with GNAT.OS_Lib;

procedure Set_Modification_Time_GNAT
  (Name : in String;
   To   : in Ada.Calendar.Time)
is
  use Ada.Calendar, GNAT.OS_Lib;

  GNAT_Time : constant OS_Time :=
    GM_Time_Of (Year   => Year (To),
                Month  => Month (To),
                Day    => Day (To),
                Hour   =>  Integer (Seconds (To)) / 3600,
                Minute => (Integer (Seconds (To)) / 60) mod 60,
                Second => Integer (Seconds (To)) mod 60);
begin
  Set_File_Last_Modify_Time_Stamp (Name, GNAT_Time);
end Set_Modification_Time_GNAT;
