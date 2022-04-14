with Ada.Calendar,
     Ada.Directories;

with System;
with GNAT.OS_Lib;

procedure Set_Modification_Time_GNAT
  (Name : in String;
   To   : in Ada.Calendar.Time)
is
  use Ada.Directories, Ada.Calendar, GNAT.OS_Lib;
  t : constant OS_Time :=
    GM_Time_Of (Year   => Year (To),
                Month  => Month (To),
                Day    => Day (To),
                Hour   =>  Integer (Seconds (To)) / 3600,
                Minute => (Integer (Seconds (To)) / 60) mod 60,
                Second => Integer (Seconds (To)) mod 60);
  procedure C_Set_Modification_Time (N : System.Address; T : OS_Time);
  pragma Import (C, C_Set_Modification_Time, "__gnat_set_file_time_name");
  F_Name : constant String := Full_Name (Name);
  C_Name : aliased String (1 .. F_Name'Length + 1);
begin
  C_Name := F_Name & ASCII.NUL;
  C_Set_Modification_Time (C_Name'Address, t);
end Set_Modification_Time_GNAT;
