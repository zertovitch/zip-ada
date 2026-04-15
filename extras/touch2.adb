with Ada.Calendar,
     Ada.Command_Line,
     Ada.Text_IO;

with Set_Modification_Time_GNAT;

procedure Touch2 is
  use Ada.Calendar, Ada.Command_Line, Ada.Text_IO;
begin
  if Argument_Count not in 1 | 7 then
    Put_Line (Current_Error, "Syntax: touch2 file_name year month day hour minute second");
    Put_Line (Current_Error, "  or    touch2 file_name");
    New_Line (Current_Error);
    Put_Line (Current_Error, "The first variant sets the indicated time as timestamp.");
    Put_Line (Current_Error, "The second variant sets the current time as timestamp.");
  else
    Set_Modification_Time_GNAT
      (Argument (1),
       (if Argument_Count = 1 then
          Clock
        else
          Time_Of
            (Year  => Integer'Value (Argument (2)),
             Month => Integer'Value (Argument (3)),
             Day   => Integer'Value (Argument (4)),
             Seconds => Duration
               (Integer'Value (Argument (5)) * 60 * 60 +
                Integer'Value (Argument (6)) *      60 +
                Integer'Value (Argument (7))))));
  end if;
end Touch2;
