with Ada.Calendar,
     Ada.Command_Line,
     Ada.Text_IO;

with Set_Modification_Time_GNAT;

procedure Touch2 is
  use Ada.Calendar, Ada.Command_Line, Ada.Text_IO;
begin
  if Argument_Count < 7 then
    Put_Line (Current_Error, "Syntax: touch2 file year month day hour minute second");
  else
    Set_Modification_Time_GNAT
      (Argument (1),
       Time_Of (
         Year  => Integer'Value (Argument (2)),
         Month => Integer'Value (Argument (3)),
         Day   => Integer'Value (Argument (4)),
         Seconds => Duration (
           Integer'Value (Argument (5)) * 60 * 60 +
             Integer'Value (Argument (6)) *      60 +
               Integer'Value (Argument (7))
          )
        ));
  end if;
end Touch2;
