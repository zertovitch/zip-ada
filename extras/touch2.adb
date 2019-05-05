with Ada_Directories_Extensions;
with Ada.Calendar;
with Ada.Command_Line;
with Ada.Text_IO;

procedure Touch2 is

  use Ada_Directories_Extensions;
  use Ada.Calendar;
  use Ada.Command_Line;
  use Ada.Text_IO;

begin
  Set_Modification_Time (
    Argument (1),
    Time_Of (
      Year  => Integer'Value (Argument (2)),
      Month => Integer'Value (Argument (3)),
      Day   => Integer'Value (Argument (4)),
      Seconds => Duration (
        Integer'Value (Argument (5)) * 60 * 60 +
        Integer'Value (Argument (6)) *      60 +
        Integer'Value (Argument (7))
      )
    )
  );
exception
  when Constraint_Error =>
    Put_Line ("Syntax: touch2 file year month day hour minute second");
end;