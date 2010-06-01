------------------------------------------------------------------------------
--  File:            TB_Wrap.adb
--  Description:     Trace-back wrapper for GNAT 3.13p+ (body)
------------------------------------------------------------------------------

with GNAT.Traceback.Symbolic, Ada.Exceptions, Ada.Text_IO;
use Ada.Exceptions, Ada.Text_IO;

procedure TB_Wrap is
  --  pragma Compiler_options("-g");
  --  pragma Binder_options("-E");
begin
  My_main_procedure;
exception
  when E: others =>
    New_Line(Standard_Error);
    Put_Line(Standard_Error,
             "--------------------[ Unhandled exception ]-----------------");
    Put_Line(Standard_Error, " > Name of exception . . . . .: " &
             Ada.Exceptions.Exception_Name(E) );
    Put_Line(Standard_Error, " > Message for exception . . .: " &
             Ada.Exceptions.Exception_Message(E) );
    Put_Line(Standard_Error, " > Trace-back of call stack: " );
    Put_Line(Standard_Error, GNAT.Traceback.Symbolic.Symbolic_Traceback(E) );
end TB_Wrap;
