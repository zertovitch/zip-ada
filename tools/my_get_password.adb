------------------------------------------------------------------------------
--  File:            Mygetpas.adb or My_get_password.adb
--  Description:     part of UnZipada demo
------------------------------------------------------------------------------

with Ada.Text_IO;                       use Ada.Text_IO;
with Ada.Strings.Unbounded;             use Ada.Strings.Unbounded;

procedure My_get_password(password: out Unbounded_String) is
  c: Character;
begin
  New_Line;
  Put_Line(" Current password is incorrect.");
  Put(" Password please : ");
  -- Fake "Get_line( password );" without echo.
  -- We use Get_Immediate that has no echo on GNAT/Windows - no mention
  -- of that feature in the (A)RM95, so no warranty about it!

  password:= To_Unbounded_String("");

  loop
    Get_Immediate(c);
    exit when c = ASCII.CR;
    Put('*');
    password:= password & c;
  end loop;

  New_Line;

  -- Cosmetic : position for the [.....]
  Put("                                                                    ");
end My_get_password;