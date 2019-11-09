------------------------------------------------------------------------------
--  File:            my_resolve_conflict.adb
--  Description:     part of the UnZipAda demo
------------------------------------------------------------------------------

with Zip;
with UnZip;                             use UnZip;
with Ada.Text_IO;                       use Ada.Text_IO;

procedure My_resolve_conflict
             (file_name       :  in String;
              name_encoding   :  in Zip.Zip_name_encoding;
              action          : out Name_conflict_intervention;
              new_name        : out String;
              new_name_length : out Natural)
is
  pragma Unreferenced (name_encoding);
  c : Character;
begin
  loop
    New_Line;
    Put_Line ("File " & file_name & " already exists.");
    Put (" Overwrite ?  (y)es / (n)o / (A)ll / (N)one / (r)ename / (q)uit ");
    Get_Immediate (c);
    Put_Line ("-> " & c);
    exit when c = 'y' or c = 'n' or c = 'A' or c = 'N' or c = 'r' or c = 'q';
  end loop;
  case c is
    when 'y'       => action := yes;
    when 'n'       => action := no;
    when 'A'       => action := yes_to_all;
    when 'N'       => action := none;
    when 'q'       => action := abort_now;
    when 'r'       => action := rename_it; Put ("New name: ");
                      Get_Line (new_name, new_name_length);
    when others    => null;
  end case;

  --  Cosmetic : position for the [.....]
  Put ("                                                                    ");
end My_resolve_conflict;
