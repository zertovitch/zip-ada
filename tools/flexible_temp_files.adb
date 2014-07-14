package body Flexible_temp_files is

  use Ada.Text_IO;

  procedure Initialize is
  begin
    Create(radix_temp_file, Out_File);
    -- A.8.2 File Management
    -- A null string for Name specifies an external file that is not
    -- accessible after the completion of the main program (a temporary file).
    --
    -- NB: at least on ObjectAda, the temporary file is even not accessible
    -- after Close, that is before completion of main program. So we need
    -- to keep one temporary file open and take its name as radix for other ones.
  end Initialize;

  function Radix return String is
    s: constant String:= Name(radix_temp_file);
    -- GNAT & ObjectAda give the temporary file name; other compilers/systems
    -- may have not a usable name and raise Use_Error (A.8.2, 23).
    dot: Integer:= s'Last+1;
  begin
    if s = "" then
      return s;
    end if;
    for i in reverse s'Range loop
      if s(i)='.' then
        dot:= i;
        exit;
      end if;
    end loop;
    return s(s'First..dot-1);
  end Radix;

  procedure Finalize is
  begin
    Close(radix_temp_file);
  end Finalize;

end Flexible_temp_files;