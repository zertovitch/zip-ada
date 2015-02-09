--  Portable package for getting temporary file names but the behaviour
--  of normal files which can be created, closed, re-read, etc. and are
--  not automatically deleted by the Ada run-time. Downside: the program
--  needs to delete the files itself.

with Ada.Text_IO;

package Flexible_temp_files is

  procedure Initialize;
  procedure Finalize;

  --  Give a suitable temporary file name radix (no extension), eventually
  --  with a path.
  function Radix return String;

private
  radix_temp_file: Ada.Text_IO.File_Type;
end Flexible_temp_files;
