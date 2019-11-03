with UnZip; use UnZip;

procedure Test_Extract is
begin
  --  Extract to a file.
  --    You can customize this with Create_Path (creation of missing paths)
  --    and Compose_File_Name (add a custom prefix directory for instance)
  --    via the file_system_routines parameter. See tools/unzipada.adb for
  --    a complete example.
  --
  Extract ("aaa.zip");                              --  ok
  Extract ("aaa.zip", "file2.txt");                 --  ok
  Extract ("aaa.zip", "file2.txt", "file2_b.txt");  --  ok
end Test_Extract;
