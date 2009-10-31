with UnZip; use UnZip;

procedure Test_Extract is
begin
  Extract("aaa.zip");                             -- ok
  Extract("aaa.zip", "file2.txt");                -- ok
  Extract("aaa.zip", "file2.txt", "file2_b.txt"); -- ok
end Test_Extract;