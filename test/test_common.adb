package body Test_Common is

  function test_dir return VString is
  begin
    return +"test_data" & Directory_Separator;
  end;

  function test_files return VString is
  begin
    return
      +"*.ad* " & "*.hac " & test_dir & "*.ad* " &
        test_dir & "*.txt " &
        "*.cmd " &
        test_dir & "*.pgm " & test_dir & "*.ppm " & test_dir & "*.jpg " &
        test_dir & "*.bin " & test_dir & "*.pdf " & test_dir & "*.xls " &
        test_dir & "*.au  " & test_dir & "*.db  " & test_dir & "*.mdb ";

      --  Orig. set: *.mix *.ad* *.txt *.cmd *.bmp *.csv *.pdf *.html *.bin *.xls
  end;

end Test_Common;
