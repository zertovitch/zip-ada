with HAC_Pack;  use HAC_Pack;

procedure Test_RZ is

  function Gather_Arguments return VString is
    args : VString;
  begin
    for i in 1 .. Argument_Count loop
      args := args & Argument (i) & ' ';
    end loop;
    return args;
  end;

  r : Integer;

begin
  Put_Line ("Testing the ReZip tool - only internal compression methods.");
  New_Line;
  Put_Line ("A call to Comp_Zip is done to compare archives (before / after)");
  Put_Line ("and check that their uncompressed contents are identical.");
  New_Line;
  Put_Line ("Usage: hax test_rz.adb [ReZip options]");
  New_Line;
  --  The HAX command-line tool can be built from the project HAC @
  --    https://hacadacompiler.sourceforge.io/ ,
  --    https://github.com/zertovitch/hac

  --  Create an archive with default compression method (fast).
  r := Shell_Execute (+".." & Directory_Separator & 
                      "zipada test_rz *.ad* *.txt *.cmd *.sh *.pdf *.mdb *.xls *.au");

  --  Recompress the archive, with optional options...
  r := Shell_Execute (+".." & Directory_Separator & 
                      "rezip -int " & Gather_Arguments & " test_rz");

  --  Show recompression report.
  r := Shell_Execute ("firefox test_rz.ReZip.html");
  r := Shell_Execute ("start   test_rz.ReZip.html");

  --  Compare before vs. after.
  r := Shell_Execute (+".." & Directory_Separator & 
                      "comp_zip test_rz test_rz.repacked -q2");

end Test_RZ;