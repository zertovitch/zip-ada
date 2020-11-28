--  This is a HAC script *and* an Ada program (your choice).
--
--  The HAC command-line tool can be built from the project HAC @
--    https://hacadacompiler.sourceforge.io/ ,
--    https://github.com/zertovitch/hac

with HAC_Pack;  use HAC_Pack;
--  NB: the "full Ada" package for HAC_Pack is in /src in the HAC project

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
  Put_Line ("Usage:  hac test_rz.adb [ReZip options]");
  New_Line;

  --  Create an archive with default compression method (fast).
  Shell_Execute (+".." & Directory_Separator &
                 "zipada test_rz *.ad* *.txt *.cmd *.sh *.pgm *.ppm *.pdf *.db *.mdb *.xls *.au");

  --  Recompress the archive, with optional options...
  Shell_Execute (+".." & Directory_Separator &
                 "rezip -int " & Gather_Arguments & " test_rz");

  --  Show recompression report.
  Shell_Execute ("firefox test_rz.ReZip.html");  --  Linux
  Shell_Execute ("start   test_rz.ReZip.html");  --  Windows

  --  Compare before vs. after.
  Shell_Execute (+".." & Directory_Separator &
                 "comp_zip test_rz test_rz.repacked -q2");

end Test_RZ;
