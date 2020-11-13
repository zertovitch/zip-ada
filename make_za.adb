--  This is a HAC script *and* an Ada program (your choice).
--
--  The HAX command-line tool can be built from the project HAC @
--    https://hacadacompiler.sourceforge.io/ ,
--    https://github.com/zertovitch/hac
--
--  Usage: hax make_za.adb [build mode] [full]
--
--    Build mode can be Debug, Fast, Small, ...

with HAC_Pack;  use HAC_Pack;
--  NB: the "full Ada" package for HAC_Pack is in /src in the HAC project

procedure Make_ZA is
  gprbuild_options : VString;

  procedure Build_with_nice_title (gpr_name, topic : VString) is
    r : Integer;
  begin
    Put_Line (" ________  ___   ______       ______      ___");
    Put_Line ("/___..._/  I.I   I.___.\     /. __ .\   __I.I   ____");
    Put_Line ("   /../    I.I   I.____/     I.I__I.I  /....I  __\..\ ");
    Put_Line (" _/../___  I.I   I.I    ===  I..__..I I. = .I I = ..I");
    Put_Line ("/_______/  I_I  /__I        /__I  I_I  \__\_I  \__\_I");
    New_Line;
    Put_Line ("/=====================================================\");
    Put_Line ("   Build in progress: " & topic & "...");
    Put_Line ("\=====================================================/");
    New_Line;
    r := Shell_Execute ("gprbuild -P " & gpr_name & gprbuild_options);
  end Build_with_nice_title;

  type OS_Kind is (Any, Windoze);
  k : OS_Kind;

begin
  if Index (Get_Env ("OS"), "Windows") > 0 then
    k := Windoze;
  else
    k := Any;
  end if;
  if Argument_Count > 0 then
    gprbuild_options := " -XZip_Build_Mode=" & Argument (1);
  end if;
  case k is
    when Windoze =>
      gprbuild_options := gprbuild_options & " -XZip_OS=Win64 -largs -Xlinker --stack=0x2000000,0x20000";
    when others =>
      null;
  end case;

  --  For Windows binary distribution with icons and Set_Modification_Time in UnZipAda:
  --
  --  -largs extras/zip_icons.rbj
  --  copy /b extras\lib*.a obj_[mode]

  Build_with_nice_title (+"zipada", +"Command-line Tools and Demos");
  
  if Argument_Count >1 then
    Build_with_nice_title (+"zipada_test", +"Tests and other Tools");
  end if;

end Make_ZA;
