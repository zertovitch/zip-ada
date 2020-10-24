--  This is a HAC script *and* an Ada program (your choice).
--
--  The HAX command-line tool can be built from the project HAC @
--    https://hacadacompiler.sourceforge.io/ ,
--    https://github.com/zertovitch/hac

with HAC_Pack;  use HAC_Pack;
--  NB: the "full Ada" package for HAC_Pack is in /src in the HAC project

procedure Test_ZA is

  type Action_Type is (Create, Compare);

  procedure Process_Archive (command, archive : VString; action : Action_Type) is
    full_name : VString := archive & ".zip";
    ref_archive : VString := +"test_zash";
    files : VString := +"*.ad* *.txt *.cmd *.sh *.bin *.pdf *.xls";
    --  Orig. set: *.mix *.ad* *.txt *.cmd *.bmp *.csv *.pdf *.html *.bin *.xls
    r : Integer;
  begin
    case action is
      when Create =>
        if Exists (full_name) then
          Delete_File (full_name);
        end if; 
        r := Shell_Execute (command & ' ' & full_name & ' ' & files);
      when Compare =>
        r := Shell_Execute (+".." & Directory_Separator & 
                            "comp_zip " & ref_archive & " " & archive & " -q2");
        --  !!  To do: comp_zip should set a code if comparison fails.
    end case;
  end Process_Archive;

  slow : Boolean := False;
  full : Boolean := False;
  r : Integer;

begin
  Put_Line ("Testing the ZipAda tool.");
  New_Line;
  Put_Line ("A call to Comp_Zip is done to compare archives.");
  New_Line;
  Put_Line ("Usage: hax test_za.adb [option]");
  New_Line;
  Put_Line ("  Options: slow : test some exotic / slow formats");
  Put_Line ("           full : all zippers, including very slow ones");
  New_Line;
  if Argument_Count > 0 then
    full := Argument (1) = "full";
    slow := full or (Argument (1) = "slow");
  end if;
  --  Have badly compressible files
  if not Exists ("rnd_0.bin") then
    for i in 0 .. 10 loop
      r := Shell_Execute (+".." & Directory_Separator & "random_data " & i*10 & " rnd_" & i*10 & ".bin");
    end loop;
    for i in 1 .. 9 loop
      r := Shell_Execute (+".." & Directory_Separator & "random_data " & i & " rnd_" & i & ".bin");
    end loop;
  end if;
  if not Exists ("rand_alpha.txt") then
    r := Shell_Execute (+".." & Directory_Separator & "random_data 77777 rand_alpha.txt 65 90");
  end if;
  --  We generate a "large" random file (will take more than 1 Deflate block in random_and_text.mix)
  if not Exists ("random.bin") then
    r := Shell_Execute (+".." & Directory_Separator & "random_data 66666");
  end if;
  --  if not exist random_and_text.mix copy /b random.bin+*.txt+..\doc\*.txt random_and_text.mix
 
  for action in Action_Type loop
    if slow then
      --
      --  If you need a coffee now, it's the right time to have one or two...
      --
      Process_Archive (+".." & Directory_Separator & "zipada -er1", +"test_zar1", action);
      Process_Archive (+".." & Directory_Separator & "zipada -er2", +"test_zar2", action);
      Process_Archive (+".." & Directory_Separator & "zipada -er3", +"test_zar3", action);
      Process_Archive (+".." & Directory_Separator & "zipada -er4", +"test_zar4", action);
    end if;           
    Process_Archive   (+".." & Directory_Separator & "zipada -esh", +"test_zash", action);
    Process_Archive   (+".." & Directory_Separator & "zipada -edf", +"test_zadf", action);
    Process_Archive   (+".." & Directory_Separator & "zipada -ed1", +"test_zad1", action);
    Process_Archive   (+".." & Directory_Separator & "zipada -ed2", +"test_zad2", action);
    Process_Archive   (+".." & Directory_Separator & "zipada -ed3", +"test_zad3", action);
    Process_Archive   (+".." & Directory_Separator & "zipada -el1", +"test_zal1", action);
    Process_Archive   (+".." & Directory_Separator & "zipada -el2", +"test_zal2", action);
    Process_Archive   (+".." & Directory_Separator & "zipada -el3", +"test_zal3", action);
    Process_Archive   (+".." & Directory_Separator & "zipada -ep1", +"test_zap1", action);
    Process_Archive   (+".." & Directory_Separator & "zipada -ep2", +"test_zap2", action);
    Process_Archive   (+"zip           -1              ",           +"test_izd1", action);
    Process_Archive   (+"zip           -6              ",           +"test_izd6", action);
    Process_Archive   (+"zip           -9              ",           +"test_izd9", action);
    Process_Archive   (+"zip           -9 -Z bzip2     ",           +"test_izb9", action);
    Process_Archive   (+"7z a -tzip -mx=9 -mm=deflate  ",           +"test_7z_d", action);
    Process_Archive   (+"7z a -tzip -mx=9 -mm=LZMA     ",           +"test_7z_l", action);
    if full then
      --
      --  Now, a good lunch is recommended...
      --
      Process_Archive (+"kzip",                                     +"test_kzip", action);
      Process_Archive (+"advzip -a -4",                             +"test_zopf", action);
    end if;
  end loop;
end Test_ZA;
