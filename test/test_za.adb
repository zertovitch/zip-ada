--  This is a HAC script *and* an Ada program (your choice).
--
--  The HAX command-line tool can be built from the project HAC @
--    https://hacadacompiler.sourceforge.io/ ,
--    https://github.com/zertovitch/hac

with HAC_Pack;  use HAC_Pack;
--  NB: the "full Ada" package for HAC_Pack is in /src in the HAC project

procedure Test_ZA is

  type Action_Type is (Create, Compare);

  fails, successes, zipada_used : Natural := 0;

  files : VString := +"*.ad* *.txt *.cmd *.pgm *.ppm *.bin *.pdf *.xls *.au";

  procedure Process_Archive (command, archive : VString; action : Action_Type) is
    full_name : VString := archive & ".zip";
    ref_archive : VString := +"test_zash";
    --  Orig. set: *.mix *.ad* *.txt *.cmd *.bmp *.csv *.pdf *.html *.bin *.xls
    r : Integer;
  begin
    case action is
      when Create =>
        if Exists (full_name) then
          Delete_File (full_name);
        end if; 
        r := Shell_Execute (command & ' ' & full_name & ' ' & files);
        if Index (command, "zipada") > 0 then
          zipada_used := zipada_used + 1;
        end if;
      when Compare =>
        if archive /= ref_archive then
          r := Shell_Execute (+".." & Directory_Separator & 
                              "comp_zip " & ref_archive & " " & archive & " -q3");
          if r = 0 then
            successes := successes + 1;
          else
            fails := fails + 1;
          end if;
        end if;
    end case;
  end Process_Archive;

  function Nice_Date (with_intraday : Boolean) return VString is
    t1 : constant Time := Clock;
    day_secs, day_mins : Integer;
    just_day : VString;
    --
    function Two_Digits (x : Integer) return VString is
    begin
      if x < 10 then
        return "0" & Image (x);
      else
        return Image (x);
      end if;
    end Two_Digits;
    --
  begin
    day_secs := Integer (Seconds (t1));
    day_mins := day_secs / 60;
    just_day := +"" &  --  VString concatenation
      Year (t1)  & '-' &
      Two_Digits (Month (t1)) & '-' &
      Two_Digits (Day (t1));
    if with_intraday then
      return just_day & "--" &
      Two_Digits (day_mins / 60) & '-' &
      Two_Digits (day_mins mod 60) & '-' &
      Two_Digits (day_secs mod 60);
    else
      return just_day;
    end if;
  end Nice_Date;

  procedure Create_List is
    log_name : VString := "test_za_" & Nice_Date (True) & ".log";
    all_zips : VString :=
      +"test_za??.zip test_iz??.zip test_7z_?.* test_kzip.zip test_zopf.zip";
    r : Integer;
  begin
    if Index (Get_Env ("OS"), "Windows") > 0 then
      r := Shell_Execute (+"dir /OS " & all_zips & " |find "".zip"" >" & log_name);
    else
      r := Shell_Execute (+"ls -lrS " & all_zips & '>' & log_name);
    end if;
  end Create_List;

  slow, full : Boolean := False;
  r : Integer;

begin
  Put_Line ("Testing the ZipAda tool.");
  New_Line;
  Put_Line ("Calls to Comp_Zip are done to compare archives.");
  New_Line;
  Put_Line ("Usage: hax test_za.adb [option]");
  New_Line;
  Put_Line ("  Options: slow : test some exotic / slow formats in Zip-Ada");
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
  if not Exists ("za_work_copy.xls") then
     Copy_File (
       +".." & Directory_Separator & "doc" & Directory_Separator & "za_work.xls",
       "za_work_copy.xls"
     );
  end if;
  --
  for action in Action_Type loop
    if slow then
      --  If you need a coffee now, it's the right time to have one or two...
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
      --  Now, a good lunch is recommended...
      Process_Archive (+"kzip",                                     +"test_kzip", action);
      Process_Archive (+"advzip -a -4",                             +"test_zopf", action);
    end if;
    New_Line;
  end loop;
  --  LZMA, with a "solid" (=files not compressed individually)
  --  archive container, the .7z archive format.
  r := Shell_Execute  (+"7z a -mx=9 test_7z_l.7z " & files);
  Create_List;
  Put_Line ("Archive comparisons:");
  Put_Line (+"   " & successes & " successes");
  Put_Line (+"   " & fails & " failures");
  New_Line;
  Put_Line (+"" & zipada_used & " different methods of ZipAda tested.");
end Test_ZA;
