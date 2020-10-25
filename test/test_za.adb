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
    ref_archive : VString := +"$mth_zash";
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

  procedure Smuggle (other_dir, file_name: VString) is
    copy_name : VString := "$_" & file_name;
    other_path : VString := +".." & Directory_Separator & other_dir & Directory_Separator & file_name;
  begin
    if not Exists (copy_name) then
      Copy_File (other_path, copy_name);
    end if;
  end Smuggle;

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
    all_zips : VString := +"$mth_*";
    r : Integer;
  begin
    if Index (Get_Env ("OS"), "Windows") > 0 then
      r := Shell_Execute (+"dir /OS " & all_zips & " |find ""$mth"" >" & log_name);
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
  if not Exists ("$rnd_0.bin") then
    for i in 0 .. 10 loop
      r := Shell_Execute (+".." & Directory_Separator & "random_data " & i*10 & " $rnd_" & i*10 & ".bin");
    end loop;
    for i in 1 .. 9 loop
      r := Shell_Execute (+".." & Directory_Separator & "random_data " & i & " $rnd_" & i & ".bin");
    end loop;
  end if;
  if not Exists ("$rand_alpha.txt") then
    r := Shell_Execute (+".." & Directory_Separator & "random_data 77777 $rand_alpha.txt 65 90");
  end if;
  --  We generate a "large" random file (will take more than 1 Deflate block in random_and_text.mix)
  if not Exists ("$random.bin") then
    r := Shell_Execute (+".." & Directory_Separator & "random_data 66666");
  end if;
  --  if not exist random_and_text.mix copy /b random.bin+*.txt+..\doc\*.txt random_and_text.mix
  --
  --  Smuggle some files from the project as test files
  --
  Smuggle (+"doc",     +"za_work.xls");
  Smuggle (+"doc",     +"appnote.txt");
  Smuggle (+"doc",     +"lzma-specification.txt");
  Smuggle (+"doc",     +"za_history.txt");
  Smuggle (+"doc",     +"za_todo.txt");
  Smuggle (+"doc",     +"za_fosdem_2019.pdf");
  Smuggle (+"zip_lib", +"lz77.adb");
  Smuggle (+"zip_lib", +"unzip-decompress.adb");
  Smuggle (+"zip_lib", +"zip-compress-deflate.adb");
  Smuggle (+"zip_lib", +"lzma-encoding.adb");
  --
  for action in Action_Type loop
    if slow then
      --  If you need a coffee now, it's the right time to have one or two...
      Process_Archive (+".." & Directory_Separator & "zipada -er1", +"$mth_zar1", action);
      Process_Archive (+".." & Directory_Separator & "zipada -er2", +"$mth_zar2", action);
      Process_Archive (+".." & Directory_Separator & "zipada -er3", +"$mth_zar3", action);
      Process_Archive (+".." & Directory_Separator & "zipada -er4", +"$mth_zar4", action);
    end if;
    Process_Archive   (+".." & Directory_Separator & "zipada -esh", +"$mth_zash", action);
    Process_Archive   (+".." & Directory_Separator & "zipada -edf", +"$mth_zadf", action);
    Process_Archive   (+".." & Directory_Separator & "zipada -ed1", +"$mth_zad1", action);
    Process_Archive   (+".." & Directory_Separator & "zipada -ed2", +"$mth_zad2", action);
    Process_Archive   (+".." & Directory_Separator & "zipada -ed3", +"$mth_zad3", action);
    Process_Archive   (+".." & Directory_Separator & "zipada -el1", +"$mth_zal1", action);
    Process_Archive   (+".." & Directory_Separator & "zipada -el2", +"$mth_zal2", action);
    Process_Archive   (+".." & Directory_Separator & "zipada -el3", +"$mth_zal3", action);
    Process_Archive   (+".." & Directory_Separator & "zipada -ep1", +"$mth_zap1", action);
    Process_Archive   (+".." & Directory_Separator & "zipada -ep2", +"$mth_zap2", action);
    Process_Archive   (+"zip           -1              ",           +"$mth_izd1", action);
    Process_Archive   (+"zip           -6              ",           +"$mth_izd6", action);
    Process_Archive   (+"zip           -9              ",           +"$mth_izd9", action);
    Process_Archive   (+"zip           -9 -Z bzip2     ",           +"$mth_izb9", action);
    Process_Archive   (+"7z a -tzip -mx=9 -mm=deflate  ",           +"$mth_7z_d", action);
    Process_Archive   (+"7z a -tzip -mx=9 -mm=LZMA     ",           +"$mth_7z_l", action);
    if full then
      --  Now, a good lunch is recommended...
      Process_Archive (+"kzip",                                     +"$mth_kzip", action);
      Process_Archive (+"advzip -a -4",                             +"$mth_zopf", action);
    end if;
    if action = Create then
      --  LZMA, with a "solid" (=files not compressed individually)
      --  archive container, the .7z archive format.
      if Exists ("$mth_7z_l.7z") then
        Delete_File ("$mth_7z_l.7z");
      end if; 
      r := Shell_Execute  (+"7z a -mx=9 $mth_7z_l.7z " & files);
    end if;
    New_Line;
  end loop;
  Create_List;
  Put_Line ("Archive comparisons:");
  Put_Line (+"   " & successes & " successes");
  Put_Line (+"   " & fails & " failures");
  New_Line;
  Put_Line (+"" & zipada_used & " different methods of ZipAda tested.");
end Test_ZA;
