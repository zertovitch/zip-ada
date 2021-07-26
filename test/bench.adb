--  This is a HAC script *and* an Ada program.
--  How to run it is your choice.
--
--  Usage with the HAC command-line tool :
--
--    hac bench.adb benchmark_name {option}
--
--  The HAC (HAC Ada Compiler) command-line tool can be found @
--    https://hacadacompiler.sourceforge.io/ ,
--    https://github.com/zertovitch/hac

with HAL;
--  NB: in case you want to build this script into an executable with a
--      "full Ada" compiler, the files for HAL (hal.ads, hal*.adb)
--      is located in the /src directory, in the HAC project.

procedure Bench is

  use HAL;

  procedure Blurb is
  begin
    Put_Line ("Bench");
    Put_Line ("  Purpose: to benchmark various zippers on a corpus");
    Put_Line ("  of test files, stored in directory of the corpus's name.");
    New_Line;
    Put_Line ("Usage:  hac bench.adb benchmark_name {option}");
    New_Line;
    Put_Line ("Options:");
    Put_Line ("     full  : test everything (long!)");
    Put_Line ("     7z    : test LZMA with various parameters");
    Put_Line ("     lzma3 : test Zip-Ada's LZMA level 3 (default)");
    Put_Line ("     tar   : test tar + various one-file compressors");
  end Blurb;

  type Category is (
    Reduce_Shrink,
    Deflate,
    Deflate_External,
    BZip2_External,
    PPMd_External,
    LZMA_7z,
    LZMA_3,
    TAR,
    Preselection
  );

  cat_set : array (Category) of Boolean;

  procedure Options is
  begin
    for c in Category loop
      cat_set (c) := False;
    end loop;
    if Argument_Count < 2 then
      cat_set (LZMA_3) := True;
    end if;
    for a in 2 .. Argument_Count loop
      if Argument (a) = "full" then
        for c in Category loop
          cat_set (c) := True;
        end loop;
      elsif Argument (a) = "tar" then
        cat_set (tar) := True;
      elsif Argument (a) = "7z" then
        cat_set (LZMA_7z) := True;
      elsif Argument (a) = "lzma" then
        cat_set (LZMA_3) := True;
      end if;
    end loop;
  end Options;

  ds : Character := Directory_Separator;

  bn : VString;  --  Short name for benchmark name.
  up2, za : VString;

begin
  if Argument_Count = 0 then
    Blurb;
    return;
  end if;
  bn := Argument (1);
  if not Directory_Exists (bn) then
    Put_Line (+"Directory """ & bn & """ doesn't exist.");
    New_Line;
    Blurb;
    return;
  end if;
  Set_Directory (bn);
  Options;
  up2 := +".." & ds & ".." & ds;
  za := up2 & "zipada";
  if cat_set (Reduce_Shrink) then
    Shell_Execute (za & " -es  ../bench_" & bn & "_shrink   *");
    Shell_Execute (za & " -er4 ../bench_" & bn & "_reduce_4 *");
  end if;
  if cat_set (Deflate) then
    Shell_Execute (za & " -edf ../bench_" & bn & "_deflate_f *");
    Shell_Execute (za & " -ed1 ../bench_" & bn & "_deflate_1 *");
    Shell_Execute (za & " -ed2 ../bench_" & bn & "_deflate_2 *");
    Shell_Execute (za & " -ed3 ../bench_" & bn & "_deflate_3 *");
  end if;
  if cat_set (Deflate_External) then
    Shell_Execute (+"zip    -6   ../bench_" & bn & "_iz_6      *");
    Shell_Execute (+"zip    -9   ../bench_" & bn & "_iz_9      *");
    Shell_Execute (+"kzip        ../bench_" & bn & "_kzip      *");
    Shell_Execute (+"7z a -tzip -mm=deflate -mx5  ../bench_" & bn & "_7zip_defl_5 *");
    Shell_Execute (+"7z a -tzip -mm=deflate -mfb=258 -mpass=15 -mmc=10000 ../bench_" & bn & "_7zip_deflate *");
    Shell_Execute (+"advzip -a -4 ../bench_" & bn & "_zopfli.zip *");
  end if;
  if cat_set (BZip2_External) then
    Shell_Execute (+"zip -9 -Z bzip2 ../bench_" & bn & "_bzip2_9 *");
  end if;
  if cat_set (PPMd_External) then
    Shell_Execute (+"7z a -tzip -mmt1 -mm=PPMd -mx9 ../bench_" & bn & "_7zip_ppmd_mx9 *");
  end if;
  if cat_set (LZMA_7z) then
    Shell_Execute (+"7z a -tzip -mmt1 -mm=LZMA:a=2:d=25:mf=bt3:fb=273:lc=7 ../bench_" & bn & "_7zip_lzma_bt3 *");
    Shell_Execute (+"7z a -tzip -mmt1 -mm=LZMA:a=2:d=25:mf=bt4:fb=273:lc=7 ../bench_" & bn & "_7zip_lzma_bt4 *");
    Shell_Execute (+"7z a -tzip -mmt1 -mm=LZMA:a=2:d=25:mf=bt5:fb=273:lc=7 ../bench_" & bn & "_7zip_lzma_bt5 *");
    Shell_Execute (+"7z a -tzip -mmt1 -mm=LZMA:a=2:d=25:mf=hc4:fb=273:lc=7 ../bench_" & bn & "_7zip_lzma_hc4 *");

    Shell_Execute (+"7z a -tzip -mmt1 -mm=LZMA:fb=273:mf=bt3 -mx9          ../bench_" & bn & "_7zip_lzma_bt3mx9 *");
    Shell_Execute (+"7z a -tzip -mmt1 -mm=LZMA:fb=273        -mx9          ../bench_" & bn & "_7zip_lzma_mx9    *");
    Shell_Execute (+"7z a -tzip -mmt1 -mm=LZMA:fb=273:mf=bt5 -mx9          ../bench_" & bn & "_7zip_lzma_bt5mx9 *");
    Shell_Execute (+"7z a       -mmt1 -mm=LZMA:fb=273        -mx9          ../bench_" & bn & "_7zip_lzma_mx9    *");
  end if;
  if cat_set (LZMA_3) then
    Shell_Execute (za & " -el3 ../bench_" & bn & "_lzma_3 *");
  end if;
  if cat_set (TAR) then
    Shell_Execute (+"tar -c -f ../bench_" & bn & ".tar *");
    Shell_Execute (+"lzma e -mt1 ../bench_" & bn & ".tar ../bench_" & bn & "_7zip_lzma_mx9.tar.lzma");
    Shell_Execute (up2 & "lzma_enc ../bench_" & bn & ".tar ../bench_" & bn & "_lzma_3.tar");
    Shell_Execute (+"zstd  --ultra -22 --single-thread ../bench_" & bn & ".tar -o ../bench_" & bn & "_ZStd.tar.zst");
  end if;
  if cat_set (Preselection) then
    Shell_Execute (za & " -el3 ../bench_" & bn & "_prsl_2 *");
  end if;
  Set_Directory ("..");
end Bench;
