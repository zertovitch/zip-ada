@echo off

hac bench.adb %1 %2 %3 %4 %5 %6 %7 %8 %9

rem Post-benchmarking administration

dir /OS- bench_%1_*.*

rem Wrapping it all - update archive with all archives 
rem Funnily, upon zipping bench_matrix_*.zip, level 1-8 stores everything, but level 9 deflates to 26% !

zip -9 all_bench_%1.zip bench_%1_*.zip bench_%1_*.lzma bench_%1_*.7z bench_%1_*.zst
echo.
echo ***************************************************************
echo ********** Test the Zip archives, bench_%1_*.zip
echo ***************************************************************
echo.
if exist bench_%1_*.zip 7z t bench_%1_*.zip | tail
echo ***************************************************************
if exist bench_%1_*.zip dir /OS- bench_%1_*.zip
echo.>>bench_%1.log
echo -- [%1] [%date% %time%] -- 
echo -- [%1] [%date% %time%] -- >>bench_%1.log

rem  Delete individual method-related archives
if exist bench_%1_*.zip  del bench_%1_*.zip
if exist bench_%1_*.lzma del bench_%1_*.lzma
if exist bench_%1_*.7z   del bench_%1_*.7z
if exist bench_%1_*.zst  del bench_%1_*.zst
if exist bench_%1.tar    del bench_%1.tar

unzip -v all_bench_%1.zip | find ".zip"  | find "%%"  >bench_%1_.log
unzip -v all_bench_%1.zip | find ".7z"   | find "%%" >>bench_%1_.log
unzip -v all_bench_%1.zip | find ".lzma" | find "%%" >>bench_%1_.log
unzip -v all_bench_%1.zip | find ".zst"  | find "%%" >>bench_%1_.log

more bench_%1_.log | sort >>bench_%1.log
more bench_%1_.log | sort
del bench_%1_.log
