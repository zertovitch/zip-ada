@echo off

rem Benchmark a corpus, stored in a directory

if "%1" == "" goto instr

del bench_%1_*.zip

cd %1

rem   ### LZW
zipada      ../bench_%1_shrink    *
rem   ### Reduce (LZ & Markov)
zipada -er4 ../bench_%1_reduce_4  *
rem   ### Deflate
zipada -edf ../bench_%1_deflate_f *
zipada -ed1 ../bench_%1_deflate_1 *
zip    -9   ../bench_%1_iz_9      *
kzip        ../bench_%1_kzip      *
7z a -tzip -mm=deflate -mfb=258 -mpass=15 -mmc=10000 ../bench_%1_7zip_deflate *
advzip -a -4 ../bench_%1_zopfli.zip *
rem   ### LZMA
7z a -tzip -mm=LZMA:a=2:d=25:mf=bt3:fb=255:lc=7 ../bench_%1_7zip_lzma *

cd ..

goto fin

:instr
echo Usage
echo   bench directory_name
echo.
echo Will produce bench_directory_name_... .zip
echo.
echo See benchs.cmd for a set of benchmarks
echo.
pause
:fin