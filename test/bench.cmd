@echo off

echo.
echo Benchmark a corpus, stored in directory "%1", using various Zippers
echo.

if "%1" == "" goto instr
if exist "%1" goto installed

if not exist bench_%1.zip echo Directory "%1" and archive "bench_%1.zip" do not exist - stopping benchmark.
if not exist bench_%1.zip goto fin

mkdir %1
cd %1
unzipada ..\bench_%1.zip
cd ..

:installed

rem  Delete individual method-related archives
if exist bench_%1_*.zip del bench_%1_*.zip

cd %1

if not "%2" == "full" goto skip

rem   ### LZW
call zipada -es  ../bench_%1_shrink    *

rem   ### Reduce (LZ & Markov)
call zipada -er4 ../bench_%1_reduce_4  *

rem   ### Deflate (Zip.Compress.Deflate)
if exist Zip.Compress.Deflate.zcd del Zip.Compress.Deflate.zcd
call zipada -edf ../bench_%1_deflate_f *
if exist Zip.Compress.Deflate.zcd del Zip.Compress.Deflate.zcd
call zipada -ed1 ../bench_%1_deflate_1 *
if exist Zip.Compress.Deflate.zcd copy Zip.Compress.Deflate.zcd ..\Zip.Compress.Deflate_1_%1.zcd
if exist Zip.Compress.Deflate.zcd del Zip.Compress.Deflate.zcd
call zipada -ed2 ../bench_%1_deflate_2 *
if exist Zip.Compress.Deflate.zcd copy Zip.Compress.Deflate.zcd ..\Zip.Compress.Deflate_2_%1.zcd
if exist Zip.Compress.Deflate.zcd del Zip.Compress.Deflate.zcd
call zipada -ed3 ../bench_%1_deflate_3 *
if exist Zip.Compress.Deflate.zcd copy Zip.Compress.Deflate.zcd ..\Zip.Compress.Deflate_3_%1.zcd
if exist Zip.Compress.Deflate.zcd del Zip.Compress.Deflate.zcd

rem   ### Deflate, external
zip    -6   ../bench_%1_iz_6      *
zip    -9   ../bench_%1_iz_9      *
kzip        ../bench_%1_kzip      *
7z a -tzip -mm=deflate -mx5  ../bench_%1_7zip_defl_5 *
7z a -tzip -mm=deflate -mfb=258 -mpass=15 -mmc=10000 ../bench_%1_7zip_deflate *
advzip -a -4 ../bench_%1_zopfli.zip *

rem   ### BZip2
zip -9 -Z bzip2 ../bench_%1_bzip2_9 *

rem   ### LZMA, external
7z a -tzip -mm=LZMA:a=2:d=25:mf=bt3:fb=255:lc=7 ../bench_%1_7zip_lzma_bt3 *
7z a -tzip -mm=LZMA -mx9                        ../bench_%1_7zip_lzma_mx9 *

rem   ### LZMA (Zip.Compress.LZMA_E)
..\..\zipada -el1 ../bench_%1_lzma_1 *
..\..\zipada -el2 ../bench_%1_lzma_2 *

:skip
rem   ### LZMA (Zip.Compress.LZMA_E) and Preselection
..\..\zipada -el3 ../bench_%1_lzma_3 *
..\..\zipada -eps ../bench_%1_presel *

cd ..

dir /OS- bench_%1_*.zip

rem Wrapping it all - update archive with all archives 
rem Funnily, upon zipping bench_matrix_*.zip, level 1-8 stores everything, but level 9 deflates to 26% !

zip -9 all_bench_%1.zip bench_%1_*.zip
echo.
echo ***************************************************************
echo ********** Test the archives, bench_%1_*.zip
echo ***************************************************************
echo.
7z t bench_%1_*.zip | tail
echo ***************************************************************
dir /OS- bench_%1_*.zip
echo.>>benchs.log
echo -- [%1] -- >>benchs.log

rem  Delete individual method-related archives
if exist bench_%1_*.zip del bench_%1_*.zip

unzip -v all_bench_%1.zip | find ".zip" | find "%%" | sort
unzip -v all_bench_%1.zip | find ".zip" | find "%%" | sort>>benchs.log

goto fin

:instr
echo Usage
echo   bench directory_name [full]
echo.
echo When "full", redo all (including out-of-focus compression methods)
echo Will produce bench_directory_name_... .zip for each method.
echo.
echo See benchs.cmd running a set of benchmarks in parallel.
echo.
pause
:fin