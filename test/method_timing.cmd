@echo off

echo.
echo Timing various 'methods' (formats)
echo.

rem The 'timeit' tool is available e.g. with the
rem Windows Server 2003 Resource Kit Tools
rem http://www.microsoft.com/en-us/download/details.aspx?id=17657

copy /b ..\zipada.exe      .
copy /b ..\unzipada.exe    .

rem Try temp directory in a RAM-disk; if not available, use normal temp dir
set mytemp=R:\temp
if not exist %mytemp%\nul if not exist %temp%\methtime mkdir %temp%\methtime
if not exist %mytemp%\nul set mytemp=%temp%\methtime

if exist %mytemp%\test_*.zip del %mytemp%\test_*.zip

rem Have a badly compressible file
zip -9 $random$.zip *.ads

call :set_files

echo.
echo *** Compression ***
echo.

timeit -f .\method.dat -k encode_za_shrink    .\zipada.exe -esh %mytemp%\test_zash %files%

timeit -f .\method.dat -k encode_za_reduce_1  .\zipada.exe -er1 %mytemp%\test_zar1 %files%
timeit -f .\method.dat -k encode_za_reduce_4  .\zipada.exe -er4 %mytemp%\test_zar4 %files%

timeit -f .\method.dat -k encode_za_deflate_f .\zipada.exe -edf %mytemp%\test_zadf %files%
timeit -f .\method.dat -k encode_za_deflate_1 .\zipada.exe -ed1 %mytemp%\test_zad1 %files%
timeit -f .\method.dat -k encode_za_deflate_3 .\zipada.exe -ed3 %mytemp%\test_zad3 %files%

timeit -f .\method.dat -k encode_iz_deflate_1 zip.exe -1     %mytemp%\test_def1 %files%
timeit -f .\method.dat -k encode_iz_deflate_9 zip.exe -9     %mytemp%\test_def9 %files%

timeit -f .\method.dat -k encode_iz_bzip2_1 zip.exe -1 -Z bzip2  %mytemp%\test_bzp2_1 %files%
timeit -f .\method.dat -k encode_iz_bzip2_9 zip.exe -9 -Z bzip2  %mytemp%\test_bzp2_9 %files%

timeit -f .\method.dat -k encode_za_lzma_1    .\zipada.exe -el1 %mytemp%\test_zal1 %files%
timeit -f .\method.dat -k encode_za_lzma_3    .\zipada.exe -el3 %mytemp%\test_zal3 %files%

timeit -f .\method.dat -k encode_za_presel_1  .\zipada.exe -ep1 %mytemp%\test_zap1 %files%
timeit -f .\method.dat -k encode_za_presel_2  .\zipada.exe -ep2 %mytemp%\test_zap2 %files%

timeit -f .\method.dat -k encode_7z_lzma      7z.exe a -tzip -mx9 -mm=lzma      %mytemp%\test_7z_lzma %files%

echo.
echo *** Decompression ***
echo.

call :set_files %mytemp%\

call :clean_unpacked
timeit -f .\method.dat -k decode_shrink        .\unzipada -d %mytemp% %mytemp%\test_zash.zip
call :clean_unpacked
timeit -f .\method.dat -k decode_reduce_4      .\unzipada -d %mytemp% %mytemp%\test_zar4.zip
call :clean_unpacked
timeit -f .\method.dat -k decode_deflate_1_za  .\unzipada -d %mytemp% %mytemp%\test_def1.zip
call :clean_unpacked
timeit -f .\method.dat -k decode_deflate_9_za  .\unzipada -d %mytemp% %mytemp%\test_def9.zip
call :clean_unpacked
timeit -f .\method.dat -k decode_deflate_9_iz       unzip -d %mytemp% %mytemp%\test_def9.zip
call :clean_unpacked
timeit -f .\method.dat -k decode_deflate_9_7z    7z.exe x -o%mytemp%  %mytemp%\test_def9.zip
call :clean_unpacked
timeit -f .\method.dat -k decode_bzip2_1_za    .\unzipada -d %mytemp% %mytemp%\test_bzp2_1.zip
call :clean_unpacked
timeit -f .\method.dat -k decode_pres_2_za     .\unzipada -d %mytemp% %mytemp%\test_zap2.zip
call :clean_unpacked
timeit -f .\method.dat -k decode_pres_2_7z       7z.exe x -o%mytemp%  %mytemp%\test_zap2.zip

echo Produce results
echo.

rem --------------------------
rem Nice date YYYY-MM-DD_HH.MM
rem --------------------------
set year=%date:~-4,4%
set month=%date:~-7,2%
if "%month:~0,1%" equ " " set month=0%month:~1,1%
set day=%date:~-10,2%
if "%day:~0,1%" equ " " set day=0%day:~1,1%
set hour=%time:~0,2%
if "%hour:~0,1%" equ " " set hour=0%hour:~1,1%
set min=%time:~3,2%
set nice_date=%year%-%month%-%day%_%hour%.%min%
rem --------------------------

timeit -f .\method.dat -t 2>method_stat_%nice_date%.log
dir /OS %mytemp%\test_*.zip |find ".zip" >method_list_%nice_date%.log

goto :EOF

:set_files
set files=%1*.ad* %1*.txt %1*.exe %1*.cmd %1$random$.zip %1*.bmp %1*.csv %1*.pdf %1*.html %1*.bin
goto :EOF

:clean_unpacked
attrib -R %mytemp%\*
del %files%
goto :EOF
