@echo off
echo.
echo Timing various 'methods' (formats)
echo.

rem Try temp directory in a RAM-disk; if not available, use normal temp dir
set mytemp=R:\temp
if not exist %mytemp%\nul set mytemp=%temp%

if exist %mytemp%\test_????.zip del %mytemp%\test_????.zip

rem Have a badly compressible file
zip -9 $random$.zip *.ads

call :set_files

echo.
echo *** Compression ***
echo.

timeit -f .\methods.dat -k encode_shrink   zipada      %mytemp%\test_zash %files%
timeit -f .\methods.dat -k encode_reduce_1 zipada -er1 %mytemp%\test_zar1 %files%
timeit -f .\methods.dat -k encode_reduce_2 zipada -er2 %mytemp%\test_zar2 %files%
timeit -f .\methods.dat -k encode_reduce_3 zipada -er3 %mytemp%\test_zar3 %files%
timeit -f .\methods.dat -k encode_reduce_4 zipada -er4 %mytemp%\test_zar4 %files%

timeit -f .\methods.dat -k encode_deflate_1 zip -1     %mytemp%\test_def1 %files%
timeit -f .\methods.dat -k encode_deflate_6 zip -6     %mytemp%\test_def6 %files%
timeit -f .\methods.dat -k encode_deflate_9 zip -9     %mytemp%\test_def9 %files%

timeit -f .\methods.dat -k encode_bzip2_1 zip.exe -1 -Z bzip2  %mytemp%\test_bzp1 %files%
timeit -f .\methods.dat -k encode_bzip2_6 zip.exe -6 -Z bzip2  %mytemp%\test_bzp6 %files%
timeit -f .\methods.dat -k encode_bzip2_9 zip.exe -9 -Z bzip2  %mytemp%\test_bzp9 %files%

echo.
echo *** Decompression ***
echo.

call :set_files %mytemp%\

del %files%
timeit -f .\methods.dat -k decode_shrink     unzipada -d %mytemp% %mytemp%\test_zash
del %files%
timeit -f .\methods.dat -k decode_reduce_1   unzipada -d %mytemp% %mytemp%\test_zar1
del %files%
timeit -f .\methods.dat -k decode_reduce_2   unzipada -d %mytemp% %mytemp%\test_zar2
del %files%
timeit -f .\methods.dat -k decode_reduce_3   unzipada -d %mytemp% %mytemp%\test_zar3
del %files%
timeit -f .\methods.dat -k decode_reduce_4   unzipada -d %mytemp% %mytemp%\test_zar4
del %files%
timeit -f .\methods.dat -k decode_deflate_1  unzipada -d %mytemp% %mytemp%\test_def1
del %files%
timeit -f .\methods.dat -k decode_deflate_6  unzipada -d %mytemp% %mytemp%\test_def6
del %files%
timeit -f .\methods.dat -k decode_deflate_9  unzipada -d %mytemp% %mytemp%\test_def9
del %files%
timeit -f .\methods.dat -k decode_info_zip_deflate_9  unzip -d %mytemp% %mytemp%\test_def9

echo Produce results
echo.

timeit -f .\methods.dat -t 2>methods.log

pause

goto :EOF

:set_files
set files=%1*.ad* %1*.txt %1*.exe %1*.cmd %1$random$.zip %1*.bmp
