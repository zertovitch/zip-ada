@echo off

echo.
echo Timing various 'methods' (formats)
echo.

rem The 'timeit' tool is available e.g. with the
rem Windows Server 2003 Resource Kit Tools
rem http://www.microsoft.com/downloads/details.aspx?FamilyID=9D467A69-57FF-4AE7-96EE-B18C4790CFFD

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

timeit -f .\methods.dat -k encode_za_shrink    zipada.exe -esh %mytemp%\test_zash %files%

timeit -f .\methods.dat -k encode_za_reduce_1  zipada.exe -er1 %mytemp%\test_zar1 %files%
timeit -f .\methods.dat -k encode_za_reduce_2  zipada.exe -er2 %mytemp%\test_zar2 %files%
timeit -f .\methods.dat -k encode_za_reduce_3  zipada.exe -er3 %mytemp%\test_zar3 %files%
timeit -f .\methods.dat -k encode_za_reduce_4  zipada.exe -er4 %mytemp%\test_zar4 %files%

timeit -f .\methods.dat -k encode_za_deflate_f zipada.exe -edf %mytemp%\test_zadf %files%

timeit -f .\methods.dat -k encode_iz_deflate_1 zip.exe -1     %mytemp%\test_def1 %files%
timeit -f .\methods.dat -k encode_iz_deflate_6 zip.exe -6     %mytemp%\test_def6 %files%
timeit -f .\methods.dat -k encode_iz_deflate_9 zip.exe -9     %mytemp%\test_def9 %files%

timeit -f .\methods.dat -k encode_7z_deflate64 7z.exe a -tzip -mx9 -mm=deflate64 %mytemp%\test_df64 %files%

timeit -f .\methods.dat -k encode_iz_bzip2_1 zip.exe -1 -Z bzip2  %mytemp%\test_bzp1 %files%
timeit -f .\methods.dat -k encode_iz_bzip2_6 zip.exe -6 -Z bzip2  %mytemp%\test_bzp6 %files%
timeit -f .\methods.dat -k encode_iz_bzip2_9 zip.exe -9 -Z bzip2  %mytemp%\test_bzp9 %files%

echo.
echo *** Decompression ***
echo.

call :set_files %mytemp%\

attrib -R %mytemp%\*
del %files%
timeit -f .\methods.dat -k decode_shrink     unzipada -d %mytemp% %mytemp%\test_zash
attrib -R %mytemp%\*
del %files%
timeit -f .\methods.dat -k decode_reduce_1   unzipada -d %mytemp% %mytemp%\test_zar1
attrib -R %mytemp%\*
del %files%
timeit -f .\methods.dat -k decode_reduce_2   unzipada -d %mytemp% %mytemp%\test_zar2
attrib -R %mytemp%\*
del %files%
timeit -f .\methods.dat -k decode_reduce_3   unzipada -d %mytemp% %mytemp%\test_zar3
attrib -R %mytemp%\*
del %files%
timeit -f .\methods.dat -k decode_reduce_4   unzipada -d %mytemp% %mytemp%\test_zar4
attrib -R %mytemp%\*
del %files%
timeit -f .\methods.dat -k decode_deflate_f  unzipada -d %mytemp% %mytemp%\test_zadf
attrib -R %mytemp%\*
del %files%
timeit -f .\methods.dat -k decode_deflate_1  unzipada -d %mytemp% %mytemp%\test_def1
attrib -R %mytemp%\*
del %files%
timeit -f .\methods.dat -k decode_deflate_6  unzipada -d %mytemp% %mytemp%\test_def6
attrib -R %mytemp%\*
del %files%
timeit -f .\methods.dat -k decode_deflate_9  unzipada -d %mytemp% %mytemp%\test_def9
attrib -R %mytemp%\*
del %files%
timeit -f .\methods.dat -k decode_iz_defl_9  unzip -d %mytemp% %mytemp%\test_def9
attrib -R %mytemp%\*
del %files%
timeit -f .\methods.dat -k decode_deflate64  unzipada -d %mytemp% %mytemp%\test_df64
attrib -R %mytemp%\*
del %files%
timeit -f .\methods.dat -k decode_bzip2_1  unzipada -d %mytemp% %mytemp%\test_bzp1

echo Produce results
echo.

timeit -f .\methods.dat -t 2>methods.log

pause

goto :EOF

:set_files
set files=%1*.ad* %1*.txt %1*.exe %1*.cmd %1$random$.zip %1*.bmp
