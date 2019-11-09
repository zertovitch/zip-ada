@echo off
echo.
echo Testing ZipTest
echo.

rem Ensure the input files exist
if not exist file1.txt copy ziptest.adb file1.txt
if not exist file2.txt copy ..\doc\appnote.txt file2.txt

..\ziptest

echo.
echo *** Compare the archives
comp_zip to_file to_memo

echo.
echo *** Check the file -) compression -) decompression -)file path
unzipada -j to_memo
fc file1.txt file1_z.txt
fc file2.txt file2_z.txt

pause
