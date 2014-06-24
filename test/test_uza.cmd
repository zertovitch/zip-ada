@echo off
echo.
echo This batch file will perform a small test of UnZipAda tool.
echo Don't hesitate to tune it your way!
echo.
echo You need to have in your path:
echo.
echo   unzipada.exe
echo.
echo The test will try to call the following compressors,
echo they should be also visible in the path:
echo.
echo   pkzip090.exe (or pkzip092.exe)
echo   pkzip101.exe (or pkzip102.exe)
echo   pkzip110.exe
echo   pkzip193.exe
echo   pkzip204.exe
echo   pkzip250.exe
echo   7z.exe
echo   kzip.exe
echo   zip.exe (v.3 or later)
echo.

echo The PK compressors are renamed from "pkzip.exe" in various versions
echo available on the Web. Read ZipAda.txt for details.

echo *****************************************************************
echo.
echo 1) I will use the text files (sources etc.) in the Zip-Ada
echo distribution as compression data
echo.
pause
copy *.ad? + *.txt + *.ali + zip_lib\*.ad? + zip_lib\*.ali + *.bat $test.txt
call test_1 $test.txt tuttifru

echo *****************************************************************
echo.
echo 2) Now, a test with binaries (*.exe)
echo.
pause
copy /B *.exe $test.bin
call test_1 $test.bin binana
