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
echo   pkzip090.exe (or 092), from: pkz090.exe,  10.02.1989, Shrink, Reduce 1-4
echo   pkzip101.exe (or 102), from: pkz101.exe,  21.07.1989, Implode, Shrink
echo   pkzip110.exe,          from: pkz110.exe,  15.03.1990, Implode, Shrink
echo   pkzip193.exe,          from: pkz193a.exe, 15.10.1991, Deflate
echo   pkzip204.exe,          from: pkz204.exe,  25.02.1993, Deflate
echo   pkzip250.exe           from: pkz250.exe,   1.03.1999, Deflate
echo   7z.exe
echo   kzip.exe
echo   zip.exe (v.3 or later, with BZip2 add-on)
echo.

echo The PK compressors are renamed from "pkzip.exe" in various versions
echo available on the Web.

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
