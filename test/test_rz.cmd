@echo off
echo.
echo Testing the ReZip tool - only internal compression methods.
echo.
echo A call to Comp_Zip is done to compare archives (before / after)
echo and check that their uncompressed contents are identical.
echo.

copy /b ..\zipada.exe .
copy /b ..\rezip.exe .

if exist test_rz.zip          del test_rz.zip
if exist test_rz.repacked.zip del test_rz.repacked.zip
if exist test_rz.ReZip.html   del test_rz.ReZip.html
if exist test_rz.ReZip.log    del test_rz.ReZip.log

rem  Create an archive with default compression method (fast).

zipada test_rz *.ad* *.txt *.cmd *.sh *.pdf *.mdb *.xls *.au

rem  Recompress the archive.

rezip -int %1 %2 %3 %4 %5 %6 %7 %8 %9 test_rz

rem  Show recompression report.

start test_rz.ReZip.html

rem  Compare before vs. after.

comp_zip test_rz test_rz.repacked -q2

pause
