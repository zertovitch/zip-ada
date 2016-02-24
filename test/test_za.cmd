@echo off
echo.
echo Testing the ZipAda tool with Comp_Zip
echo.

if exist test_za??.zip del test_za??.zip
if exist test_ifz?.zip del test_ifz?.zip

copy /b ..\zipada.exe .
copy /b ..\random_data.exe .

rem Have a badly compressible file (random.bin), a bit more than 2**n because of rare repetitions
if not exist random.bin random_data 16500
if not exist random_and_text.mix copy /b random.bin+*.txt random_and_text.mix
if exist test_rz.ReZip.html del test_rz.ReZip.html

set files=*.mix *.ad* *.txt *.cmd *.bmp *.csv *.pdf *.html *.mdb *.bin

zipada -esh test_zash %files%
REM zipada -er1 test_zar1 %files%
REM zipada -er2 test_zar2 %files%
REM zipada -er3 test_zar3 %files%
REM zipada -er4 test_zar4 %files%
zipada -ed1 test_zad1 %files%
zipada -edf test_zadf %files%
zip    -6   test_ifz6 %files%
REM zip    -9   test_ifz9 %files%

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

dir test_za??.zip |find ".zip" >test_za_%nice_date%.log
ren Zip.Compress.Deflate.zcd Zip.Compress.Deflate.%nice_date%.zcd

echo.
REM comp_zip test_zash test_ifz9
REM comp_zip test_zash test_zar1
REM comp_zip test_zash test_zar2
REM comp_zip test_zash test_zar3
REM comp_zip test_zash test_zar4
comp_zip test_zash test_zadf
comp_zip test_zash test_zad1

pause
