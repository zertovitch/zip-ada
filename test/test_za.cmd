@echo off
echo.
echo Testing the ZipAda tool with Comp_Zip
echo.

if exist test_z???.zip del test_z???.zip

copy /b ..\zipada.exe .
copy /b ..\random_data.exe .

rem Have a badly compressible file
random_data 1000

set files=*.ad* *.txt *.exe *.cmd *.bmp random.bin

zipada -esh test_zash %files%
zipada -er1 test_zar1 %files%
zipada -er2 test_zar2 %files%
zipada -er3 test_zar3 %files%
zipada -er4 test_zar4 %files%
zipada -edf test_zadf %files%
zip         test_zzip %files%

echo.
comp_zip test_zash test_zzip
comp_zip test_zash test_zar1
comp_zip test_zash test_zar2
comp_zip test_zash test_zar3
comp_zip test_zash test_zar4
comp_zip test_zash test_zadf

pause
