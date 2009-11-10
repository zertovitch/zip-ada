@echo off
echo.
echo Testing the ZipAda tool with Comp_Zip
echo.

if exist test_z???.zip del test_z???.zip

rem Have a badly compressible file
zip -9 $random$.zip *.ads

set files=*.ad* *.txt *.exe *.cmd $random$.zip

zipada      test_zash %files%
zipada -er1 test_zar1 %files%
zipada -er2 test_zar2 %files%
zipada -er3 test_zar3 %files%
zipada -er4 test_zar4 %files%
zip         test_zdef %files%

echo.
comp_zip test_zash test_zdef
comp_zip test_zash test_zar1
comp_zip test_zash test_zar2
comp_zip test_zash test_zar3
comp_zip test_zash test_zar4

pause