@echo off
echo.
echo Testing the ReZip tool with Comp_Zip
echo.

if exist test_rz.zip del test_rz.zip

zipada test_rz *.ad* *.txt *.cmd

rezip test_rz

start test_rz.ReZip.html

comp_zip test_rz test_rz.repacked

pause