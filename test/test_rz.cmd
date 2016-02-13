@echo off
echo.
echo Testing the ReZip tool with Comp_Zip
echo.

copy /b ..\zipada.exe .
copy /b ..\rezip.exe .

if exist test_rz.zip          del test_rz.zip
if exist test_rz.repacked.zip del test_rz.repacked.zip
if exist test_rz.ReZip.html   del test_rz.ReZip.html
if exist test_rz.ReZip.log    del test_rz.ReZip.log

zipada test_rz *.ad* *.txt *.cmd *.pdf *.mdb

rezip test_rz

start test_rz.ReZip.html

comp_zip test_rz test_rz.repacked

pause