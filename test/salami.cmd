@echo off

if "%1" == "" echo Salami: compresses slices of various sizes of a file
if "%1" == "" echo into Zip archives, with different compression methods.
if "%1" == "" echo.
if "%1" == "" echo Usage: salami [sample_file]
if "%1" == "" goto :eof

del *.tmp

several_sizes %1 

zipada -ed1 %1_d1.zip *.tmp %1
zipada -ed2 %1_d2.zip *.tmp %1
zipada -ed3 %1_d3.zip *.tmp %1
zipada -el1 %1_l1.zip *.tmp %1
zipada -el2 %1_l2.zip *.tmp %1
zipada -el3 %1_l3.zip *.tmp %1

rem We would need to give a hint otherwise than with the file extension
rem
rem zipada -ep1 %1_p1.zip *.tmp
rem zipada -ep2 %1_p2.zip *.tmp

del *.tmp

rem  Test

7z t %1_*.zip | tail
dir /os %1_*.zip
