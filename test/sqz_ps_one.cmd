@echo off

echo Test Zip-Ada with one SqueezeChart data set [%1]
echo.

if "%1"=="" echo Usage: sqz_ps_one [set] where set is the directory name with data
if "%1"=="" echo NB: sqz_ps does the whole test (9 archives)
if "%1"=="" goto fin

del %1_ps.zip
zipada -r -eps %1_ps %1
echo.
echo Test the archive, %1_ps.zip
echo.
7z t %1_ps.zip | tail

:fin
