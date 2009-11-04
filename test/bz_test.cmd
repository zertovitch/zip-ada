@echo off

rem Try temp directory in a RAM-disk; if not available, use normal temp dir
set mytemp=R:\temp
if not exist %mytemp%\nul set mytemp=%temp%

set tempname=%mytemp%\$$temp$$

if "%1"=="" goto fin

echo ****** 1) Create BZip2 archive
copy /B %1 %tempname%
del %tempname%.bz2

bzip2 %tempname%

echo ****** 2) Decompression

echo.
echo *** bzip2 (C version from www.bzip.org)
timeit bzip2 -k -d %tempname%.bz2

echo.
echo *** pasbzip (non crc check, detransform in asm)
timeit pasbzip %tempname%.bz2

echo.
echo *** pasbzip (non crc check, all in freepascal)
timeit pasbzip_pas %tempname%.bz2

rem echo *** bunzip (no write)
rem timeit bunzip_nowrite %tempname%.bz2

echo.
echo *** bunzip (pure Ada)
timeit bunzip %tempname%.bz2

echo ****** 3) Comparison
echo n | comp outfile %tempname%
echo.
echo n | comp bunzip.out %tempname%
echo.
echo n | comp bunzip.out outfile
echo.

dir %1

:fin