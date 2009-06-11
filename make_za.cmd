@echo off

rem Option-neutral build command for the GNAT compiler

if not "%gnatoptions_87312863%" == "" goto build_them
echo.
echo Please use one of the build commands with compiler options:
echo.
echo    debg_za, for debugging
echo    opti_za, optimized
echo or prof_za, for profiling
echo.
pause
goto fin

:build_them

rem The Tools...

call make_one zipada %1
call make_one unzipada
call make_one rezip
call make_one comp_zip
call make_one find_zip
call make_one demo_zip
call make_one demo_unzip
call make_one ziptest
call make_one demo_csv_into_zip

set gnatoptions_87312863=

if exist b~*.* del b~*.*
if exist b_*.* del b_*.*

:fin