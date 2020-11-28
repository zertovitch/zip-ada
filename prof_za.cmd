@echo off

echo.
echo ** Profiling Zip-Ada.

if exist obj_pro\create_dir.txt goto skip_md
md obj_pro
copy /b extras\lib*.a obj_pro

:skip_md

rem  The HAC command-line tool can be built from the project HAC @
rem    https://hacadacompiler.sourceforge.io/ ,
rem    https://github.com/zertovitch/hac

hac make_za.adb Profiling %1

echo run unzipada, then type "gprof unzipada.exe".
