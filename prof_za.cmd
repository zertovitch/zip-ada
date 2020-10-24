@echo off

echo.
echo ** Profiling Zip-Ada.

if exist obj_pro\create_dir.txt goto skip_md
md obj_pro
copy /b extras\lib*.a obj_pro

:skip_md

rem  The HAX command-line tool can be built from the project HAC @
rem    https://hacadacompiler.sourceforge.io/ ,
rem    https://github.com/zertovitch/hac

hax make_za.adb Profiling

echo run unzipada, then type "gprof unzipada.exe".
