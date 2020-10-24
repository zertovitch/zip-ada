@echo off

echo.
echo ** Debug mode **

if exist obj_dbg\*.a goto skip_md
md obj_dbg
copy /b extras\lib*.a obj_dbg

:skip_md

rem  The HAX command-line tool can be built from the project HAC @
rem    https://hacadacompiler.sourceforge.io/ ,
rem    https://github.com/zertovitch/hac

hax make_za.adb Debug
