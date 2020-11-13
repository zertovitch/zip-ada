@echo off

echo.
echo ** Optimized mode **

if exist obj_opt\*.a goto skip_md
md obj_opt
copy /b extras\lib*.a obj_opt

:skip_md

rem  The HAX command-line tool can be built from the project HAC @
rem    https://hacadacompiler.sourceforge.io/ ,
rem    https://github.com/zertovitch/hac

hax make_za.adb Fast %1
