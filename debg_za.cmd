@echo off

echo.
echo ** Debug mode **

if exist obj_dbg\*.a goto skip_obj_setup
rem md obj_dbg
rem This is just for Set_Modification_Time, using non-standard Ada_Directories_Extensions and Win32Ada.
copy /b extras\lib*.a obj_dbg

:skip_obj_setup

rem  The HAC command-line tool can be built from the project HAC @
rem    https://hacadacompiler.sourceforge.io/ ,
rem    https://github.com/zertovitch/hac

hac make_za.adb Debug %1
if "%ERRORLEVEL%"=="0" goto :eof

echo.
echo   You need here hac.exe, the Ada scripting tool.
echo   Check...
echo     https://hacadacompiler.sourceforge.io/
echo   or
echo     https://github.com/zertovitch/hac

