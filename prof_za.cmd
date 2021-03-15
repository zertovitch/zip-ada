@echo off

echo.
echo ** Profiling Zip-Ada.

if exist obj_pro\*.a goto skip_obj_setup
rem md obj_pro
rem This is just for Set_Modification_Time, using non-standard Ada_Directories_Extensions and Win32Ada.
copy /b extras\lib*.a obj_pro

:skip_obj_setup

rem  The HAC command-line tool can be built from the project HAC @
rem    https://hacadacompiler.sourceforge.io/ ,
rem    https://github.com/zertovitch/hac

hac make_za.adb Profiling %1
if "%ERRORLEVEL%"=="0" goto :instr

echo.
echo   You need here hac.exe, the Ada scripting tool.
echo   Check...
echo     https://hacadacompiler.sourceforge.io/
echo   or
echo     https://github.com/zertovitch/hac

goto :eof

:instr
echo run unzipada, then type "gprof unzipada.exe".
