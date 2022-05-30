@echo off

echo.
echo ** Debug mode **

rem  The HAC command-line tool can be built from the project HAC @
rem    https://hacadacompiler.sourceforge.io/ ,
rem    https://github.com/zertovitch/hac

hac make_za.hac Debug %1
if "%ERRORLEVEL%"=="0" goto :eof

echo.
echo   You need here hac.exe, the Ada scripting tool.
echo   Check...
echo     https://hacadacompiler.sourceforge.io/
echo   or
echo     https://github.com/zertovitch/hac

pause
