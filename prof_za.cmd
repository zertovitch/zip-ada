@echo off

echo.
echo ** Profiling Zip-Ada.

rem  The HAC command-line tool can be built from the project HAC @
rem    https://hacadacompiler.sourceforge.io/ ,
rem    https://github.com/zertovitch/hac

hac make_za.hac Profiling %1
if "%ERRORLEVEL%"=="0" goto :instr

echo.
echo   You need here hac.exe, the Ada scripting tool.
echo   Check...
echo     https://hacadacompiler.sourceforge.io/
echo   or
echo     https://github.com/zertovitch/hac

pause

goto :eof

:instr
echo run unzipada, then type "gprof unzipada.exe".
