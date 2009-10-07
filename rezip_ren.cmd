@echo off

if exist %1 set arch345239874621=%1
if exist %1 goto ok

if exist %1.zip set arch345239874621=%1.zip
if exist %1.zip goto ok

echo Archive %1 or %1.zip not found!
goto fin

:ok

rem Name only, no extension
for %%z in (%1) do set name345239874621=%%~nz

if exist %name345239874621%.old.zip del %name345239874621%.old.zip
copy /b %arch345239874621% %name345239874621%.old.zip

del *.tmp
del $temp$.zip

rezip -comp %1

del %arch345239874621%
move %name345239874621%.repacked.zip %arch345239874621%

set name345239874621=
set arch345239874621=

:fin
