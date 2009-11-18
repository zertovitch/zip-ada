@echo off

rem Try temp directory in a RAM-disk; if not available, use normal temp dir
set mytemp=R:\temp
if exist %mytemp%\nul set temp=%mytemp%
if exist %mytemp%\nul set tmp=%mytemp%

rem Name only, no extension
for %%z in (%1) do set name=%%~dz%%~pz%%~nz

if exist %name%.old.zip del %name%.old.zip
copy /b %1 %name%.old.zip

set iter=0
set size=xxx
set size1=xxx
set size2=xxx
set size3=xxx

:loop
  set size4=%size3%
  set size3=%size2%
  set size2=%size1%
  set size1=%size%
  for %%z in (%1) do set size=%%~zz
  echo *************** Archive size is: %size%
  rezip %2 %3 %4 %1
  del %1
  for %%z in (%1) do move "%%~dz%%~pz%%~nz.repacked.zip" "%%~dz%%~pz%%~nz.zip"
  set /A iter=%iter%+1
  echo *************** Done iteration # %iter%

  rem We let a chance, for a few iterations with the same size,
  rem to a recompression with a lucky random parameter


  if "%size4%"=="%size%" goto fin


goto loop

:fin

echo *************** Now the archive is really squeezed !

comp_zip %name%.old.zip %1
