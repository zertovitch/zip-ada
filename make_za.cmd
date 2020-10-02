@echo off

rem  Option-neutral build command for the GNAT compiler

if not "%gnatoptions_87312863%" == "" set gnatoptions_87312863=-XZip_Build_Mode=%gnatoptions_87312863%

:build_them

rem  Build the Tools...

call :make_one zipada
call :make_one unzipada
call :make_one rezip
call :make_one comp_zip
call :make_one find_zip

rem  Build the Demos and Tests...

set linkoptions_7245=-largs -Xlinker --stack=0x2000000,0x20000

gprbuild -p -Pzipada.gpr      %linkoptions_7245%
gprbuild -p -Pzipada_test.gpr %linkoptions_7245%

set gnatoptions_87312863=

if exist b~*.* del b~*.*
if exist b_*.* del b_*.*

goto fin

:make_one

echo.
echo  ________  ___   ______       ______      ___
echo /___..._/  :.:   :.___.\     /. __ .\   __:.:   ____
echo    /../    :.:   :.____/     :.:__:.:  /....:  __\..\
echo  _/../___  :.:   :.:    ===  :..__..: :. = .: : = ..:
echo /_______/  :_:  /__:        /__:  :_:  \__\_:  \__\_:
echo.
echo /=====================================================\
echo    The Making of... %1
echo \=====================================================/
echo.

set linkoptions_7245=-largs -Xlinker --stack=0x2000000,0x20000

gnatmake -PZipAda.gpr %1 %linkoptions_7245%

rem -largs extras/zip_icons.rbj

:fin
