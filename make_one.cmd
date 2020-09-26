@echo off

if "%1" == "" echo use debg_za, opti_za or prof_za
if "%1" == "" echo.
if "%1" == "" pause
if "%1" == "" goto fin

rem Sub-batch of make_za.bat

echo.
echo  ________  ___   ______       ______      ___
echo /___..._/  I.I   I.___.\     /. __ .\   __I.I   ____
echo    /../    I.I   I.____/     I.I__I.I  /....I  __\..\
echo  _/../___  I.I   I.I    ===  I..__..I I. = .I I = ..I
echo /_______/  I_I  /__I        /__I  I_I  \__\_I  \__\_I
echo.
echo /=====================================================\
echo    The Making of... %1
echo \=====================================================/
echo.

set linkoptions_7245=-largs -Xlinker --stack=0x2000000,0x20000

gnatmake -PZipAda.gpr %1 -XZip_Build_Mode=%gnatoptions_87312863% %linkoptions_7245%

rem -largs extras/zip_icons.rbj

:fin