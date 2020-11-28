rem --------------------------
rem Nice date YYYY-MM-DD_HH.MM
rem --------------------------

set year=%date:~-4,4%

set month=%date:~-7,2%
if "%month:~0,1%" equ " " set month=0%month:~1,1%

set day=%date:~-10,2%
if "%day:~0,1%" equ " " set day=0%day:~1,1%

set hour=%time:~0,2%
if "%hour:~0,1%" equ " " set hour=0%hour:~1,1%

set min=%time:~3,2%

set nice_date=%year%-%month%-%day%_%hour%.%min%
set nice_date=%year%-%month%-%day%

rem --------------------------

rem call clean_za
cd ..
cd ..

set ver=%1
if "%1"=="" echo *** No revision number given, putting XXX
if "%1"=="" set ver=XXX

set root=za

set files=%root%/z*.txt %root%/*.gpr %root%/*.prj %root%/*.pra
rem Ada sources
set files=%files% %root%/zip_lib/*.ad*
set files=%files% %root%/test/*.ad*
set files=%files% %root%/demo/*.ad* 
set files=%files% %root%/trained/*.ad* %root%/trained/*.gpr
rem Documentation
set files=%files% %root%/doc/*.txt %root%/doc/za*.xls %root%/doc/za*.pdf
rem Tools
set files=%files% %root%/tools/*.ad* %root%/tools/rez*.cmd %root%/tools/verif.* %root%/tools/adactl*.cmd %root%/tools/save.cmd %root%/tools/clean*.cmd
rem Extras
set files=%files% %root%/extras/*.ad* %root%/extras/*.a %root%/extras/*.rc %root%/extras/*.rbj %root%/extras/*.ico %root%/extras/*.pl %root%/extras/w*.cmd
rem Scripts
set files=%files% %root%/*_za.cmd %root%/make_one.cmd %root%/*.sh
set files=%files% %root%/test/za_gcov.cmd %root%/test/test*.cmd %root%/test/bench*.cmd %root%/test/prof.cmd %root%/test/*.sh

zipada -ep2 za_%nice_date%_%ver%.zip %files%

cd %root%/tools
