echo Profiling of Zip-Ada
rem
rem Assumes everything compiled with prof_za.cmd or zipada.gpr, build_mode=profiling

set bigzip=big.zip
set files=*.mix *.ad* *.txt *.cmd *.bmp *.csv *.pdf *.html *.mdb *.bin

if exist %bigzip% del %bigzip%

..\zipada -ed1 %bigzip% %files% >za.out
gprof ..\zipada.exe >zipada.pro

..\unzipada -t %bigzip% >uza.out
gprof ..\unzipada.exe >unzipada.pro
