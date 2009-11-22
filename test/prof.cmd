echo Profiling of Zip-Ada
rem
rem Assumes everything compiled with prof_za.cmd or zipada.grp, mode=profiling

set zip=c:\archives\aide-1.03.zip

timeit ..\unzipada -t %zip% >unzipada.out
gprof ..\unzipada.exe >unzipada.pro

del r:\temp\big.zip
timeit ..\zipada r:\temp\big.zip r:\temp\*.tmp >r:\temp\za.txt
gprof ..\zipada.exe >zipada.pro
