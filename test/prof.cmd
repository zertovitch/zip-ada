echo Profiling of Zip-Ada
rem
rem Assumes everything compiled with prof_za.cmd or zipada.gpr, build_mode=profiling

set zip=r:\temp\big.zip

del r:\temp\big.zip
timeit ..\zipada r:\temp\big.zip r:\temp\*.tmp >r:\temp\za.txt
gprof ..\zipada.exe >zipada.pro

timeit ..\unzipada -t %zip% >unzipada.out
gprof ..\unzipada.exe >unzipada.pro
