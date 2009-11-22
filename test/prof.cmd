echo Profiling of Zip-Ada
rem
rem Assumes everything compiled with prof_za.cmd or zipada.grp, mode=profiling

set zip=c:\archives\aide-1.03.zip

..\unzipada -t %zip%
gprof ..\unzipada.exe >unzipada.pro
