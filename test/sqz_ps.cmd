rem Test with SqueezeChart data set (5,156,295,185 bytes; 21,532 files)
rem http://www.squeezechart.com/

rem Delays tuned 

start /min sqz_ps_one xml
timeout /t 5
start /min sqz_ps_one gut
timeout /t 5

REM These 2 are quick.
call sqz_ps_one aud
call sqz_ps_one cam

start /min sqz_ps_one app
timeout /t 20

start /min sqz_ps_one ins
timeout /t 20

start /min sqz_ps_one mob
timeout /t 20

start /min sqz_ps_one pgm
timeout /t 20

start /min sqz_ps_one src

pause
