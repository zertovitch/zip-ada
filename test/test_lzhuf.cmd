if "%1"=="" goto fin

rem  Custom temp dir
set temp0=%temp%

rem  Compress
..\lz e %1 %temp0%\lzhuf.dat
rem  Decompress
..\lz d %temp0%\lzhuf.dat %temp0%\lzhuf_dec.dat 

rem  Test against original
fc /b %temp0%\lzhuf_dec.dat %1

:fin
pause