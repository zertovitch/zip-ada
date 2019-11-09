if "%1"=="" goto fin

rem  Custom temp dir
set temp0=%temp%

rem  Compress
..\lzma_enc %1 %temp0%\lzma_dat
rem  Decompress
..\lzma_dec %temp0%\lzma_dat.lzma %temp0%\lzma_dec.dat 

rem  Test against original
fc /b %temp0%\lzma_dec.dat %1

:fin
pause