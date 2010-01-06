rem ************* Pack Zip-Ada Win32 binaries - tools only.

rem del *.exe

call opti_za

rem goto skip_upx
upx --ultra-brute comp_zip.exe
upx --ultra-brute find_zip.exe
upx --ultra-brute rezip.exe
upx --ultra-brute unzipada.exe
upx --ultra-brute zipada.exe
upx --ultra-brute bunzip.exe
:skip_upx

set version=37

zip zipada%version%-bin-win32.zip comp_zip.exe find_zip.exe rezip.exe unzipada.exe zipada.exe bunzip.exe

rezip -defl -comp zipada%version%-bin-win32.zip

del zipada%version%-bin-win32.zip
ren zipada%version%-bin-win32.repacked.zip zipada%version%-bin-win32.zip

echo This archive contains tools from Zip-Ada built as Windows 32-bit command line executables | zip -z zipada%version%-bin-win32.zip

pause
