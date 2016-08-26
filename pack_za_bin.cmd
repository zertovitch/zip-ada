rem ************* Pack Zip-Ada Win32 binary executables. Tools only, no demos.

rem del *.exe

call opti_za

rem goto skip_upx
upx --ultra-brute comp_zip.exe
upx --ultra-brute find_zip.exe
upx --ultra-brute rezip.exe
upx --ultra-brute unzipada.exe
upx --ultra-brute zipada.exe
upx --ultra-brute bunzip.exe
upx --ultra-brute lzma_dec.exe
upx --ultra-brute lzma_enc.exe
:skip_upx

set version=51

zipada -ed3 zipada%version%-bin-win32.zip comp_zip.exe find_zip.exe rezip.exe unzipada.exe zipada.exe bunzip.exe lzma_dec.exe lzma_enc.exe
deflopt     zipada%version%-bin-win32.zip


rem rezip -defl -comp zipada%version%-bin-win32.zip
rem del zipada%version%-bin-win32.zip
rem ren zipada%version%-bin-win32.repacked.zip zipada%version%-bin-win32.zip

echo This archive contains tools from Zip-Ada built as Windows 32-bit command line executables | zip -z zipada%version%-bin-win32.zip

pause
