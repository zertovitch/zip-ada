@echo off

rem Use 7Zip's LZ77 compressor and Zip-Ada for the further stages of Deflate compression.

7z a -tzip -mx=9 -mm=deflate $temp$.zip %2

rem The following executable is obtained by setting trace = some_t in UnZip.Decompress and renaming unzipada.exe
uza_dump_lz  -t $temp$.zip

rem The following executable is obtained by setting bypass_LZ77 = True in Zip.Compress.Deflate and renaming zipada.exe
za_bypass_lz -ed3 %1 %2
