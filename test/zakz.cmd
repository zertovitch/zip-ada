@echo off

rem Use KZip's LZ77 compressor, then Zip-Ada for the further stages of Deflate compression.

kzip $temp$.zip %2

rem The following executable is obtained by setting trace = some_t in UnZip.Decompress and renaming unzipada.exe
uza_dump_lz  -t $temp$.zip

rem The following executable is obtained by setting bypass_LZ77 = True in Zip.Compress.Deflate and renaming zipada.exe
za_bypass_lz -ed3 %1 %2
