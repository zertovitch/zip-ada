@echo off
echo Test UnZipAda for one data file compressed with external zippers (no ZipAda !)
echo Archive name = %2
echo Data name = %1

if (%2)==() goto vide

if exist *.tmp del *.tmp
if exist %2.zip del %2.zip

ren %1 $13_kzip.tmp
kzip %2 $13_kzip.tmp

if not exist %2.zip goto no_data

ren $13_kzip.tmp $00store.tmp
rem zip -0 -P tralala %2 $00store.tmp
7z a -mx0 -ptralala %2.zip $00store.tmp

ren $00store.tmp $01pk090.tmp
pkzip090 -ea1 %2 $01pk090.tmp

ren $01pk090.tmp $02pk090.tmp
pkzip090 -ea2 %2 $02pk090.tmp

ren $02pk090.tmp $03pk090.tmp
pkzip090 -ea3 %2 $03pk090.tmp

ren $03pk090.tmp $04pk090.tmp
pkzip090 -ea4 %2 $04pk090.tmp

ren $04pk090.tmp $05pk090.tmp
pkzip090 -eb %2 $05pk090.tmp

ren $05pk090.tmp $06pk101.tmp
pkzip101 -ex %2 $06pk101.tmp

ren $06pk101.tmp $07pk110.tmp
pkzip110 -ei %2 $07pk110.tmp

ren $07pk110.tmp $08pk193.tmp
pkzip193 -es %2 $08pk193.tmp

ren $08pk193.tmp $09pk193.tmp
pkzip193 -en %2 $09pk193.tmp

ren $09pk193.tmp $10pk193.tmp
pkzip193 -ex %2 $10pk193.tmp

ren $10pk193.tmp $11pk204.tmp
pkzip204 -ex %2 $11pk204.tmp

ren $11pk204.tmp $12pk250.tmp
pkzip250 -exx -stralala %2 $12pk250.tmp

ren $12pk250.tmp $14_df64.tmp
call 7z a -mx9 -mm=deflate64 -mfb=257 -mpass=15 -mmc=10000 %2.zip $14_df64.tmp

ren $14_df64.tmp $15_bzp2.tmp
zip -Z bzip2 %2 $15_bzp2.tmp

ren $15_bzp2.tmp $16_lzma.tmp
7z a -mx -mm=LZMA %2.zip $16_lzma.tmp

ren $16_lzma.tmp %1

echo All! | unzipada -s tralala %2.zip

echo Checking decompressed outputs:
pause

if exist $00store.tmp fc /B %1 $00store.tmp
if exist $00_cryp.tmp fc /B %1 $00_cryp.tmp
if exist $01pk090.tmp fc /B %1 $01pk090.tmp
if exist $02pk090.tmp fc /B %1 $02pk090.tmp
if exist $03pk090.tmp fc /B %1 $03pk090.tmp
if exist $04pk090.tmp fc /B %1 $04pk090.tmp
if exist $05pk090.tmp fc /B %1 $05pk090.tmp
if exist $06pk101.tmp fc /B %1 $06pk101.tmp
if exist $07pk110.tmp fc /B %1 $07pk110.tmp
if exist $08pk193.tmp fc /B %1 $08pk193.tmp
if exist $09pk193.tmp fc /B %1 $09pk193.tmp
if exist $10pk193.tmp fc /B %1 $10pk193.tmp
if exist $11pk204.tmp fc /B %1 $11pk204.tmp
if exist $12pk250.tmp fc /B %1 $12pk250.tmp
if exist $13_kzip.tmp fc /B %1 $13_kzip.tmp
if exist $14_df64.tmp fc /B %1 $14_df64.tmp
if exist $15_bzp2.tmp fc /B %1 $15_bzp2.tmp
if exist $16_lzma.tmp fc /B %1 $16_lzma.tmp

pause

goto fin

:vide
echo test_1 compresses a file with various Zip compressors into a single
echo archive with several names and then checks the decompressed outputs
echo of Zip-Ada against that file.
echo.
echo Please start test_uza for a sample test (binary, text)
echo as well as a list of the needed compressors.
echo.
echo Usage: test_1 file archive
goto fin

:no_data
echo UnZipAda test... No file name matches %1

:fin