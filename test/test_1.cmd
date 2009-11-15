@echo off
rem Test for one file

if (%2)==() goto vide

if exist *.tmp del *.tmp
del %2.zip

ren %1 $13_kzip.tmp
kzip %2 $13_kzip.tmp

ren $13_kzip.tmp $00store.tmp
pkzip250 -e0 %2 $00store.tmp

ren $00store.tmp $01redu1.tmp
pkzip090 -ea1 %2 $01redu1.tmp

ren $01redu1.tmp $02redu2.tmp
pkzip090 -ea2 %2 $02redu2.tmp

ren $02redu2.tmp $03redu3.tmp
pkzip090 -ea3 %2 $03redu3.tmp

ren $03redu3.tmp $04redu4.tmp
pkzip090 -ea4 %2 $04redu4.tmp

ren $04redu4.tmp $05shri1.tmp
pkzip090 -eb %2 $05shri1.tmp

ren $05shri1.tmp $06impl1.tmp
pkzip101 -ex %2 $06impl1.tmp

ren $06impl1.tmp $07impl2.tmp
pkzip110 -ei %2 $07impl2.tmp

ren $07impl2.tmp $08defl1.tmp
pkzip193 -es %2 $08defl1.tmp

ren $08defl1.tmp $09defl2.tmp
pkzip193 -en %2 $09defl2.tmp

ren $09defl2.tmp $10defl3.tmp
pkzip193 -ex %2 $10defl3.tmp

ren $10defl3.tmp $11defl4.tmp
pkzip204 -ex %2 $11defl4.tmp

ren $11defl4.tmp $12defl5.tmp
pkzip250 -exx -stralala %2 $12defl5.tmp

ren $12defl5.tmp $14_df64.tmp
call 7z a -mx9 -mm=deflate64 -mfb=257 -mpass=15 -mmc=10000 %2.zip $14_df64.tmp

ren $14_df64.tmp $15_bzp2.tmp
zip -Z bzip2 %2 $15_bzp2.tmp

ren $15_bzp2.tmp %1

echo All! | unzipada -s tralala %2.zip

pause

fc /B %1 $00store.tmp
fc /B %1 $01redu1.tmp
fc /B %1 $02redu2.tmp
fc /B %1 $03redu3.tmp
fc /B %1 $04redu4.tmp
fc /B %1 $05shri1.tmp
fc /B %1 $06impl1.tmp
fc /B %1 $07impl2.tmp
fc /B %1 $08defl1.tmp
fc /B %1 $09defl2.tmp
fc /B %1 $10defl3.tmp
fc /B %1 $11defl4.tmp
fc /B %1 $12defl5.tmp
fc /B %1 $13_kzip.tmp
fc /B %1 $14_df64.tmp
fc /B %1 $15_bzp2.tmp

pause

goto fin

:vide
echo test_1 compresses a file with various Zip compressors into a single
echo archive with several names and checks the decompressed outputs
echo of Zip-Ada against that file.
echo.
echo Please start test_uza for a sample test (binary, text)
echo as well as a list of the needed compressors.
echo.
echo Usage: test_1 file archive
:fin