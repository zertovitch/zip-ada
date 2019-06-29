#!/bin/bash

echo ""
echo " =================================================="
echo "      Testing the ZipAda tool with Comp_Zip"
echo " =================================================="
echo ""

#  for f in test_za*.zip do if[ -f "$f" ] then rm $f fi done
#  for f in test_iz*.zip do if[ -f "$f" ] then rm $f fi done

#  Have a badly compressible file (random.bin), a bit more than 2**n because of rare repetitions

files="*.mix *.ad* *.txt *.cmd *.sh *.bmp *.csv *.pdf *.html *.bin"

../zipada     -esh              test_zash     $files
../zipada     -edf              test_zadf     $files
../zipada     -ed1              test_zad1     $files
../zipada     -ed2              test_zad2     $files
../zipada     -ed3              test_zad3     $files
../zipada     -el1              test_zal1     $files
../zipada     -el2              test_zal2     $files
../zipada     -el3              test_zal3     $files
../zipada     -ep1              test_zap1     $files
../zipada     -ep2              test_zap2     $files
zip           -1                test_izd1     $files
zip           -6                test_izd6     $files
zip           -9                test_izd9     $files
zip           -9 -Z bzip2       test_izb9     $files

../comp_zip test_zash test_zadf -q2
../comp_zip test_zash test_zad1 -q2
../comp_zip test_zash test_zad2 -q2
../comp_zip test_zash test_zad3 -q2
../comp_zip test_zash test_zal1 -q2
../comp_zip test_zash test_zal2 -q2
../comp_zip test_zash test_zal3 -q2
../comp_zip test_zash test_zap1 -q2
../comp_zip test_zash test_zap2 -q2
../comp_zip test_zash test_izd9 -q2
../comp_zip test_zash test_izb9 -q2

