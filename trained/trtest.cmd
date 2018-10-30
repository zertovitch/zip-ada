@echo off

gprbuild -p

rem  .xls and .csv data: public data series from https://fred.stlouisfed.org/series/[series name]

call trtest_single gs1.xls 50196 7300 gs2 xls
call trtest_single gs1.xls 50196 7300 gs3 xls

call trtest_single wsbase.csv 35048 8500 wm1ns csv
call trtest_single wsbase.csv 35048 8500 wm2ns csv

rem  Need to ajust the sizes to what your compiler has given...
REM call trtest_single trained_decoder.exe 336916 134100 trained_encoder exe

echo.
echo ****************************************************
echo *** Files compressed with untrained compression: ***
echo ****************************************************
echo.
dir *.utc

echo.
echo **************************************************
echo *** Files compressed with trained compression: ***
echo **************************************************
echo.
dir *.tc

echo.
echo ***************************************************************
echo *** Trainer files (themselves compressed without training): ***
echo ***************************************************************
echo.
dir *.dat
