@echo off

gprbuild -p

rem  .xls and .csv data: public data series from https://fred.stlouisfed.org/series/[series name]

call trtest_single gs1.xls gs2.xls
call trtest_single gs1.xls gs3.xls

call trtest_single wsbase.csv wm1ns.csv
call trtest_single wsbase.csv wm2ns.csv

REM call trtest_single u1.json u2.json
REM call trtest_single u1.json u3.json

REM call trtest_single trained_decoder.exe trained_encoder.exe

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

pause
