@echo off

if "%1"=="" echo Syntax: trtest_single tc_train tc_in
if "%1"=="" echo See trtrain.cmd for an example
if "%1"=="" goto :EOF

set tc_train=%1
rem tc_full_train_size is the size of tc_train + 10 (noise file's size).
set /a tc_full_train_size=%~z1 + 10
set tc_prefix=%~n2
set tc_ext=%~x2
set tc_in=%2
set tc_out=%tc_prefix%_out%tc_ext%

rem  Syntax of encoder & decoder:
rem    trained_encoder train_file data_file compressed_file skip_compressed_size
rem    trained_decoder train_file data_file decompressed_file train_compressed_size skip_decompressed_size

rem  Add 10 bytes noise.
copy /B %tc_train%+rnd_10.bin full_trainer.dat

rem  Get the compressed trainer, itself with zero training (untrained compression).
echo.
echo ***  Encoding the trainer...
rem Old technique (just to see how long the EOS marker is in the compressed sream...)
trained_encoder zero.txt full_trainer.dat trainer_%tc_train%_old.dat   0
rem New technique
trainer full_trainer.dat trainer_%tc_train%.dat
rem New technique, with pure trainer file without noise (don't use it!)
trainer %tc_train% trainer_%tc_train%_no_noise.dat

call :calibrate trainer_%tc_train%.dat 

rem  See how the test data is compressed with zero training.
echo ***  Encoding the data without training...
trained_encoder zero.txt %tc_in% %tc_prefix%%tc_ext%.utc 0

rem
rem  The real test: compress %tc_in% to %tc_prefix%%tc_ext%.tc, using %tc_train% as training data.
rem

echo ***  Encoding the data with training...
@echo on
trained_encoder full_trainer.dat %tc_in% %tc_prefix%%tc_ext%.tc        %tc_train_stub_size%
@echo off

echo ***  Decoding...
@echo on
trained_decoder trainer_%tc_train%.dat %tc_prefix%%tc_ext%.tc %tc_out% %tc_train_stub_size% %tc_full_train_size%
@echo off

echo ***  ---  Encoding-decoding done.
echo ***  Checking that the decompressed (out) is identical to the uncompressed (in):
fc /B %tc_in% %tc_out%

del full_trainer.dat

goto :EOF

:calibrate
rem  The constant is empirically set.
rem  To do: find n = the maximum #bytes on encoder flushing and use n in the Ada code. 
set /a tc_train_stub_size=%~z1 - 140
