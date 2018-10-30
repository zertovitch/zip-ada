@echo off

if "%1"=="" echo Syntax: trtest_single tc_train tc_full_train_size tc_train_stub_size tc_prefix tc_ext
if "%1"=="" echo See trtrain.cmd for an example
if "%1"=="" goto fin


set tc_train=%1
rem tc_full_train_size is the size of tc_train + 20 (noise file's size).
set tc_full_train_size=%2
set tc_train_stub_size=%3
set tc_prefix=%4
set tc_ext=%5
set tc_in=%tc_prefix%.%tc_ext%
set tc_out=%tc_prefix%_out.%tc_ext%

rem  Syntax of encoder & decoder:
rem    trained_encoder train_file data_file compressed_file skip_compressed_size
rem    trained_decoder train_file data_file decompressed_file train_compressed_size skip_decompressed_size

rem  Add 20 bytes noise.
copy /B %tc_train%+rnd_20.bin full_trainer.dat

rem  Get the compressed trainer, itself with zero training (untrained compression).
trained_encoder zero.txt full_trainer.dat trainer_%tc_train%.dat   0

rem  See how the test data is compressed with zero training.
trained_encoder zero.txt %tc_in% %tc_prefix%.%tc_ext%.utc 0

rem  The real test: compress %tc_in% to %tc_prefix%.%tc_ext%.tc, using %tc_train% as training data.
trained_encoder full_trainer.dat %tc_in% %tc_prefix%.%tc_ext%.tc        %tc_train_stub_size%
trained_decoder trainer_%tc_train%.dat %tc_prefix%.%tc_ext%.tc %tc_out% %tc_train_stub_size% %tc_full_train_size%

del full_trainer.dat

echo ***  Encoding-decoding done.
echo ***  Check that the decompressed (out) is identical to the uncompressed (in):
fc /B %tc_in% %tc_out%

:fin