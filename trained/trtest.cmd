@echo off

gprbuild

set tc_train=gs1.xls
rem tc_full_train_size is the size of tc_train + 20 (noise file's size).
set tc_full_train_size=50196
set tc_train_stub_size=7300
set tc_prefix=gs2
set tc_ext=xls
set tc_in=%tc_prefix%.%tc_ext%
set tc_out=%tc_prefix%_out.%tc_ext%

rem  Syntax of encoder & decoder:
rem    trained_encoder train_file data_file compressed_file skip_compressed_size
rem    trained_decoder train_file data_file decompressed_file train_compressed_size skip_decompressed_size

rem  Add 20 bytes noise.
copy /B %tc_train%+rnd_20.bin full_trainer.dat

rem  Get the compressed trainer, itself with zero training (untrained compression).
trained_encoder zero.txt full_trainer.dat trainer.utc   0

rem  See how the test data is compressed with zero training.
trained_encoder zero.txt %tc_in% %tc_prefix%.utc 0

rem  The real test: compress %tc_in% to %tc_prefix%.tc, using %tc_train% as training data.
trained_encoder full_trainer.dat %tc_in%        %tc_prefix%.tc %tc_train_stub_size%
trained_decoder trainer.utc      %tc_prefix%.tc %tc_out%       %tc_train_stub_size% %tc_full_train_size%

echo Check that the decompressed is identical to the uncompressed:
fc /B %tc_in% %tc_out%
dir %tc_in% *.utc *.tc
