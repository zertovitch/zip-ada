rem  Syntax: trained_encoder train_file data_file compressed_file skip_compressed_size

rem  Get the compressed size of the trainer, with zero training (untrained compression).
trained_encoder zero.txt gs1.xls trainer.tc    0

rem  See how the test data is compressed with zero training.
trained_encoder zero.txt gs2.xls untrained.tc  0

rem  The real test: compress gs2.xls using gs1.xls as training data.
trained_encoder gs1.xls  gs2.xls trained.tc    7300

