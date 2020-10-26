@echo off

if exist benchs.log del benchs.log

rem ***********************************************
rem *** Classic lossless compression benchmarks ***
rem ***********************************************

rem Some vintage corpora from latest millennium
rem https://corpus.canterbury.ac.nz/descriptions/
start /min /low bench calGAry  %1
start /min /low bench caNTrBry %1

rem Silesia corpus (~2003)
rem http://sun.aei.polsl.pl/~sdeor/index.php?page=silesia
start /min /low bench silesia %1

rem The 100 Million first signs of Wikipedia at some date in 2006
rem http://mattmahoney.net/dc/textdata.html
start /min /low bench enwik8 %1

rem **********************
rem *** Our benchmarks ***
rem **********************

rem Highly sparse integer matrix, CSV format
start /min /low bench matrix %1
start /min /low bench pdf    %1
rem DNA sequences in various formats (e.g. FASTA, Genbank)
start /min /low bench dna    %1
rem Database stuff (.sql, .mdb)
start /min /low bench db     %1
