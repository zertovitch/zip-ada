@echo off

del benchs.log

rem ***********************************************
rem *** Classic lossless compression benchmarks ***
rem ***********************************************

rem Some classic corpora from latest millennium
start /min /low bench calGAry %1
start /min /low bench caNTrBry %1

rem The 100 Million first signs of Wikipedia at some date in 2006
rem https://cs.fit.edu/~mmahoney/compression/textdata.html
rem http://cs.fit.edu/~mmahoney/compression/enwik8.zip
rem http://download.wikipedia.org/enwiki/20060303/enwiki-20060303-pages-articles.xml.bz2
start /min /low bench enwik8 %1

start /min /low bench silesia %1

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
