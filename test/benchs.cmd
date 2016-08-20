@echo off

rem ***********************************************
rem *** Classic lossless compression benchmarks ***
rem ***********************************************

rem Some classic corpora from latest millennium
call bench calGAry %1
call bench caNTrBry %1

rem The 100 Million first signs of Wikipedia at some date in 2006
rem https://cs.fit.edu/~mmahoney/compression/textdata.html
rem http://cs.fit.edu/~mmahoney/compression/enwik8.zip
rem http://download.wikipedia.org/enwiki/20060303/enwiki-20060303-pages-articles.xml.bz2
call bench enwik8 %1

rem **********************
rem *** Our benchmarks ***
rem **********************

rem Highly sparse integer matrix, CSV format
call bench matrix %1
call bench pdf    %1
rem DNA sequences in various formats (e.g. FASTA, Genbank)
call bench dna    %1
