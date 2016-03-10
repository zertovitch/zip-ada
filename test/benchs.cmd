@echo off

rem Some classic corpora from latest millenium

call bench calGAry %1
call bench caNTrBry %1

rem The 100 Million first signs of Wikipedia in 2006

call bench enwik8 %1
