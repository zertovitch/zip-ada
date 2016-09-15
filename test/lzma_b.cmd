@echo off

echo Benchmark various LZMA parameters for compressing file, with the standard LZMA program... %1
echo Similar to... lzma_enc %1 %~n1 -b

for /l %%l in (0,1,8) do for /l %%p in (0,1,4) do for /l %%b in (0,1,4) do lzma e %1 %~n1_%%l%%p%%b.lzma -lc%%l -lp%%p -pb%%b
dir /o-s %~n1_*.lzma
