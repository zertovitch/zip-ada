for /L %%i in (1,1,9) do (
  bzip2 -k -%%i %1
  mv %1.bz2 %1_%%i.bz2)

for /L %%i in (1,1,9) do (
  bzip2_enc %1 %1_%%i_ada -%%i)


