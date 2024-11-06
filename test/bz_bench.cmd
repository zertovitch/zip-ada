@echo off

for /L %%i in (1,1,9) do (
  bzip2 -k -%%i %1
  if exist %1_%%i.bz2 del %1_%%i.bz2
  mv %1.bz2 %1_%%i.bz2
  bzip2_dec %1_%%i.bz2 %1_%%i.out
  )
  
for /L %%i in (1,1,4) do (
  bzip2_enc %1 %1_%%i_ada -%%i
  bzip2_dec    %1_%%i_ada.bz2 %1_%%i_ada.out
  )
  
for /L %%i in (1,1,9) do comp /M %1 %1_%%i.out
for /L %%i in (1,1,4) do comp /M %1 %1_%%i_ada.out

