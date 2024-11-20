@echo off

REM for /L %%i in (1,1,9) do (
  REM bzip2 -k -%%i %1
  REM if exist %1_%%i.bz2 del %1_%%i.bz2
  REM mv %1.bz2 %1_%%i.bz2
  REM bzip2_dec %1_%%i.bz2 %1_%%i.out
  REM )
  
REM for /L %%i in (1,1,4) do (
  REM bzip2_enc %1 %1_%%i_ada -%%i
  REM bzip2_dec    %1_%%i_ada.bz2 %1_%%i_ada.out
  REM )
  
REM for /L %%i in (1,1,9) do comp /M %1 %1_%%i.out
REM for /L %%i in (1,1,4) do comp /M %1 %1_%%i_ada.out

del $_bb_*max.zip

zip -9 -Z bzip2                     $_bb_iz_bzip2_max %1
7z a -tzip -mm=BZip2:d=900k:pass=99 $_bb_7z_bzip2_max %1
..\zipada -eb3                      $_bb_za_bzip2_max %1

7z a -tzip -mm=LZMA -mx9            $_bb_7z_lzma_max  %1
zipada -el3                         $_bb_za_lzma_max  %1

7z t $_bb_*max.zip

dir $_bb_*max.zip
