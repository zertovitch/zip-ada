rem Rezip, show results and compare

call rezip_ren %1

for %%z in (%1) do start %%~dz%%~pz%%~nz.ReZip.html

pause
