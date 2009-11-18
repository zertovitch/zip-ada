rem Rezip, show results and compare

call rezip_ren %1

for %%z in (%1) do for %%y in ("%%~dz%%~pz%%~nz.ReZip.html") do start %%~fsy

pause
