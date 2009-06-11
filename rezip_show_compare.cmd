rem Rezip, show results and compare

call rezip_ren %1

start %1.ReZip.html

comp_zip %1.old.zip %1

pause
