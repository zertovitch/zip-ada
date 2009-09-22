if exist %1.old.zip del %1.old.zip
copy /b %1 %1.old.zip

del *.tmp
del $temp$.zip

rezip -comp %1
del %1
move %1.repacked.zip %1

