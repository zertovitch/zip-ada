if exist %1.old.zip del %1.old.zip
copy /b %1 %1.old.zip

:loop
  rezip %1
  del %1
  move %1.repacked.zip %1
goto loop

