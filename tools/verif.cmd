del *.ali
del *.adt
del *.o

rem Invoke AdaControl:

adactl %1 %2 %3 %4 %5 %6 %7 %8 %9 -vw -f verif.aru -p ../zipada.gpr -- -Izip_lib -Itools -Iextras
