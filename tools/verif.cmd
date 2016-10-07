cd ..

del *.ali
del *.adt
del *.o

rem Invoke AdaControl:

call adactl %1 %2 %3 %4 %5 %6 %7 %8 %9 -vw -f tools/verif.aru -p zipada.gpr -- -Izip_lib -Itools -Iextras

del *.ali
del *.adt
del *.o

cd tools
