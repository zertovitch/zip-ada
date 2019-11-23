#!/bin/bash

cd ..

rm  *.ali
rm  *.adt
rm  *.o

echo Invoking AdaControl.
echo "Arg 1: $1"
echo "Arg 2: $2"

adactl $1 $2 -vw -f tools/verif.aru -p zipada.gpr -- -Izip_lib -Itools -Iextras

rm  *.ali
rm  *.adt
rm  *.o

cd tools
