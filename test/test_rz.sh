#!/bin/bash

echo ""
echo "Testing the ReZip tool with Comp_Zip"
echo ""

#  Create an archive

../zipada test_rz *.ad* *.txt *.cmd *.sh *.pdf *.mdb

#  Recompress it

../rezip -int %1 %2 %3 %4 %5 %6 %7 %8 %9 test_rz

firefox test_rz.ReZip.html

#  Compare before vs. after

../comp_zip test_rz test_rz.repacked -q2

