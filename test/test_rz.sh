#!/bin/bash

echo ""
echo "Testing the ReZip tool - only internal compression methods."
echo ""
echo "A call to Comp_Zip is done to compare archives (before / after)"
echo "and check that their uncompressed contents are identical."
echo ""

#  Create an archive with default compression method (fast).

../zipada test_rz *.ad* *.txt *.cmd *.sh *.pdf *.mdb *.xls *.au

#  Recompress the archive.

../rezip -int $1 $2 $3 $4 $5 $6 $7 $8 $9 test_rz

#  Show recompression report.

firefox test_rz.ReZip.html

#  Compare before vs. after.

../comp_zip test_rz test_rz.repacked -q2

