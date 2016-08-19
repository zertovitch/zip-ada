rem Code coverage for compression to check which cases are covered.

rem gprbuild -P zipada.gpr -XBuild_Mode=Debug -f -cargs -fprofile-arcs -ftest-coverage -largs -fprofile-arcs

rem Do something with zipada and lots of mixed data...

cd ..\obj_dbg
gcov zip-compress-reduce.adb  -s ../zip_lib
gcov zip-compress-shrink.adb  -s ../zip_lib
gcov zip-compress-deflate.adb -s ../zip_lib
gcov zip-compress-lzma_e.adb  -s ../zip_lib
gcov lzma_encoding.adb        -s ../zip_lib
cd ..\test
