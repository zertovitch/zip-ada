del *.ali
del *.o
del *.#*
del b~*
del *.bak
del *.bk.?
del *.0
del *.1
del *.2
del *.3
del *.tmp
del *.adt
del $test.*

cd zip_lib
del *.ali
del *.o
del *.#*
del b~*
del *.bak
del *.bk.?
del *.0
del *.1
del *.2
del *.3
del *.adt
cd..

cd test
if exist test_za??.zip del test_za??.zip
if exist test_ifz?.zip del test_ifz?.zip
cd ..

if not exist obj_opt\zip.ali md obj_opt
cd obj_opt
cleanacu
cd..

if not exist obj_dbg\zip.ali md obj_dbg
cd obj_dbg
cleanacu
cd..

if not exist obj_pro\zip.ali md obj_pro
cd obj_pro
cleanacu
cd..

