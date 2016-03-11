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
del *.lz77

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

if exist test_za*.zip  del test_za*.zip
if exist test_ifz?.zip del test_ifz?.zip
if exist test_kzip.zip del test_kzip.zip
if exist test_7z_d.zip del test_7z_d.zip
if exist test_zopf.zip del test_zopf.zip

if exist test_rz.zip          del test_rz.zip
if exist test_rz.repacked.zip del test_rz.repacked.zip
if exist test_rz.ReZip.html   del test_rz.ReZip.html
if exist test_rz.ReZip.log    del test_rz.ReZip.log

if exist $* del $*
if exist tuttifru.zip del tuttifru.zip
if exist binana.zip   del binana.zip

del *.zcd
del *.log
del *.bak
del *.lz77

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

