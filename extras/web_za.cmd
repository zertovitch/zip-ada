cd ..

gprbuild -Pzipada      -XZip_Build_Mode=Debug
gprbuild -Pzipada_test -XZip_Build_Mode=Debug

set mains=zipada.adb unzipada.adb rezip.adb comp_zip.adb find_zip.adb
set mains=%mains% demo_zip.adb demo_unzip.adb unzip.adb ziptest.adb
set mains=%mains% zip.ads zip.adb bzip2_dec.adb bzip2_enc.adb lzma_dec.adb lzma_enc.adb lzhuf.adb zip_with_many_files.adb

rem GNATHTML can be found at
rem https://alire.ada.dev/crates/ali_parse
rem https://github.com/zertovitch/ali_parse
gnathtml -Izip_lib -Iobj/dbg -Iobj/dbg/test -Itest -Iextras -Itools -Idemo %mains% -f -d -oza_html -b#fffbf4 -iextras/za_head.txt -jextras/za_top.txt -kextras/za_bottom.txt

cd extras
pause
