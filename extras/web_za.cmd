cd ..

gprbuild -Pzipada      -XZip_Build_Mode=Debug
gprbuild -Pzipada_test -XZip_Build_Mode=Debug

set mains=zipada unzipada rezip comp_zip find_zip demo_zip demo_unzip unzip ziptest zip.ads zip.adb lzma_dec lzma_enc lzhuf.adb bunzip zip_with_many_files

perl extras/za_html.pl -Izip_lib -Iobj/dbg -Iobj/dbg/test -Itest -Iextras -Itools -Idemo %mains% -f -d -oza_html

cd extras
pause
