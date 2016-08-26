call clean_za
cd ..
cd ..

set root=zip-ada

set files=%root%/z*.txt %root%/appnote.txt %root%/za*.xls %root%/*.gpr %root%/*.prj %root%/*.pra %root%/*_za.cmd %root%/make_one.cmd
set files=%files% %root%/zip_lib/*.ad*
set files=%files% %root%/test/*.ad* %root%/test/za_gcov.cmd %root%/test/test*.cmd %root%/test/bench*.cmd %root%/test/prof.cmd
set files=%files% %root%/demo/*.ad* 
set files=%files% %root%/tools/*.ad* %root%/tools/rez*.cmd %root%/tools/verif.* %root%/tools/adactl*.cmd %root%/tools/save.cmd %root%/tools/clean*.cmd
set files=%files% %root%/extras/*.ad* %root%/extras/*.a %root%/extras/*.rc %root%/extras/*.rbj %root%/extras/*.ico %root%/extras/*.pl %root%/extras/w*.cmd

zipada -ed3 za_%date%_.zip %files%

cd %root%/tools