call clean_za
cd ..

set files=z*.txt appnote.txt za*.xls *.gpr *.prj *.pra *_za.cmd make_one.cmd
set files=%files% zip_lib/*.ad*
set files=%files% test/*.ad* test/za_gcov.cmd test/test*.cmd test/bench*.cmd test/prof.cmd
set files=%files% tools/*.ad* tools/rez*.cmd tools/verif.* tools/adactl*.cmd tools/save.cmd tools/clean*.cmd
set files=%files% extras/*.ad* extras/*.a extras/*.rc extras/*.rbj extras/*.ico extras/*.pl extras/w*.cmd

zipada -ed3 za_%date%_.zip %files%
