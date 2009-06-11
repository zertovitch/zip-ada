call clean_uza

zip -9 uza_src_%date%_.zip *.ad* *.cmd u*.txt *.ago *.gpr
zip -9 uza_src_%date%_.zip zip_lib/*.ad* zip_lib/*.ago
zip -9 uza_src_%date%_.zip obj_dbg/*.pra
