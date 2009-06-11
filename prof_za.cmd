@echo off

echo.
echo ** Profiling Zip-Ada.

if exist obj_pro\create_dir.txt goto skip_md
md obj_pro

:skip_md

set gnatoptions_87312863=Profiling
call make_za %1

echo run unzipada, then type "gprof unzipada.exe".
