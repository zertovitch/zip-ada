@echo off

echo.
echo ** Optimized mode **

if exist obj_opt\create_dir.txt goto skip_md
md obj_opt
copy /b extras\lib*.a obj_opt

:skip_md

set gnatoptions_87312863=Optimize
call make_za %1