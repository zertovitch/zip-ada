@echo off

echo.
echo ** Debug mode **

if exist obj_dbg\debug.pra goto skip_md
md obj_dbg

:skip_md

set gnatoptions_87312863=Debug
call make_za %1
