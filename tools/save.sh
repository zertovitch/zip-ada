#!/bin/bash

nice_date=$(date +%Y-%m-%d_%H:%M:%S)

root=za

cd ..
cd ..

files="$root/*.gpr $root/*.pra"
# Ada sources
files="$files $root/zip_lib/*.ad*"
files="$files $root/demo/*.ad* $root/extras/*.ad*"
files="$files $root/test/*.ad* $root/tools/*.ad*"
files="$files $root/trained/*.ad* $root/trained/*.gpr"
# Documentation
files="$files $root/doc/*.txt $root/doc/*.xls $root/doc/*.pdf"
# Scripts
files="$files $root/*.cmd $root/*.sh"
files="$files $root/tools/*.cmd $root/tools/*.sh"

# echo $nice_date
# echo $files

$root/zipada -ep2 za_$nice_date $files 

cd $root
cd tools


