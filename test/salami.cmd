del *.tmp

several_sizes %1 

zipada -ed1 %1_d1.zip *.tmp
zipada -ed2 %1_d2.zip *.tmp
zipada -ed3 %1_d3.zip *.tmp
zipada -el1 %1_l1.zip *.tmp
zipada -el2 %1_l2.zip *.tmp
zipada -el3 %1_l3.zip *.tmp

rem Need to give hint otherwise than with extension
rem
rem zipada -ep1 %1_p1.zip *.tmp
rem zipada -ep2 %1_p2.zip *.tmp

del *.tmp

7z t %1_*.zip | tail
dir /os %1_*.zip
