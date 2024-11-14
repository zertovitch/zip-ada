Zip-Ada
=======

Zip-Ada is a library for dealing with
the Zip compressed archive file format.

 - Fully open-source - no black-box
 - Fully in Ada - no interfacing headaches
 - Fully portable - no preprocessing, no conditionals

====
Complete description in: doc/zipada.txt
====

If you are impatient: in Alire (https://alire.ada.dev/),
do "alr get zipada", then "alr run" from the zip* directory.

Alternatively: have GCC / GNAT installed (e.g. from
https://www.adacore.com/download), then, open zipada.gpr with
GNAT Studio or, on your preferred command-line interpreter, type:

  gnatmake -P zipada

(if `gnatmake` doesn't work, try `gprbuild`)

Bonus: the file zipada.gpr is a text file,
with some comments.
