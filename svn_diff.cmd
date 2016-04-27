rem SVN diff, with blank space neutral, and case insensitive.

svn diff --diff-cmd diff -x "-i -b -B" %1 %2 %3

echo Command used was... svn diff --diff-cmd diff -x "-i -b -B" %1 %2 %3
