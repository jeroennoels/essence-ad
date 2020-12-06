#!/bin/bash
find ../essence-ad/ -name "*.o"  -print0 | xargs -r -0 /bin/rm
find ../essence-ad/ -name "*.hi" -print0 | xargs -r -0 /bin/rm
find ../essence-ad/ -name "*~"   -print0 | xargs -r -0 /bin/rm
/bin/rm -f ../essence-ad/test/Main
