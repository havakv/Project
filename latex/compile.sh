#!/usr/bin/env bash

# This script checks if there are any differences in the *R files.
# If that is the case, the cache is deleted so that they are not cached in the Rnw file

if [ -d "cache" ]; then
    #rm -r cache
    for f in ./cache/extra/*.R; do 
        echo "Processing $f file.."; 
        ./compile/deleteCasheIfDifferent.sh $f
    done
    ./compile/compile_Rnw.sh project.Rnw
else
    ./compile/compile_Rnw.sh project.Rnw
    mkdir cache/extra
fi

cp ../code/*.R cache/extra



