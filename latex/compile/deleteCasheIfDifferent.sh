#!/usr/bin/env bash

# This file needs to be run in the latex directory!!!

file=$(basename "$1") # without path
filename="${file%.*}" # without filending

realFile="../code/${file}" # find file in code folder

if ! diff $1 $realFile >/dev/null ; then
    cd ./cache
    echo "Deleting cached ${filename}"
    rm -f ${filename}*
fi

#echo $1
