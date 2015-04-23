#!/usr/bin/env bash

# Build common library
echo Building libraries
cd ./code/common
./INSTALL.sh
cd -

# Get dataset
echo
echo Retreving datasets
cd ./dataset
./getSpam.sh
cd -

# Setup latex folders
echo
echo Setting up latex folders
cd latex
if [ ! -d "$figures" ]; then
    mkdir figures
fi
if [ ! -d "$dataR" ]; then
    mkdir dataR
fi

echo 
echo To compile project run: \"./compile.sh\" in latex folder
