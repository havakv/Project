#!/usr/bin/env bash

# Build common library
cd ./code/common
./INSTALL.sh
cd -

cd latex
./compile.sh
cp ./project.pdf ../

