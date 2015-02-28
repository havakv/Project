#!/usr/bin/env bash

# Build common library
cd ./code/common
./INSTALL.sh
cd -

cd latex
if [ ! -d "$figures" ]; then
    mkdir figures
fi

./compile.sh

cp ./project.pdf ../

