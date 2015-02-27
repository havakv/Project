#!/usr/bin/env bash

# Build common library
cd ./code/common
./INSTALL.sh
cd -

./R/ldaVsLogistic.R

cd latex
pdflatex ./project.tex
cp ./project.pdf ../

