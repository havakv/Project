#!/usr/bin/env bash

# Make .tex file
name=$(echo $1 | cut -d "." -f1)
echo -e "require('knitr') \n knit('${name}.Rnw')" | R --no-save -q

# Compile to pdf
gnome-terminal -e "pdflatex ${name}.tex"

