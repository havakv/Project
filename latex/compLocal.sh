#!/usr/bin/env bash
# echo -e "require('knitr') \n Sweave2knitr('prob5.Rnw')" | R --no-save 
if [ -d "cache" ]; then
    #rm -r cache
    for f in ./cache/extra/*.R; do 
        echo "Processing $f file.."; 
        ./compile/deleteCasheIfDifferent.sh $f
    done
    name=$(echo $1 | cut -d "." -f1)
    cp -f ${name}.tex ${name}Tmp.Rnw
    echo -e "require('knitr') \n knit('${name}Tmp.Rnw')" | R --no-save -q
    gnome-terminal -e "pdflatex ${name}Tmp.tex"
else
    name=$(echo $1 | cut -d "." -f1)
    cp -f ${name}.tex ${name}Tmp.Rnw
    echo -e "require('knitr') \n knit('${name}Tmp.Rnw')" | R --no-save -q
    gnome-terminal -e "pdflatex ${name}Tmp.tex"
    ./compile/compile_Rnw.sh project.Rnw
    mkdir cache/extra
fi

cp ../code/*.R cache/extra

