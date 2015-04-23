#!/usr/bin/env bash

# The spambase dataset can be found on 
#    http://sci2s.ugr.es/keel/dataset.php?cod=109

# Get spambase
curl -O http://sci2s.ugr.es/keel/dataset/data/classification/spambase.zip

# unzip
unzip -o ./spambase.zip

rm -f ./spambase.zip
