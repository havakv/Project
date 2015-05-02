#!/usr/bin/env bash

# The dataset can be found at
#   http://sci2s.ugr.es/keel/dataset.php?cod=105
# Get data
curl -O http://sci2s.ugr.es/keel/dataset/data/classification/phoneme.zip

# unzip
unzip -o ./phoneme.zip

rm -f ./phoneme.zip
