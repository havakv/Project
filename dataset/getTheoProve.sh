#!/usr/bin/env bash

# The theorem proving dataset can be found on 
#    http://archive.ics.uci.edu/ml/datasets/First-order+theorem+proving

# Get data
curl -O http://archive.ics.uci.edu/ml/machine-learning-databases/00249/ml-prove.tar.gz

# unzip
tar -zxvf ./ml-prove.tar.gz

rm -f ./ml-prove.tar.gz
