#!/usr/bin/env bash

./adaboostPlot.R

OMP_NUM_THREADS=1 nice ./gradBoostPlot.R 10

./baggingAndRandomForestPlot.R

