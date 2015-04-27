#!/usr/bin/env bash

OMP_NUM_THREADS=1 nice ./adaboostSim.R 10

OMP_NUM_THREADS=1 nice ./gradBoost.R 5

nice ./baggingSpam.R

OMP_NUM_THREADS=1 nice ./randomForestSpam.R 10
