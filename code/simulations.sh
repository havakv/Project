#!/usr/bin/env bash

OMP_NUM_THREADS=1 nice ./adaboostSpam.R 10

OMP_NUM_THREADS=1 nice ./gradBoostSpam.R 5

nice ./baggingSpam.R

OMP_NUM_THREADS=1 nice ./randomForestSpam.R 10
