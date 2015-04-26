#!/usr/bin/env bash

OMP_NUM_THREADS=1 nice ./adaboostSim.R 10

OMP_NUM_THREADS=1 nice ./gradBoost.R 4
