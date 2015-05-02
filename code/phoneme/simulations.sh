#!/usr/bin/env bash

OMP_NUM_THREADS=1 nice ./adaboostPhoneme.R 10

OMP_NUM_THREADS=1 nice ./gradBoostPhoneme.R 5

OMP_NUM_THREADS=1 nice ./baggingPhoneme.R 10

OMP_NUM_THREADS=1 nice ./randomForestPhoneme.R 10
