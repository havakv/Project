#!/usr/bin/env bash

./cartSpam.R # Both simulation and plot

OMP_NUM_THREADS=1 nice ./adaboostSpam.R 10

OMP_NUM_THREADS=1 nice ./gradBoostSpam.R 5

OMP_NUM_THREADS=1 nice ./baggingSpam.R 10

OMP_NUM_THREADS=1 nice ./randomForestSpam.R 10

OMP_NUM_THREADS=1 nice ./OOBvsCVvsTest.R 10

OMP_NUM_THREADS=1 nice ./RFDepthSpam.R 5
