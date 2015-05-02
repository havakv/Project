#!/usr/bin/env Rscript
# Script for simulations for adaboost.M1

set.seed(0)

# Check if environment variables are fine.
if(system("echo $OMP_NUM_THREADS", intern = TRUE) != 1)
    stop("OMP_NUM_THREADS is not 1")

argv <- commandArgs(trailingOnly=TRUE)
if (identical(argv, character(0)))
    stop("Need number of threads as input parameter")

nCores <- as.integer(argv[1])

library(common)    
library(adabag)
require(parallel) # one of the core R packages
require(doParallel)
library(foreach)

# Get training and test data
X <- getPhoneme()

maxdepth  <- c(1, 3, 5, 10, 20, 30)
its       <- round(c(seq(1, 10, length.out = 5), 
                     seq(20, 300, length.out = 10)))
ndept     <- length(maxdepth)
nit       <- length(its)
minbucket <- 1
cp        <- 1e-100
Errors <- matrix(NA, nit, ndept)

registerDoParallel(nCores)
Errors <- foreach(i = 1:nit, .combine = rbind) %dopar% {
    err <- rep(NA, ndept)
    for (j in 1:ndept) {
        cat("it:", its[i], "\tdepth:", maxdepth[j], "\n")

        ada2 <- boosting(class ~ ., X$train, boos=FALSE, mfinal=its[i], coeflearn="Freund",
                         control = rpart.control(maxdepth=maxdepth[j], cp = cp, minbucket=minbucket))
        predAda2 <- predict(ada2, X$test)
        err[j] <- sum(predAda2$class != X$test$class)/X$nTest
    }
    err
}

# Save variables
save(maxdepth, its, ndept, nit, Errors, file = "../../dataset/phonemeResults/adaboostPhoneme.Rdata")




