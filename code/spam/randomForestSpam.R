#!/usr/bin/env Rscript
    
set.seed(0)

# Check if environment variables are fine.
if(system("echo $OMP_NUM_THREADS", intern = TRUE) != 1)
    stop("OMP_NUM_THREADS is not 1")

# Get number of cores
argv <- commandArgs(trailingOnly=TRUE)
if (identical(argv, character(0)))
    stop("Need number of threads as input parameter")

nCores <- as.integer(argv[1])
library(common)
library(randomForest)
require(parallel) # one of the core R packages
require(doParallel)
library(foreach)

X <- getSpam()

B <- round(seq(5, 2000, length.out = 25))
err <- rep(NA,length(B))

registerDoParallel(nCores)
err <- foreach(i = 1:length(B), .combine = c) %dopar% {
    cat("Starting", i, "of", length(B), "\n")
    fit <- randomForest(spam ~ ., data = X$train, ntree = B[i])
    pred <- predict(fit, X$test, type="response")
    cat("Ending", i, "of", length(B), "\n")
    sum(pred != X$test$spam)/X$nTest
}
errRF <- err
BRF <- B


#------------------------------------------
mVec <- 1:10
mVec <- c(mVec, round(seq(12, 25, length.out = 10)))
B <- c(5, 10, 50, 500)
Errors <- matrix(NA, length(B), length(mVec))

registerDoParallel(nCores)
Errors <- foreach(i = 1:length(mVec), .combine = cbind) %dopar% {
    cat("Starting", i, "\n")
    err <- rep(NA, length(B))
    for (j in 1:length(B)) {
        fit <- randomForest(spam ~ ., data = X$train, 
                            mtry = mVec[i], ntree = B[j])
        pred <- predict(fit, X$test, type="response")
        err[j] <- sum(pred != X$test$spam)/X$nTest
    }
    cat("Ending", i, "\n")
    err
}


save(errRF, BRF, Errors, mVec, B, file = "../../dataset/spamResults/randomForestSpam.Rdata")

