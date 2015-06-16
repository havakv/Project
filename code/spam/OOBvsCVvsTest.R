#!/usr/bin/env Rscript
# Difference of OOB, CV and Test error
NOPRINT=FALSE

# Check if environment variables are fine.
if(system("echo $OMP_NUM_THREADS", intern = TRUE) != 1)
    stop("OMP_NUM_THREADS is not 1")

# Get number of cores
argv <- commandArgs(trailingOnly=TRUE)
if (identical(argv, character(0)))
    stop("Need number of threads as input parameter")

nCores <- as.integer(argv[1])

set.seed(0)
library(common)
library(randomForest)
require(parallel) # one of the core R packages
require(doParallel)
library(foreach)
library(cvTools)

X <- getSpam()

## ---- OOBvsCVvsTest.R ----
nTree <- 600
fit <- randomForest(spam ~ ., data = X$train, ntree = nTree)
nindex <- 100
index <- round(seq(1, nTree, length.out=nindex))
errVec <- rep(NA, nindex)
for (i in 1:nindex) {
    fit$forest$ntree <- index[i]
    pred <- predict(fit, X$test, type="response")
    errVec[i] <- sum(pred != X$test$spam)/X$nTest
}

cv.fold <- 10
shuffled.order <- sample(X$nTrain)
groupSizes <- as.numeric(table(cvFolds(X$nTrain, cv.fold)$which))
ixGroup <- c(0, cumsum(groupSizes))

registerDoParallel(nCores)
Rates <- matrix(NA, nindex, cv.fold)

seeds <- sample.int(1e7, cv.fold)

Rates <- foreach(i = 1:cv.fold, .combine = cbind) %dopar% {
    cat("Starting ", i, "\n", sep = '')
    set.seed(seeds[i])
    ixTest <- (1+ixGroup[i]):ixGroup[i+1]
    test   <- shuffled.order[ixTest]
    train  <- shuffled.order[-ixTest]
    trainX <- X$train[train,]
    testX  <- X$train[test,]

    fitCV <- randomForest(spam ~ ., data = trainX, ntree = nTree)
    err <- rep(NA, nindex)
    for (j in 1:nindex) {
        fitCV$forest$ntree <- index[j]
        pred <- predict(fitCV, testX, type="response")
        err[j] <- sum(pred != testX$spam)/groupSizes[i]
    }
     cat("Ending ", i, "\n", sep = '')
    err
}

# Save variables
save(Rates, errVec, fit, index, 
     file = "../../dataset/spamResults/OOBvsCVvsTest.Rdata")

