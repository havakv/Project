#!/usr/bin/env Rscript
# Different tree depths of random forest

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

## ---- RFDepthSpam.R ----
nodesize <- c(1, 5, 10, 40)
nsizes <- length(nodesize)
nTree <- 600
nindex <- 100
index <- round(seq(1, nTree, length.out=nindex))

registerDoParallel(nCores)
Errors <- matrix(NA, nindex, nsizes)

seeds <- sample.int(1e7, nsizes)
Errors <- foreach(i = 1:nsizes, .combine = cbind) %dopar% {
    cat("Starting ", i, "\n", sep = '')
    set.seed(seeds[i])

    fit <- randomForest(spam ~ ., data = X$train, ntree = nTree, 
                        nodesize = nodesize[i])
    err <- rep(NA, nindex)
    for (j in 1:nindex) {
        fit$forest$ntree <- index[j]
        pred <- predict(fit, X$test, type="response")
        err[j] <- sum(pred != X$test$spam)/X$nTest
    }
     cat("Ending ", i, "\n", sep = '')
    err
}


# Save variables
save(Errors, index, nodesize, nsizes, 
     file = "../../dataset/spamResults/RFDepthSpam.Rdata")


