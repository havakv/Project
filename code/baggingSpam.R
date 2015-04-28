#!/usr/bin/env Rscript
# Bagging used on spam data

set.seed(0)
# Check if environment variables are fine.
if(system("echo $OMP_NUM_THREADS", intern = TRUE) != 1)
    stop("OMP_NUM_THREADS is not 1")

argv <- commandArgs(trailingOnly=TRUE)
if (identical(argv, character(0)))
    stop("Need number of threads as input parameter")

nCores <- as.integer(argv[1])

library(common)
library(ipred)
library(methods)
require(parallel) # one of the core R packages
require(doParallel)
library(foreach)

X <- getSpam()

#B <- round(seq(5, 2000, length.out = 25))
B <- round(seq(5, 10, length.out = 2))
err <- rep(NA,length(B))

registerDoParallel(nCores)
err <- foreach(i = 1:length(B), .combine = c) %dopar% {
    cat("Starting", i, "of", length(B), "\n")
    fit <- bagging(spam ~ ., data = X$train, nbagg = B[i])
    pred <- predict(fit, X$test)
    cat("Ending", i, "of", length(B), "\n")
    sum(pred != X$test$spam)/X$nTest
}

errBag <- err
BBag <- B
save(errBag, BBag, file = "../dataset/spamResults/baggingSpam.Rdata")

quit()




#-----------------------------------------------------------------------------
fit <- bagging(spam ~ ., data = X$train)
pred <- predict(fit, X$test)
err <- sum(pred != X$test$spam)/X$nTest
err
