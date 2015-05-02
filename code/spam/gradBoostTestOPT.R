#!/usr/bin/env Rscript
# Trying to get optimal performance from gradient boosting

set.seed(0)

library(common)
library(gbm)
library(methods)

X <- getSpam()
# To make gbm work
X$train$spam <- as.character(X$train$spam)
X$test$spam <- as.character(X$test$spam)

shrink      <- c(0.1, 0.01, 0.001)
interDepth  <- c(1, 2, 3)
testPars    <- matrix(NA, 2, length(shrink)*length(interDepth))
rownames(testPars) <- c("shrink", "interDepth")
testPars[1,] <- rep(shrink, 1, each=length(interDepth))
testPars[2,] <- rep(interDepth, length(shrink))

errors <- rep(NA, dim(testPars)[2])
for (i in 1:dim(testPars)[2]) {
    cat("Iteration:", i, "of", dim(testPars)[2], "\n")
    fit <- gbm(spam ~ ., distribution = "bernoulli", data = X$train, 
               n.trees=50000, cv.folds=5, n.cores=5, 
               shrinkage=testPars[1,i], interaction.depth = testPars[2,i])
    gbmPerf <- gbm.perf(fit, plot.it=FALSE)
    if(gbmPerf == 50000)
        cat("gbmPerf is max")
    pred <- predict(fit, X$test, n.trees=gbmPerf, type="response")
    pred[pred>0.5] <- 1
    pred[pred<0.5] <- 0
    errors[i] <- sum(pred != X$test$spam)/X$nTest
}

