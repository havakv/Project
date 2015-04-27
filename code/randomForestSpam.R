#!/usr/bin/env Rscript
    
set.seed(0)

# Check if environment variables are fine.
if(system("echo $OMP_NUM_THREADS", intern = TRUE) != 1)
    stop("OMP_NUM_THREADS is not 1")

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

mVec <- round(seq(5, 58, length.out = 20))
B <- round(seq(100, 2000, length.out = 4))
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


save(Errors, mVec, B, file = "../dataset/spamResults/randomForestSpam.Rdata")
quit()

#---------------------------------------------------------------------------
fit <- randomForest(spam ~ ., data = X$train)
pred <- predict(fit, X$test, type="response")
err <- sum(pred != X$test$spam)/X$nTest
err

#---------------------------------------------------------------------------

sortPred.optRF <- function(X, y, ...){
    rfTuned <- tuneRF(X, y,
                      trace = FALSE, plot = FALSE, doBest = TRUE, ...)
    if (length(rfTuned$importance) != ncol(X))
        stop("importance does not contain all variables")
    sortPred <- sort(rfTuned$importance, decreasing = TRUE, 
                     index.return = TRUE)$ix
    return(sortPred)
}

bestPred.optRF <- function(X, y, nfold = 5, ...){
    # Find index for the best predictors in optRF
    cv <- rfcv(X, y, cv.fold = nfold, ...)
    nrPred <- cv$n.var[which(cv$error.cv == min(cv$error.cv))]
    if (length(nrPred) > 1)
        nrPred <- min(nrPred)
    sortPred <- sortPred.optRF(X, y, ...)
    bestPred <- sortPred[1:nrPred]
    return(bestPred)
}

optRF <- function(X, y, nfold = 5, ...){
    # Optimal RF, by using crossvalidation to find the best set of predictors
    bestPred <- bestPred.optRF(X, y, nfold, ...)
    rfOpt <- tuneRF(X[,bestPred], y,
                    trace = FALSE, plot = FALSE, doBest = TRUE, ...)
    rfOpt$Xnames <- names(X)
    rfOpt$bestPred <- bestPred
    class(rfOpt)  <- c("optRF", class(rfOpt))
    return(rfOpt)
}

predict.optRF <- function(obj, testX){
    if(class(testX) != "data.frame")
        testX <- data.frame(testX)
    names(testX) <- obj$Xnames
    class(obj) <- "randomForest"
    pred <- predict(obj, testX[,obj$bestPred], type = "response")
    return(pred)
}

optFit <- optRF(X$train[,-58], X$train$spam)
optPred <- predict(optFit, X$test[,-58])
err <- sum(optPred != X$test$spam)/X$nTest
err
