#!/usr/bin/env Rscript
# Gradient Boosting on Spam data

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
library(gbm)
library(methods)
require(parallel) # one of the core R packages
require(doParallel)
library(foreach)

X <- getSpam()
# To make gbm work
X$train$spam <- as.character(X$train$spam)
X$test$spam <- as.character(X$test$spam)

#-----------------------------------------------------------------------------
## ---- gradBoostSpam.R ----
# Get error as function of interactions for different shrinkages
shrink <- c(1, 0.1, 0.01, 0.001)
fit1 <- list()
registerDoParallel(nCores)
fit1 <- foreach(i = 1:length(shrink)) %dopar% {
    obj <- gbm(spam ~ ., distribution = "bernoulli", data = X$train, 
               n.trees=100000, shrinkage=shrink[i], bag.fraction=1, 
               keep.data=FALSE)
    obj
}
fit1$shrinkVec <- shrink
cat("Done with sim 1 \n")

###############################################################################
# Stochastic gradient boosting
shrink <- 0.01
nTrees <- 10000
bag    <- c(1, 0.75, 0.5, 0.25, 0.1)
fit2 <- list()
registerDoParallel(nCores)
fit2 <- foreach(i = 1:length(bag)) %dopar% {
    t <- system.time(
    obj <- gbm(spam ~ ., distribution = "bernoulli", data = X$train, 
               n.trees=nTrees, shrinkage=shrink, bag.fraction=bag[i], 
               keep.data=FALSE)
    )
    obj$time <- t
    obj
}
fit2$bagVec <- bag

cat("Done with sim 2 \n")

###############################################################################
# Different tree depths
shrink       <- 0.01
nTrees       <- 10000
#bag          <- 0.5
bag          <- 1
minObsInNode <- 1
interaction  <- c(1, 2, 5, 20, 40)
fit3 <- list()
registerDoParallel(nCores)
fit3 <- foreach(i = 1:length(interaction)) %dopar% {
    cat("Starting", i, "of", length(interaction), "\n")
    obj <- gbm(spam ~ ., distribution = "bernoulli", data = X$train, 
               n.trees=nTrees, shrinkage=shrink, bag.fraction=bag, 
               keep.data=FALSE, interaction.depth = interaction[i],
               n.minobsinnode = minObsInNode)
    cat("Ending", i, "of", length(interaction), "\n")
    obj
}
fit3$interactonVec <- interaction
cat("Done with sim 3 \n")

#-----------------------------------------------------------------------------
# Saving results
save(fit1, fit2, fit3, X, file = "../../dataset/spamResults/gradBoostSpam.Rdata")
cat("Done!\n")

