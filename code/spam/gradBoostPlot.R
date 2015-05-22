#!/usr/bin/env Rscript
# Plotting gradBoost on Spam data
NOPRINT = FALSE

# Check if environment variables are fine.
if(system("echo $OMP_NUM_THREADS", intern = TRUE) != 1)
    stop("OMP_NUM_THREADS is not 1")

argv <- commandArgs(trailingOnly=TRUE)
if (identical(argv, character(0)))
    stop("Need number of threads as input parameter")

nCores <- as.integer(argv[1])

library(common)
library(gbm)
require(parallel) # one of the core R packages
require(doParallel)
library(foreach)

#set.seed(0) # Make sure this is the same seed!!!!!!!!!!!!
#X <- getSpam() # Remove when run next time

load("../../dataset/spamResults/gradBoostSpam.Rdata")

nit <- 40
itVec <- round(seq(1, 1000, length.out=nit))
nfit1 <- length(fit1)-1
Errors <- matrix(NA, nit, nfit1)


registerDoParallel(nCores)
err <- rep(NA, nit)
for (i in 1:nfit1) {
    fit <- fit1[[i]]
    err <- foreach(j = 1:nit, .combine = c) %dopar% {
    #for (j in 1:nit) {
        pred <- predict(fit, X$test, n.trees=itVec[j], type="response")
        pred[pred>0.5] <- 1
        pred[pred<0.5] <- 0
        sum(pred != X$test$spam)/X$nTest
    }
    Errors[, i] <- err
}

printfig("gradboostSpamShrink1", NOPRINT)
ylim <- c(min(Errors), max(Errors))
plot(itVec, Errors[,1], type="l", xlab = "iterations", ylab = "error", ylim = ylim, col = 2)
for (j in 2:nfit1) {
    lines(itVec, Errors[, j], col = j+1)
}
grid()
legend(x = "topright", as.character(fit1$shrinkVec[1:nfit1]), lty = rep(1, nfit1), lwd = rep(1, nfit1), 
       col = 1:nfit1+1, bg="white")
off(NOPRINT)

#----------------------------------------------------------------------------------------
nit1 <- 40
itVec <- round(seq(1, 1000, length.out=nit1))
nit <- 80
itVec <- c(itVec, round(seq(1000, 100000, length.out=nit)))
nit <- nit+nit1
nfit1 <- length(fit1)-1
Errors <- matrix(NA, nit, nfit1)

registerDoParallel(nCores)
err <- rep(NA, nit)
for (i in 1:nfit1) {
    fit <- fit1[[i]]
    err <- foreach(j = 1:nit, .combine = c) %dopar% {
        pred <- predict(fit, X$test, n.trees=itVec[j], type="response")
        pred[pred>0.5] <- 1
        pred[pred<0.5] <- 0
        sum(pred != X$test$spam)/X$nTest
    }
    Errors[, i] <- err
}

printfig("gradboostSpamShrink2", NOPRINT)
ylim <- c(min(Errors), max(Errors))
ylim <- c(min(Errors), 0.1)
plot(itVec, Errors[,1], type="l", xlab = "iterations", ylab = "error", ylim = ylim, col = 2)
for (j in 2:nfit1) {
    lines(itVec, Errors[, j], col = j+1)
}
grid()
legend(x = "topright", as.character(fit1$shrinkVec[1:nfit1]), lty = rep(1, nfit1), lwd = rep(1, nfit1), 
       col = 1:nfit1+1, bg="white")
off(NOPRINT)

# Find best model
bestErr1 <- matrix(NA, 2, nfit1)
rownames(bestErr1) <- c("pos", "min")
for (i in 1:nfit1) {
    minimum <- min(Errors[,i])
    pos <- which(minimum == Errors[,i])[1]
    pos <- itVec[pos]
    bestErr1[,i] <- c(pos, minimum)
}
bestErr1    


##############################################################################
# Fit 2 Stochastic gradient boosting: Different bag fractions

nit <- 100
itVec <- round(seq(1, 10000, length.out=nit))
nfit2 <- length(fit2)-1
Errors <- matrix(NA, nit, nfit2)


registerDoParallel(nCores)
err <- rep(NA, nit)
for (i in 1:nfit2) {
    fit <- fit2[[i]]
    err <- foreach(j = 1:nit, .combine = c) %dopar% {
        pred <- predict(fit, X$test, n.trees=itVec[j], type="response")
        pred[pred>0.5] <- 1
        pred[pred<0.5] <- 0
        sum(pred != X$test$spam)/X$nTest
    }
    Errors[, i] <- err
}

printfig("gradboostSpamStoch", NOPRINT)
ylim <- c(min(Errors), max(Errors))
ylim <- c(min(Errors), 0.07)
plot(itVec, Errors[,1], type="l", xlab = "iterations", ylab = "error", ylim = ylim, col = 2)
for (j in 2:nfit2) {
    lines(itVec, Errors[, j], col = j+1)
}
grid()
legend(x = "topright", as.character(fit2$bagVec[1:nfit2]), lty = rep(1, nfit2), lwd = rep(1, nfit2), 
       col = 2:(nfit2+1), bg="white")
off(NOPRINT)

times <- rep(NA, nfit2)
for (i in 1:nfit2) {
    times[i] <- fit2[[i]]$time[3]
}
times

##############################################################################
# Fit 3 Different tree depths

nit <- 100
itVec <- round(seq(1, 10000, length.out=nit))
nit3 <- length(fit3)-1
Errors <- matrix(NA, nit, nit3)


registerDoParallel(nCores)
err <- rep(NA, nit)
for (i in 1:nit3) {
    fit <- fit3[[i]]
    err <- foreach(j = 1:nit, .combine = c) %dopar% {
        pred <- predict(fit, X$test, n.trees=itVec[j], type="response")
        pred[pred>0.5] <- 1
        pred[pred<0.5] <- 0
        sum(pred != X$test$spam)/X$nTest
    }
    Errors[, i] <- err
}

printfig("gradboostSpamDepth", NOPRINT)
ylim <- c(min(Errors), max(Errors))
ylim <- c(min(Errors), 0.08)
plot(itVec, Errors[,1], type="l", xlab = "iterations", ylab = "error", ylim = ylim, col = 2)
for (j in 2:nit3) {
    lines(itVec, Errors[, j], col = j+1)
}
grid()
legend(x = "topright", as.character(fit3$interactonVec[1:nit3]), lty = rep(1, nit3), lwd = rep(1, nit3), 
       col = 1:nit3+1, bg="white")
off(NOPRINT)




