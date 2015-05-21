#!/usr/bin/env Rscript
# Difference of OOB, CV and Test error

NOPRINT=FALSE
NOPRINT=TRUE

set.seed(0)
library(common)
library(randomForest)

X <- getSpam()

fit <- randomForest(spam ~ ., data = X$train)
fit$forest$ntree <- 1
fit$forest$ntree <- 500
pred <- predict(fit, X$test, type="response")
err <- sum(pred != X$test$spam)/X$nTest
err

nTree <- 1000
fit <- randomForest(spam ~ ., data = X$train, ntree = nTree)
nindex <- 100
index <- seq(1, nTree, length.out=nindex)
errVec <- rep(NA, nindex)
for (i in 1:nindex) {
    fit$forest$ntree <- index[i]
    pred <- predict(fit, X$test, type="response")
    errVec[i] <- sum(pred != X$test$spam)/X$nTest
}

ylim <- c(min(c(errVec, fit$err.rate[,1])), 0.08)
plot(index, fit$err.rate[,1][index], ylab = "error", xlab = "nr. trees", type = "l", ylim = ylim)
lines(index, errVec, col = 4)
grid()
legend(x = "topright", c("OOB", "Test"), lty = rep(1, 2), lwd = rep(2, 2), col = c(1, 4), bg="white")

nTree <- 100
nindex <- 3
index <- seq(1, nTree, length.out=nindex)
errCV <- rep(NA, nindex)
for (i in 1:nindex) {
    fit <- rfcv(X$train[,-58], X$train$spam, cv.fold = 5, ntree = index[i])
    errCV[i] <- mean(fit$error.cv)
}

lines(index, errCV, col = 2)

