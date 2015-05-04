#!/usr/bin/env Rscript
# Performance test of CART on Phoneme data
NOPRINT=FALSE
#NOPRINT=TRUE

set.seed(0)
library(common)
library(rpart)
library(methods)

X <- getPhoneme()

cp <- 1e-6
control <- rpart.control(minsplit = 1, cp = cp, maxdept = 30)
fit <- rpart(class ~ ., data = X$train, control = control)

cpVec <- fit$cptable[, "CP"]
nsplit <- fit$cptable[, "nsplit"]
ncp <- length(cpVec)
err <- rep(NA, ncp)
for (i in 1:ncp) {
    pfit <- prune(fit, cp = cpVec[i])
    ppred <- predict(pfit, X$test, type = "class")
    err[i] <- sum(ppred != X$test$class)/X$nTest
}

printfig("cartCPPhoneme", NOPRINT)
#ylim <- c(min(err), max(err))
ylim <- c(min(err), 0.25)
plot(nsplit, err, type = 'l', ylab = "error", xlab = "nr of splits",
     ylim = ylim, col = "blue")
grid()
off(NOPRINT)


#quit()

