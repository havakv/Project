#!/usr/bin/env Rscript
# CART on Spam data
NOPRINT=FALSE

set.seed(0)
library(common)
library(rpart)
library(methods)

X <- getSpam()

# Using the Ginie splitting criterion 
cp <- 1e-6
control <- rpart.control(minsplit = 1, cp = cp, maxdept = 30)
fit <- rpart(spam ~ ., data = X$train, control = control)

cpVec <- fit$cptable[, "CP"]
nsplit <- fit$cptable[, "nsplit"]
ncp <- length(cpVec)
err <- rep(NA, ncp)
for (i in 1:ncp) {
    pfit <- prune(fit, cp = cpVec[i])
    ppred <- predict(pfit, X$test, type = "class")
    err[i] <- sum(ppred != X$test$spam)/X$nTest
}


printfig("cartOptSpam", NOPRINT)
pfit <- prune(fit, cp = cpVec[which.min(err)])
par(col = "blue")
plot(pfit, compress=TRUE)
text(pfit, use.n = FALSE, splits = FALSE, col = 3, cex = 0.8)
off(NOPRINT)
printfig("cartSmallSpam", NOPRINT)
ns <- 16
sfit <- prune(fit, cp = cpVec[ns])
par(col = "blue")
plot(sfit)
text(sfit, use.n = FALSE, splits = FALSE, col = 2)
off(NOPRINT)


# Using the deviance splitting criterion  "information"
parms = list(split = "information")
fitDev <- rpart(spam ~ ., data = X$train, control = control, parms = parms)
cpVecDev <- fitDev$cptable[, "CP"]
nsplitDev <- fitDev$cptable[, "nsplit"]
ncpDev <- length(cpVecDev)
errDev <- rep(NA, ncpDev)
for (i in 1:ncpDev) {
    pfitDev <- prune(fitDev, cp = cpVecDev[i])
    ppredDev <- predict(pfitDev, X$test, type = "class")
    errDev[i] <- sum(ppredDev != X$test$spam)/X$nTest
}

printfig("cartOptDevianceSpam", NOPRINT)
pfitDev <- prune(fitDev, cp = cpVecDev[which.min(errDev)])
plot(pfitDev, compress=TRUE)
text(pfitDev, use.n = FALSE, splits = FALSE, col = 6, cex = 0.8)
off(NOPRINT)

# Plotting both geni and deviance 
printfig("cartCPSpam", NOPRINT)
ylim <- c(min(c(err, errDev)), 0.15)
plot(nsplit, err, type = 'l', ylab = "error", xlab = "nr of splits",
     ylim = ylim, col = "blue")
lines(nsplitDev, errDev, type = 'l', ylab = "error", xlab = "nr of splits",
     ylim = ylim, col = 1)
abline(v = nsplit[ns], lty = 2, col = 2)
abline(v = nsplit[which.min(err)], lty = 2, col = 3)
abline(v = nsplitDev[which.min(errDev)], lty = 2, col = 6)
grid()
legend(x = "topright", c("Gini", "Deviance"), lty = rep(1, 2), lwd = rep(2, 2), col = c(4, 1), bg="white")
off(NOPRINT)

