#!/usr/bin/env Rscript
# Plotting for bagging
NOPRINT=FALSE
#NOPRINT=TRUE

library(common)

load("../../dataset/spamResults/baggingSpam.Rdata")
load("../../dataset/spamResults/randomForestSpam.Rdata")

# Different depths
printfig("baggingAndRFSpam", NOPRINT)
ylim <- c(min(c(errBag, errRF)), max(c(errBag, errRF)))
plot(BBag, errBag, type="l", xlab = "nr. trees", ylab = "error", ylim = ylim, col = 2)
lines(BRF, errRF, col = 3)
grid()
legend(x = "topright", c("Bagging", "RF"), lty = rep(1, 2), 
       lwd = rep(1, 2), col = 2:3, bg="white")
off(NOPRINT)


#-------------------------------------------------------------------------
# Random forests for different m's

ylim <- c(min(Errors), max(Errors))
nB <- length(B)
printfig("RFSpam", NOPRINT)
plot(mVec, Errors[1,], type = 'l', ylim = ylim, col = 2,
     xlab = "m", ylab = "error")
for (i in 1:nB) {
    lines(mVec, Errors[i,], col = i+1)
}
grid()
legend(x = "topright", as.character(B), lty = rep(1, nB), 
       lwd = rep(1, nB), col = 1:nB+1, bg="white")
preds <- 57 #number of predictors
sqrM <- floor(sqrt(preds))
abline(v = sqrM, lty = 2)
off(NOPRINT)

#----------------------------------------------------------------------------
# Different tree depths

load("../../dataset/spamResults/RFDepthSpam.Rdata")

printfig("RFTreeDepth", NOPRINT)
ylim <- c(min(Errors), 0.08)
plot(index, Errors[,1], col = 2, ylim = ylim, type = 'l', ylab = "error", xlab = "nr. trees")
for (i in 2:ncol(Errors)) {
    lines(index, Errors[,i], col = i+1)
}
grid()
legend(x = "topright", as.character(nodesize), lty = rep(1, nsizes), 
       lwd = rep(1, nsizes), col = 1:nsizes+1, bg="white")
off(NOPRINT)













