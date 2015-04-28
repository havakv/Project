#!/usr/bin/env Rscript
# Plotting for adaboost on spam data
NOPRINT=FALSE
#NOPRINT=TRUE

library(common)

load("../dataset/spamResults/adaboostSpam.Rdata")

# Different depths
printfig("adaboostSpam", NOPRINT)
ylim <- c(min(Errors), max(Errors))
plot(its, Errors[,1], type="l", xlab = "iterations", ylab = "error", ylim = ylim)
for (j in 2:ndept) {
    lines(its, Errors[, j], col = j)
}
legend(x = "topright", as.character(maxdepth), lty = rep(1, 4), lwd = rep(1, 4), col = 1:ndept, bg="white")
off(NOPRINT)
