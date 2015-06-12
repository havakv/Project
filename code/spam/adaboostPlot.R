#!/usr/bin/env Rscript
# Plotting for Adaboost on spam data
NOPRINT=FALSE
#NOPRINT=TRUE

library(common)

load("../../dataset/spamResults/adaboostSpam.Rdata")
## ---- plotAdaboost ----
# Different depths
printfig("adaboostSpam", NOPRINT)
ylim <- c(min(Errors), max(Errors))
plot(its, Errors[,1], type="l", xlab = "iterations", ylab = "error", ylim = ylim)
for (j in 2:ndept) {
    lines(its, Errors[, j], col = j)
}
grid()
legend(x = "topright", as.character(maxdepth), lty = rep(1, 4), lwd = rep(1, 4), col = 1:ndept, bg="white")
off(NOPRINT)
