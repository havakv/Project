#!/usr/bin/env Rscript
# Plotting OOB CV and test error
NOPRINT <- FALSE
library(common)
    
load("../../dataset/spamResults/OOBvsCVvsTest.Rdata")

## ---- OOBvsCVvsTestPlot.R ----
printfig("OOBvsTestvsCV", NOPRINT)
ylim <- c(min(c(errVec, fit$err.rate[,1])), 0.08)
plot(index, fit$err.rate[,1][index], ylab = "error", xlab = "nr. trees", 
     type = "l", ylim = ylim, col = 3)
lines(index, errVec, col = 4)
lines(index, rowMeans(Rates), col = 2)
grid()
legend(x = "topright", c("OOB", "Test", "CV"), lty = rep(1, 3), 
       lwd = rep(2, 3), col = c(3, 4, 2), bg="white")
