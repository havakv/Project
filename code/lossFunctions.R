#!/usr/bin/env Rscript
NOPRINT <- FALSE    
#NOPRINT <- TRUE
library(common)


sqErr     <- function(y, f) {return((y-f)^2)}
expo      <- function(y, f) {return(exp(-y*f))}
#missClass <- function(y, f) {return(y!=sign(f))}
negBinlog <- function(y, f) {return(log(1+exp(-2*y*f))/log(2))}

n <- 200
fmax <- 2.5
f <- rep(seq(-fmax, fmax, length.out=n), 2)
y <- c(rep(-1, n), rep(1, n))

x <- y*f
ord <- sort(x, index.return = TRUE)
x <- ord$x

xlim <- c(-1, 2)
ylim <- c(0, 4.5)


printfig2("lossFunctions", NOPRINT)
plot(x, expo(y, f)[ord$ix], type = "l", col = 1, xlab="yf", 
     ylab="Loss", xlim = xlim, ylim = ylim)
#lines(x, missClass(y, f)[ord$ix], col = 3)
missx <- c(min(y*f), 0, 0, max(y*f))
missy <- c(1, 1, 0, 0)
lines(missx, missy, col = 3)
lines(x, sqErr(y, f)[ord$ix], col = 2)
lines(x, negBinlog(y, f)[ord$ix], col = 4)
grid()
legend(x = "topright", c("exponential", "squared-error", "missclassification",
                         "negative binomial"), 
       lty = rep(1, 4), lwd = rep(1, 4), col = 1:4, bg="white")
off(NOPRINT)
