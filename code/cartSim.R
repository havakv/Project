#!/usr/bin/env Rscript
# Simulation to create a pretty picture of how CART works

NOPRINT = FALSE # Set to TRUE if you don't want to print figures to pdf files.
library(methods)
library(common)
library(rpart)
set.seed(929253)

# ----------------------------------------------------
# CART simulations with 3 groups and 2 dimensions

# Create data points
N <- 20
D <- 2
g2 <- mkGroup(2, N, D) # group 2
g22 <- mkGroup(2, N/2, D) # Also group 2
g1 <- mkGroup(1, N, D) # group 1
g3 <- mkGroup(3, N, D) # group 3
sim1 <- collectGroups(list(g1, g2, g22, g3))
X <- getX(sim1)              

# fit CART
fit <- rpart(Y~., data = X)

# plot tree
printfig("cartTree1", NOPRINT)
plot(fit)
text(fit, use.n = FALSE)
off(NOPRINT)

# plot areas. NOT GENERAL!!!!!
printfig("cartAreas1", NOPRINT)
plot(sim1, xlab = "X1", ylab = "X2")
sp <- fit$split
abline(h = sp[1,4])
segments(sp[2,4], sp[1,4], sp[2,4], 10)
segments(sp[4,4], sp[1,4], sp[4,4], -10)
segments(sp[4,4], sp[5,4], 10, sp[5,4])
off(NOPRINT)


