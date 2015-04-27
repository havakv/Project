#!/usr/bin/env Rscript
# Bagging used on spam data

set.seed(0)

library(common)
library(ipred)
library(methods)

X <- getSpam()

B <- round(seq(5, 2000, length.out = 25))
err <- rep(NA,length(B))
for (i in 1:length(B)) {
    fit <- bagging(spam ~ ., data = X$train, nbagg = B[i])
    pred <- predict(fit, X$test)
    err[i] <- sum(pred != X$test$spam)/X$nTest
}

save(err, B, file = "../dataset/spamResults/baggingSpam.Rdata")

quit()




#-----------------------------------------------------------------------------
fit <- bagging(spam ~ ., data = X$train)
pred <- predict(fit, X$test)
err <- sum(pred != X$test$spam)/X$nTest
err
