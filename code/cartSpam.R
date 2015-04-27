#!/usr/bin/env Rscript
# Performance test of CART on Spam data

set.seed(0)
library(common)
library(rpart)
library(methods)

X <- getSpam()

fit <- rpart(spam ~ ., data = X$train)
pred <- predict(fit, X$test, type = "class")

err <- sum(pred != X$test$spam)/X$nTest
err

plot(fit)
text(fit, use.n = FALSE)
plotcp(fit)
