#!/usr/bin/env Rscript
# Tuning gradient boosting using caret

library(caret)
library(e1071)
library(gbm)
library(common)
library(methods)

set.seed(0)
X <- getSpam()
# To make gbm work
#X$train$spam <- as.character(X$train$spam)
#X$test$spam <- as.character(X$test$spam)


fitControl <- trainControl(## 10-fold CV
                          method = "repeatedcv",
                          number = 10,
                          ## repeated ten times
                          repeats = 10)

gbmFit1 <- train(spam ~ ., data = X$train,
                 method = "gbm",
                 trControl = fitControl,
                 ## This last option is actually one
                 ## for gbm() that passes through
                 verbose = FALSE)
gbmFit1
pred <- predict(gbmFit1, X$test)
err <- sum(pred != X$test$spam)/X$nTest
err
