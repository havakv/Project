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

gbmGrid <-  expand.grid(interaction.depth = c(1, 2, 3, 5, 9),
                        n.trees = round(seq(1000, 100000, length.out = 30)),
                        shrinkage = 0.001)

gbmFit1 <- train(spam ~ ., data = X$train,
                 method = "gbm",
                 trControl = fitControl,
                 ## This last option is actually one
                 ## for gbm() that passes through
                 verbose = FALSE,
                 tuneGrid = gbmGrid)
gbmFit1
pred <- predict(gbmFit1, X$test)
err <- sum(pred != X$test$spam)/X$nTest
err
