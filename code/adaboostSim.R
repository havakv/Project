#!/usr/bin/env Rscript
NOPRINT=FALSE
set.seed(0)
library(common)    
library(adabag)
X <- getSpam()


# AdaBoost.M1

# mfinal = nr of boosting iterations/nr of trees used.
maxdepth <- c(1, 3, 10, 30)
its      <- round(seq(10, 300, length.out = 10))
ndept    <- length(maxdepth)
nit      <- length(its)
Errors <- matrix(NA, nit, ndept)

count <- 1
for (j in 1:ndept) {
    for (i in 1:nit) {
        cat(count, " of ", nit*ndept, "\n")
        count <- count + 1

        ada2 <- boosting(spam ~ ., X$train, boos=FALSE, mfinal=its[i], coeflearn="Freund",
                         control = rpart.control(maxdepth=maxdepth[j]))
        predAda2 <- predict(ada2, X$test)
        Errors[i, j]  <-  sum(predAda2$class != X$test$spam)/X$nTest
    }
}

printfig("adaboost", NOPRINT)
ylim <- c(min(Errors), max(Errors))
plot(its, Errors[,1], type="l", xlab = "iterations", ylab = "error", ylim = ylim)
for (j in 2:ndept) {
    lines(its, Errors[, j], col = j)
}
legend(x = "topright", as.character(maxdepth), lty = rep(1, 4), lwd = rep(1, 4), col = 1:ndept, bg="white")
off(NOPRINT)


save(maxdepth, its, ndept, nit, Errors, file = "../latex/dataR/adaboostSim.Rdata")







#ada2 <- boosting(spam ~ ., X$train, boos=FALSE, mfinal=100, coeflearn="Freund")
#predAda2 <- predict(ada2, X$test)
#sum(predAda2$class != X$test$spam)/X$nTest


#ada3 <- boosting(spam ~ ., X$train, boos=TRUE, mfinal=100, coeflearn="Freund")
#predAda3 <- predict(ada3, X$test)
#sum(predAda3$class != X$test$spam)/X$nTest



#ada3 <- boosting(spam ~ ., X$train, boos=FALSE, mfinal=100, coeflearn="Freund", 
                 #control = rpart.control(maxdepth=1))
#ada3 <- boosting(spam ~ ., X$train, boos=FALSE, mfinal=100, coeflearn="Freund")
#predAda3 <- predict(ada3, X$test)
#sum(predAda3$class != X$test$spam)/X$nTest
