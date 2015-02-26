#!/usr/bin/env Rscript
# Simulation to show difference of LDA and Logistic Regression
library(MASS)
set.seed(93485038)

# Experiment 1
N <- 1000
D <- 2

# Group 1
A <- matrix(runif(D*D, -1, 1), D, D)
Sigma <- A %*% t(A)
mu1 <- runif(D)*4
x1 <- mvrnorm(N, mu1, Sigma)


# Group 2
mu2 <- mu1 + runif(D, -1, -1)*2

x2 <- mvrnorm(N, mu2, Sigma)

xlim <- c(min(c(x1[,1], x2[,1])), max(c(x1[,1], x2[,1])))
ylim <- c(min(c(x1[,2], x2[,2])), max(c(x1[,2], x2[,2])))

X <- data.frame(rbind(x1, x2))
Y <- c(rep(0, N), rep(1, N))

fitLda <- lda(X, Y, CV=FALSE) 
betaLda <- coef(fitLda)

plot(x1, col = 'blue', pch = 1, xlim = xlim, ylim = ylim) 
points(x2, col = 'red', pch = "+")


# Prediction
Ntest <- 100000
x1test <- mvrnorm(Ntest, mu1, Sigma)
x2test <- mvrnorm(Ntest, mu2, Sigma)

Xtest <- data.frame(rbind(x1test, x2test))
Ytest <- c(rep(0, Ntest), rep(1, Ntest))

predLda <- predict(fitLda, Xtest)

# Misclassification error
mceLda <- sum(predLda$class != Ytest)/Ntest
print(mceLda)


# Logistic
trainData <- cbind(X,Y)
fitLog <- glm(Y ~ ., data = trainData, family = "binomial")
#testData <- cbind(Xtest, Ytest)
predLog <- predict(fitLog, Xtest)
predLog[predLog>0] <- 1
predLog[predLog<0] <- 0
mceLog <- sum(predLog != Ytest)/Ntest
print(mceLog)


# Find decision boundary
points(Xtest[predLda$class == 1,])
#plot(Xtest[predLda$class == 1,])
#abline(3, betaLda[1]/ betaLda[2], col = 'green')
abline(betaLda[2]/betaLda[1], betaLda[1])
