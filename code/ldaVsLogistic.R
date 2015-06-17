#!/usr/bin/env Rscript
NOPRINT = FALSE # Set to TRUE if you don't want to print figures to pdf files.
#NOPRINT = TRUE # Set to TRUE if you don't want to print figures to pdf files.

# Simulation to show difference of LDA and Logistic Regression
library(MASS)
#source("../R/common.R")
library(common)
set.seed(93485038)

# ----------------------------------------------------
# Experiment 1

# Training data
N <- 200
D <- 2
A <- matrix(runif(D*D, -1, 1), D, D)
Sigma <- A %*% t(A)
mu1 <- runif(D)*4
x1 <- mvrnorm(N, mu1, Sigma)        # Group 1
mu2 <- mu1 + runif(D, -1, -1)*2     # Group 2
x2 <- mvrnorm(N, mu2, Sigma)
X <- data.frame(rbind(x1, x2))
Y <- c(rep(0, N), rep(1, N))

# Test data
Ntest <- 100000
x1test <- mvrnorm(Ntest, mu1, Sigma)
x2test <- mvrnorm(Ntest, mu2, Sigma)
Xtest <- data.frame(rbind(x1test, x2test))
Ytest <- c(rep(0, Ntest), rep(1, Ntest))

# LDA
fitLda <- lda(X, Y, CV=FALSE) 
predLda <- predict(fitLda, Xtest)

# Misclassification error
mceLda <- sum(predLda$class != Ytest)/Ntest
#cat("LDA MCE:\t", mceLda, "\n")


# Logistic
trainData <- cbind(X,Y)
fitLog <- glm(Y ~ ., data = trainData, family = "binomial")
predLog <- predict(fitLog, Xtest)
predLog[predLog>0] <- 1
predLog[predLog<0] <- 0
mceLog <- sum(predLog != Ytest)/Ntest
#cat("Logistic MCE:\t", mceLog, "\n")

# Plot
printfig2("ldaVsLogistic", NOPRINT)
xlim <- c(min(c(x1[,1], x2[,1])), max(c(x1[,1], x2[,1])))
ylim <- c(min(c(x1[,2], x2[,2])), max(c(x1[,2], x2[,2])))
plot(x1, col = 'blue', pch = 1, xlim = xlim, ylim = ylim,
     xlab = "x1", ylab = "x2") 
points(x2, col = 'red', pch = "+")

# Find decision boundary
# Logistic
betaLog <- fitLog$coefficients
abline(-betaLog[1]/betaLog[3], -betaLog[2]/betaLog[3], col = 6)

# Lda
#betaLdaFromFit <- coef(fitLda) #Get the same as betaLda[2:3]
covLda <- (cov(x1)+cov(x2))*(N-1)/(N-2)
m2minusm1 <- matrix((fitLda$means[2,]-fitLda$means[1,]), D, 1)
betaLda <- c(log(fitLda$prior[2]/fitLda$prior[1]) - 
             0.5 * matrix((fitLda$means[2,]+fitLda$means[1,]), 1, 2) %*% solve(covLda, m2minusm1),
             solve(covLda, m2minusm1))
names(betaLda) <- c("Intercept", "X1", "X2")
abline(-betaLda[1]/betaLda[3], -betaLda[2]/betaLda[3], col = 3)

# Optimal classifier
piOpt <- table(Y)/(2*N)
m2minusm1Opt <- matrix((mu2-mu1), D, 1)
betaOpt <- c(log(piOpt[2]/piOpt[1]) - 
             0.5 * matrix((mu2+mu1), 1, 2) %*% solve(Sigma, m2minusm1Opt),
             solve(Sigma, m2minusm1Opt))
names(betaOpt) <- c("Intercept", "X1", "X2")
abline(-betaOpt[1]/betaOpt[3], -betaOpt[2]/betaOpt[3], col = 1)


legend(x = "topright", c("LDA", "Logistic", "Optimal"), lty = c(1, 1, 1),
       lwd = c(1, 1, 1), col = c(3, 6, 1))
off(NOPRINT)

############
# LDA optimal
############
Nopt <- 1e5
x1opt <- mvrnorm(Nopt, mu1, Sigma)       
x2opt <- mvrnorm(Nopt, mu2, Sigma)
Xopt <- data.frame(rbind(x1opt, x2opt))
Yopt <- c(rep(0, Nopt), rep(1, Nopt))

# LDA
fitLdaopt <- lda(Xopt, Yopt, CV=FALSE) 
predLdaopt <- predict(fitLdaopt, Xtest)

# Misclassification error
mceLdaopt <- sum(predLdaopt$class != Ytest)/Ntest
#cat("LDA MCE:\t", mceLdaopt, "\n")
