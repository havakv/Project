#!/usr/bin/env Rscript
# Performance test of CART on Spam data
NOPRINT=FALSE
#NOPRINT=TRUE

set.seed(0)
library(common)
library(rpart)
library(methods)

X <- getSpam()

cp <- 1e-6
control <- rpart.control(minsplit = 1, cp = cp, maxdept = 30)
fit <- rpart(spam ~ ., data = X$train, control = control)

cpVec <- fit$cptable[, "CP"]
nsplit <- fit$cptable[, "nsplit"]
ncp <- length(cpVec)
err <- rep(NA, ncp)
for (i in 1:ncp) {
    pfit <- prune(fit, cp = cpVec[i])
    ppred <- predict(pfit, X$test, type = "class")
    err[i] <- sum(ppred != X$test$spam)/X$nTest
}

printfig("cartCPSpam", NOPRINT)
ylim <- c(min(err), 0.15)
plot(nsplit, err, type = 'l', ylab = "error", xlab = "nr of splits",
     ylim = ylim, col = "blue")
grid()
off(NOPRINT)

printfig("cartFullSpam", NOPRINT)
plot(fit)
text(fit, use.n = FALSE, splits = FALSE, cex = 0.5)
off(NOPRINT)
printfig("cartOptSpam", NOPRINT)
pfit <- prune(fit, cp = cpVec[which.min(err)])
plot(pfit)
text(pfit, use.n = FALSE, splits = FALSE)
off(NOPRINT)
    
quit()

pred <- predict(fit, X$test, type = "class")

err <- sum(pred != X$test$spam)/X$nTest
err

plot(fit)
plotcp(fit)
printcp(fit)
#summary(fit)
pfit<- prune(fit, cp=fit$cptable[which.min(fit$cptable[,"xerror"]),"CP"])
pfit<- prune(fit, cp = 0.06113538)
# plot the pruned tree 
plot(pfit)
ppred <- predict(pfit, X$test, type = "class")
err <- sum(ppred != X$test$spam)/X$nTest
err




#---------------------------------
# Standard
fit <- rpart(spam ~ ., data = X$train)
pred <- predict(fit, X$test, type = "class")

err <- sum(pred != X$test$spam)/X$nTest
err

plot(fit)
text(fit, use.n = FALSE)
plotcp(fit)
printcp(fit)
#summary(fit)
pfit<- prune(fit, cp=fit$cptable[which.min(fit$cptable[,"xerror"]),"CP"])
# plot the pruned tree 
plot(pfit)
text(fit, use.n = FALSE)
#---------------------------------

control <- rpart.control(minsplit = 1, cp = 1e-6, maxdept = 30)
fit <- rpart(spam ~ ., data = X$train, control = control)
pred <- predict(fit, X$test, type = "class")

err <- sum(pred != X$test$spam)/X$nTest
err

plot(fit)
plotcp(fit)
printcp(fit)
#summary(fit)
pfit<- prune(fit, cp=fit$cptable[which.min(fit$cptable[,"xerror"]),"CP"])
pfit<- prune(fit, cp = 0.06113538)
# plot the pruned tree 
plot(pfit)
ppred <- predict(pfit, X$test, type = "class")
err <- sum(ppred != X$test$spam)/X$nTest
err







plot(1:10,1:10,bty="n",col="red",pch=16,axes=FALSE,xlab="",ylab="")
axis(2,0:11,las=1)
axis(1,0:11,line=1,col="red",col.ticks="red",col.axis="red")
mtext("Label 1",1,line=1,at=0.2,col="red")

# Secondary points and axis:
points(rnorm(10,50,20)/10, rnorm(10,5,2),pch=16, col="blue" )
axis(1,0:11,labels=0:11*10,line=3,col="blue",col.ticks="blue",col.axis="blue")
mtext("Label 2",1,line=3,at=0.2,col="blue")




