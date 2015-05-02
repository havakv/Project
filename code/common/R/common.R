# Library for common function throughout the project 


# Functions for printing to file
printfig <- function(name, NOPRINT = FALSE, height = 6){
    if (!NOPRINT){
        path <- paste('../../latex/figures/', name, '.pdf', sep = '')
        pdf(file = path, height = height)
        par(mai = c(0.6, 0.6, 0.1, 0.1)) # manipulate inner margins of subplots
        par(mgp = c(1.8, 0.7, 0)) # manipulate spacing of axis ticks, labels, text
        par(omi = c(0, 0, 0, 0)) # manipulate outer margin of full plot
    }
}
off <- function(NOPRINT=FALSE){
    if (!NOPRINT)
        invisible(dev.off())
}

#-----------------------------------------------------------------------
# Need methods for class, and MASS for mvrnorm
mkGroup <- setRefClass("groupRefClass", 
    fields = list(
        id   = "numeric",
        mean = "numeric",
        Cov  = "matrix",
        size = "numeric",
        dim  = "numeric",
        x    = "matrix"),
    methods = list(
        initialize = function(id, size, dim, 
                              mean = NULL, Cov = NULL){
            id <<- id
            size <<- size
            dim <<- dim
            if (is.null(Cov)){
                A <- matrix(runif(dim*dim, -1, 1), dim, dim)
                Cov  <<- A %*% t(A)
                if (is.null(mean)) {
                    mean <<- runif(dim)*4
                } else {
                    mean <<- mean
                }
                x    <<- mvrnorm(size, .self$mean, .self$Cov)
            }
            else {
                Cov  <<- Cov
                if (is.null(mean)) {
                    mean <<- runif(dim)*4
                } else {
                    mean <<- mean
                }
                x    <<- mvrnorm(size, .self$mean, .self$Cov)
            }
        },

        show = function(){ # 'print' method
            cat("Group: ", id, " with ", size, " datapoints.\n", sep = '')
        },
        plot = function(add = FALSE, ...){
            if (!dim == 2)
                stop("Not 2-D")
            if (add){
                points(x, pch = as.character(id), col = id+1, ...)
            } else {
                plot.default(x, pch = as.character(id), col = id+1, ...)
            }
        }
    )
)


# S3 class for plotting groups

collectGroups <- function(groups){
    if (!is.list(groups))
        stop("Not a list")
    class(groups) <- "groups"
    return(groups)
}

getX <- function(groups, mat = FALSE){
    sizes <- rep(NA, length(groups))
    for (i in 1:length(groups)){
        sizes[i] <- groups[[i]]$size
    }
    X <- matrix(NA, sum(sizes), groups[[i]]$dim+1)
    pos <- cumsum(c(1, sizes))
    for (i in 1:length(groups)){
        X[pos[i]:(pos[i+1]-1),] = c(groups[[i]]$x, 
                                    rep(groups[[i]]$id, groups[[i]]$size))
    }
    if (!mat){
        X = data.frame(X)
        names(X)[dim(X)[2]] = "Y"
        X$Y = as.factor(X$Y)
    }

    return(X)
}

plot.groups <- function(groups, ...){
    n <- length(groups)
    xmin = ymin = Inf
    xmax = ymax = -Inf
    for (i in 1:n){
        xmin <- min(c(xmin, min(groups[[i]]$x[,1])))
        ymin <- min(c(ymin, min(groups[[i]]$x[,2])))
        xmax <- max(c(xmax, max(groups[[i]]$x[,1])))
        ymax <- max(c(ymax, max(groups[[i]]$x[,2])))
    }
    groups[[1]]$plot(xlim = c(xmin, xmax), 
                     ylim = c(ymin, ymax), ...)
    for (i in 2:length(groups)){
        groups[[i]]$plot(add = TRUE, ...)
    }
}


getSpam <- function(path = "../../dataset", testToTrainRatio = 1) {
    # Get dataset: Spam
    X <- read.csv(paste(path, "/spambase.dat", sep = ''), skip = 62, header=FALSE)
    names(X)[58] <- "spam"
    X$spam <- as.factor(X$spam)

    # Partition data in training and test set
    nData  <- nrow(X)
    nTrain <- round(nData/(testToTrainRatio+1))
    nTest  <- nData - nTrain

    shuffledIX <- sample(nData)

    Xtrain <- X[shuffledIX[1:nTrain] ,]
    Xtest  <- X[shuffledIX[(nTrain+1):nData] ,]

    L <- list(train = Xtrain, test = Xtest, nTrain = nTrain, nTest = nTest)
    return(L)
}

getPhoneme <- function(path = "../../dataset", testToTrainRatio = 1) {
    X <- read.csv(paste(path, "/phoneme.dat", sep = ''), skip = 10, header=FALSE)
    #X <- read.csv(paste(path, "/adult.dat", sep = ''), skip = 90, header=FALSE, colClasses=rep("factor", 86))
    names(X)[6] <- "class"
    X$class <- as.factor(X$class)

    # Partition data in training and test set
    nData  <- nrow(X)
    nTrain <- round(nData/(testToTrainRatio+1))
    nTest  <- nData - nTrain

    shuffledIX <- sample(nData)

    Xtrain <- X[shuffledIX[1:nTrain] ,]
    Xtest  <- X[shuffledIX[(nTrain+1):nData] ,]

    L <- list(train = Xtrain, test = Xtest, nTrain = nTrain, nTest = nTest)
    return(L)
}

getAdult <- function(path = "../../dataset", testToTrainRatio = 1, tot.size = 5000) {
    X <- read.csv(paste(path, "/adult.dat", sep = ''), skip = 19, header=FALSE)
    #X <- read.csv(paste(path, "/adult.dat", sep = ''), skip = 90, header=FALSE, colClasses=rep("factor", 86))
    names(X)[15] <- "class"

    if (tot.size > nrow(X))
        stop(paste("tot.size needs to be smaller than", nrow(X)))
    X <- X[sample(nrow(X), tot.size),]

    # Get 50/50 of class 0 and 1
    #X0 <- filter(X, class == 0)
    #X1 <- filter(X, class == 1)
    #X0 <- X[sample(nrow(X0), nrow(X1)),]
    #X  <- rbind(X0, X1)


    # Partition data in training and test set
    nData  <- nrow(X)
    nTrain <- round(nData/(testToTrainRatio+1))
    nTest  <- nData - nTrain

    shuffledIX <- sample(nData)

    Xtrain <- X[shuffledIX[1:nTrain] ,]
    Xtest  <- X[shuffledIX[(nTrain+1):nData] ,]

    L <- list(train = Xtrain, test = Xtest, nTrain = nTrain, nTest = nTest)
    return(L)
}

getInsurance <- function(path = "../../dataset", testToTrainRatio = 0.5) {
    X <- read.csv(paste(path, "/coil2000.dat", sep = ''), skip = 90, header=FALSE, colClasses=rep("factor", 86))
    names(X)[86] <- "class"

    # Get 50/50 of class 0 and 1
    X0 <- filter(X, class == 0)
    X1 <- filter(X, class == 1)
    X0 <- X[sample(nrow(X0), nrow(X1)),]
    X  <- rbind(X0, X1)


    # Partition data in training and test set
    nData  <- nrow(X)
    nTrain <- round(nData/(testToTrainRatio+1))
    nTest  <- nData - nTrain

    shuffledIX <- sample(nData)

    Xtrain <- X[shuffledIX[1:nTrain] ,]
    Xtest  <- X[shuffledIX[(nTrain+1):nData] ,]

    L <- list(train = Xtrain, test = Xtest, nTrain = nTrain, nTest = nTest)
    return(L)
}

getTheoProve <- function(path = "../../dataset", testToTrainRatio = 1) {
    # Get dataset on Theorem proving
    X <- read.csv(paste(path, "/ml-prove/all-data-raw.csv", sep = ''), header=FALSE)

    # Clean data to be able to predict. 
    # 5 classes:
    # 1-5 is best class 
    test <- select(X, V54:V58)
    test <- filter(test, V54 != -100 & V55 != -100 & V56 != -100 & V57 != -100 & V58 != -100)
    minVals = apply(test, 1, min)
    a = rep(NA, nrow(test))
    for (i in 1:nrow(test)) {
        a[i] = sum(minVals[i] == test[i,])
    }
    test <- test[a==1,]
    test$class <- apply(test, 1, function(x){which(min(x) == x)})
    ix <- as.numeric(rownames(test))
    X <- cbind(X[ix,1:53], class = as.factor(test$class))
    

    # Partition data in training and test set
    nData  <- nrow(X)
    nTrain <- round(nData/(testToTrainRatio+1))
    nTest  <- nData - nTrain

    shuffledIX <- sample(nData)

    Xtrain <- X[shuffledIX[1:nTrain] ,]
    Xtest  <- X[shuffledIX[(nTrain+1):nData] ,]

    L <- list(train = Xtrain, test = Xtest, nTrain = nTrain, nTest = nTest)
    return(L)
}
