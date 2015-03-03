# Library for common function throughout the project 


# Functions for printing to file
printfig <- function(name, NOPRINT = FALSE, height = 6){
    if (!NOPRINT){
        path <- paste('../latex/figures/', name, '.pdf', sep = '')
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

