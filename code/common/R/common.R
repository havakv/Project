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

