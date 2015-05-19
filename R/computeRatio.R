##' Function for computing the yield
##'
##' This is a function to compute yield based on production and area
##' harvested.
##'
##' @param numerator The value of the numerator.
##' @param denominator The value of the denominator.
##'
##' @export
##'
##' 

computeRatio = function(numerator, denominator){
    as.numeric(ifelse((numerator == 0 & denominator == 0) |
                      denominator == 0, NA,
                      numerator/denominator))
}