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
    ifelse(denominator == 0, NA_real_, numerator/denominator)
}
