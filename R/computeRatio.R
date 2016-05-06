##' Function for computing ratio
##'
##' This is to avoid division by zero
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
