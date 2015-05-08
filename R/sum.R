##' This is a wrapper for the normal sum function.
##'
##'
##' The function sums a vector with missing values, but when the whole
##' vector is missing it returns NA rather than zero. This function is
##' merely provided so that it does not return unexpected result for
##' users.x
##'
##' @param x The vector to be summed, see the base::sum function.
##' @param na.rm Same as base::sum
##'
##' @export
##' 

sum = function(x, na.rm = !all(is.na(x))){
    base::sum(x, na.rm = na.rm)
}
