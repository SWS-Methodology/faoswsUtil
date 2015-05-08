##' Function to determine whether the data contains any information.
##'
##' If the data contains only missing values or zeroes then it is
##' marked as containing no information.
##'
##' @param value A numeric vector to be checked.
##' @param observationFlag The character vector representing the observation
##' flag corresponding to the value.
##' @param naFlag The value of the observation flag which indicates that data
##' is missing.  Defaults to "M".
##'
##' @return A logical value (TRUE or FALSE) indicating whether or not the
##' passed vectors contain any information.
##'
##' @export
##' 

containInfo = function (value, observationFlag, naFlag = "M"){
    
    ### Data Quality Checks
    stopifnot(length(value) == length(observationFlag))
    stopifnot(is(value, "numeric"))
    stopifnot(is(observationFlag, "character"))
    
    ifelse(all(observationFlag == naFlag) |
               sum(value, na.rm = TRUE) == 0,
           FALSE, TRUE)
}
