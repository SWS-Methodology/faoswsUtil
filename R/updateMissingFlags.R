##' Update Missing Flags
##' 
##' Some data from the statistical working system may have missing values for
##' observations but the observation flag may indicate that this value is not
##' missing.  This function updates those flags by assigning them a missing
##' flag ("M") in the case where that observation is missing.  This function is
##' similar to remove0M, which assigns NA's to values where the observation
##' flag is "M" (sort of the opposite of this function).
##'
##' @param data The data.table object containing the values.
##' @param value The value of the observation.
##' @param flag The flag of the observation.
##' @param naFlag The value of the flag which denotes missing value.
##'
##' @export
##' 

updateMissingFlags = function(data, value, flag, naFlag = "M"){
    missingIndex = which(is.na(data[[value]]))
    invisible(data[missingIndex, `:=`(c(flag), list(naFlag))])
}
