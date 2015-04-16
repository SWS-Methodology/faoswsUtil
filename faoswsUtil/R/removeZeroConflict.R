##' Remove Zero Conflicts
##' 
##' The function examines two variables of a data.table object.  If one
##' variable is 0 and another is not, then both variables are marked as
##' missing.  This is useful for variables which should always be zero
##' concurrently, such as area harvested and production.
##'
##' @param data The data table object.
##' @param value1 The column name of data corresponding to the first variable.
##' @param value2 The column name of data corresponding to the second variable.
##' @param observationFlag1 The column name of data containing the observation
##' flag for the first variable.
##' @param observationFlag2 The column name of data containing the observation
##' flag for the second variable.
##' @param methodFlag1 The column name of data containing the method flag for
##' the first variable.
##' @param methodFlag2 The column name of data containing the method flag for
##' the second variable.
##' @param missingObservationFlag The flag (character value) which should be
##' placed in the observation flag columns to signify a missing value.
##' @param missingMethodFlag The flag (character value) which should be placed
##' in the method flag columns to signify a missing value.
##'
##' @return No value is returned.  However, the object "data" which was passed
##' to this function is modified (some values are marked as missing if the have
##' conflicting zeroes).
##' 
##' @export
##' 

removeZeroConflict = function(data, value1, value2, observationFlag1,
                              observationFlag2, methodFlag1, methodFlag2,
                              missingObservationFlag = "M",
                              missingMethodFlag = "u"){
    
    ### Data Quality Checks
    stopifnot(is(data, "data.table"))
    cnames = c(value1, value2, observationFlag1, observationFlag2,
               methodFlag1, methodFlag2)
    stopifnot(is(cnames, "character"))
    stopifnot(cnames %in% colnames(data))
    
    ### Identify points where area = 0 and production != 0 (or vice versa)
    filter1 = data[, get(value1) == 0 & get(value2) != 0]
    filter2 = data[, get(value1) != 0 & get(value2) == 0]
    
    ### For problematic observations, set both vals to NA and flags to missing
    data[filter1 | filter2, c(value1, value2,
                              observationFlag1, observationFlag2,
                              methodFlag1, methodFlag2) :=
             as.list(rep(c(NA_real_, missingObservationFlag,
                           missingMethodFlag), each = 2))]
}
