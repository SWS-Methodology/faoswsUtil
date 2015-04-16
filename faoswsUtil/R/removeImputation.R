##' Function to remove prior imputation.
##'
##' @param data The data.table object containing the values.
##' @param value The column name of data which contains the values for the
##' variable to be modified.
##' @param observationFlag The column name of the observation flag
##' corresponding to value.
##' @param methodFlag The column name of the method flag corresponding to
##' value.
##' @param missingObservationFlag The character value which is used to
##' represent missing values in the observation flag.  This value will be put
##' in any column containing imputation data that is removed.
##' @param missingMethodFlag The character value which is used to represent
##' missing values in the method flag.  This value will be put in any column
##' containing imputation data that is removed.
##' @param imputedFlag This character value specifies which observation flags
##' correspond to imputed values in the data.
##' 
##' @details Observations which have observationFlag == imputedFlag will be
##' modified to missing.  Thus, the value will be updated to NA, the
##' observation flag will be updated to missingObservationFlag, and the method
##' flag will be updated to missingMethodFlag.
##' 
##' @return No value is returned.  However, the object "data" which was passed
##' to this function is modified.
##'
##' @export
##' 

removeImputation = function(data, value, observationFlag, methodFlag,
                            missingObservationFlag = "M",
                            missingMethodFlag = "u",
                            imputedFlag = "I"){
    
    ### Data Quality Checks
    stopifnot(is(data, "data.table"))
    stopifnot(c(value, observationFlag, methodFlag) %in% colnames(data))
    
    imputedIndex = data[[observationFlag]] %in% imputedFlag
    invisible(data[imputedIndex, `:=`(c(value, observationFlag, methodFlag),
                                      list(NA, missingObservationFlag,
                                           missingMethodFlag))])
}
