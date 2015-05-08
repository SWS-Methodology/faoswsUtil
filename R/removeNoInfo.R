##' This function removes data with no information
##'
##' The information contained within a set of index is calculated
##' based on whether a non-zero value exist. If the data contains only
##' zero and missing value then it is considered to contain no
##' imformation for imputation.
##'
##' @param data A data.table object containing the data of interest.
##' @param value The column name of the value of the variable.
##' @param observationFlag The column name of the observation flag
##' corresponding to value.
##' @param byKey The column name of the grouping variable.  If no data exists
##' for an entire group, then that data is removed from the data.table.
##' @param environment The functions in this package generally work by
##' passing the data.table by reference and modifying it in place.  In place
##' row deletion is currently not supported by data.table, see
##' https://github.com/Rdatatable/data.table/issues/635.  However, upon
##' resolution of this issue, this function should be updated. However, until
##' then, to make this function consistent with the other functions in this
##' package, environment is passed to specify where the modified data.table
##' should be assigned.  The default of parent.frame(1) gives the calling
##' environment and this should be satisfactory for most, if not all, use
##' cases.
##'
##' @return No value is returned.  However, the object "data" which was passed
##' to this function is modified.  In fact, it's actually overwritten if 
##' environment = parent.frame(1), the default.
##'
##' @export
##' 

removeNoInfo = function (data, value, observationFlag, byKey,
    environment = parent.frame(1)){
    
    ### Data Quality Checks
    stopifnot(c(value, observationFlag) %in% colnames(data))
    stopifnot(is(environment, "environment"))
    
    info = data[, rep(containInfo(value = get(value),
                                  observationFlag = get(observationFlag)),
        NROW(.SD)), by = c(byKey)]$V1
    
    #Assign the new data.table to environment
    dataTableName = as.character(match.call()$data)
    assign(x = dataTableName, value = data[info,],
        envir = environment)
}
