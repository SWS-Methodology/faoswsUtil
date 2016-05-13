##' This function performs manipulation of the data that are standard
##' before the data is saved back to the data base.
##'
##' NOTE (Michael): The data is assumed to be normalised.
##'
##' @param data The data.table object
##'
##' @return A data.table with standard post processing steps
##'     performed.
##'
##' @export
##'

postProcessing = function(data){
    if(!all(c("timePointYears", "Value", "flagObservationStatus") %in%
       colnames(data)))
        stop("Required column not in data, this function assumes the data is ",
             "normalised")
    dataCopy = copy(data)
    ## Converting year back to database
    dataCopy[, `:=`("timePointYears", as.character(.SD[["timePointYears"]]))]
    ## Restoring the 0M values
    dataWith0M =
        restore0M(dataCopy, valueVars = "Value",
                  flagVars = "flagObservationStatus")
    dataWith0M
}
