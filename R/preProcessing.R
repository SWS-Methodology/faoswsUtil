##' This function performs manipulation of the data that are standard
##' after the data is retrieved from the data base.
##'
##' NOTE (Michael): The data is assumed to be normalised.
##'
##' @param data The data.table object
##'
##' @return A data.table with standard pre processing steps
##'     performed.
##'
##' @export
##'

preProcessing = function(data){
    if(!all("timePointYears", "Value", "flagObservationStatus") %in% colnames(data))
        stop("Required column not in data, this function assumes the data is ",
             "normalised")

    dataCopy = copy(data)
    ## Converting year to numeric for modelling
    dataCopy[, `:=`(c("timePointYears"), as.numeric(.SD[["timePointYears"]]))]

    dataWithout0M =
        remove0M(dataCopy, valueVars = "Value", flagVars = "flagObservationStatus")

    if(any(is.na(dataWithout0M$flagObservationStatus))){
        stop("There are non-existing records, please fill them with the ",
             "fillRecord() function")
    } else {
        return(dataWithout0M)
    }
}
