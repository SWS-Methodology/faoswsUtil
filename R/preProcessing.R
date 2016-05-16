##' This function performs manipulation of the data that are standard
##' after the data is retrieved from the data base.
##'
##' NOTE (Michael): The data is assumed to be normalised.
##'
##' @param data The data.table object
##' @param normalised logical, whether the data is normalised
##' @param denormalisedKey optional, only required if the input data is not
##'     normalised.It is the name of the key that denormalises the data.
##'
##' @return A data.table with standard pre processing steps
##'     performed.
##'
##' @export
##'

preProcessing = function(data,
                         normalised = TRUE,
                         denormalisedKey = "measuredElement"){

    dataCopy = copy(dataToBeSaved)

    if(!normalised){
        dataCopy = normalise(dataCopy)
    }

    if(!all(c("timePointYears", "Value", "flagObservationStatus") %in%
            colnames(dataCopy)))
        stop("Required column not in data, this function assumes the data is ",
             "normalised")

    ## Converting year to numeric for modelling
    dataCopy[, `:=`(c("timePointYears"), as.numeric(.SD[["timePointYears"]]))]

    dataWithout0M =
        remove0M(dataCopy, valueVars = "Value", flagVars = "flagObservationStatus")

    if(any(is.na(dataWithout0M$flagObservationStatus))){
        stop("There are non-existing records, please fill them with the ",
             "fillRecord() function")
    }

    if(!normalised){
        dataWithout0M = denormalise(dataWithout0M, denormalisedKey)
    }
    dataWithout0M
}
