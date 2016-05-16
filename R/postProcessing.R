##' This function performs manipulation of the data that are standard
##' before the data is saved back to the data base.
##'
##' NOTE (Michael): The data is assumed to be normalised.
##'
##' @param data The data.table object
##' @param normalised logical, whether the data is normalised
##' @param denormalisedKey optional, only required if the input data is not
##'     normalised.It is the name of the key that denormalises the data.
##' @return A data.table with standard post processing steps
##'     performed.
##'
##' @export
##'

postProcessing = function(data,
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

    ## Converting year back to database
    dataCopy[, `:=`("timePointYears", as.character(.SD[["timePointYears"]]))]
    ## Restoring the 0M values
    dataWith0M =
        restore0M(dataCopy, valueVars = "Value",
                  flagVars = "flagObservationStatus")

    if(!normalised){
        dataWith0M = denormalise(dataWith0M, denormalisedKey)
    }

    dataWith0M
}
