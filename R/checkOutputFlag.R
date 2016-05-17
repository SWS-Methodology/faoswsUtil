##' This is a function to check whether the flags in the data confine
##' to expectation.
##'
##' @param data The data.table object to be checked
##' @param flagObservationStatusVar The column name corresponding
##'     to the observation status flag.
##' @param flagObservationStatusExpected The value of the observation
##'     status flag expected in the output.
##' @param flagMethodVar The column name corresponding to the
##'     method flag.
##' @param flagMethodExpected The value of the method flag expected in
##'     the output.
##' @param returnData logical, whether the data should be returned
##' @param normalised logical, whether the data is normalised
##' @param denormalisedKey optional, only required if the input data is not
##' @return The original data is returned if all the flag matches
##'     those expected, otherwise an error is raised.
##'
##' @export
##'

checkOutputFlags = function(data,
                            flagObservationVar = "flagObservationStatus",
                            flagObservationExpected,
                            flagMethodVar = "flagMethod",
                            flagMethodExpected,
                            returnData = TRUE,
                            normalised = TRUE,
                            denormalisedKey = "measuredElement"){
    dataCopy = copy(data)

    if(!normalised){
        dataCopy = normalise(dataCopy)
    }

    if(!all(c(flagObservationVar, flagMethodVar) %in% colnames(dataCopy)))
        stop("Flag columns are not in the data")

    if(!all(data[[flagObservationVar]] %in%
            flagObservationExpected))
        stop("Incorrect Observation Flag")
    if(!all(data[[flagMethodVar]] %in% flagMethodExpected))
        stop("Incorrect Method Flag")

    if(!normalised){
        dataCopy = denormalise(dataCopy, denormalisedKey)
    }

    if(returnData)
        return(dataCopy)

}
