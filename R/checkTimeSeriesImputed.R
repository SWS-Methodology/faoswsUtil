##' This function checks whether all time series within a dataset are imputed.
##'
##' @param dataToBeSaved The data to be saved back to the database
##' @param key The key which splits the data into individual timeseries.
##'     Generally the set c('geographicAreaM49', 'measuredItemCPC',
##'     'measuredElement').
##' @param valueColumn The column which contains the numeric values.
##' @param returnData logical, whether the data should be returned
##' @param normalised logical, whether the data is normalised
##' @param denormalisedKey optional, only required if the input data is not
##'     normalised.It is the name of the key that denormalises the data.
##' @return The same data if all time series are imputed, otherwise an error.
##'
##' @export
##'

checkTimeSeriesImputed = function(dataToBeSaved,
                                  key,
                                  valueColumn,
                                  returnData = TRUE,
                                  normalised = TRUE,
                                  denormalisedKey = "measuredElement"){
    ## The number of missing values should be either zero or all
    ## missing.
    dataCopy = copy(dataToBeSaved)

    if(!normalised){
        dataCopy = normalise(dataCopy)
    }

    if(!all(key %in% colnames(dataCopy)))
        stop("Required column not in the dataset")

    dataRemoved0M =
        remove0M(dataCopy, valueVars = valueColumn,
                 flagVars = "flagObservationStatus")
    check = dataRemoved0M[, sum(is.na(.SD[[valueColumn]])) == 0 |
                            sum(is.na(.SD[[valueColumn]])) == .N,
                          by = c(key)]
    unimputedTimeSeries = which(!check$V1)
    if(length(unimputedTimeSeries) > 0){
        stop("Not all time series are imputed")
    }
    if(!normalised){
        dataCopy = denormalise(dataCopy, denormalisedKey)
    }

    if(returnData)
        return(dataCopy)
}
