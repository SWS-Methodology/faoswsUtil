##' This function normalises the denormalised data assuming the structure given 
##' by the SWS specification.
##' 
##' @param denormalisedData The data.table or data.frame object which is 
##'   denormalised in the form returned by the \code{GetData} function when the 
##'   normalised argument is set to FALSE.
##' @param areaVar The column name corresponding to the geographic area.
##' @param itemVar The column name corresponding to the commodity item.
##' @param elementVar The column name corresponding to the measured element.
##' @param yearVar The column name corresponding to the year.
##' @param flagObsVar The column name corresponding to the observation status 
##'   flag.
##' @param flagMethodVar The column name corresponding to the method flag.
##' @param valueVar The column name corresponding to the value.
##' @param removeNonExistingRecords logical, whether records where flag 
##'   observation and method are NA should be removed.
##' @return A normalised data.table or data.frame object (depending on the type of
##'   object passed in as denormalisedData) in the same form returned by
##'   the \code{GetData} function when the normalised argument is set to TRUE.
##'   
##' @export
##' 

normalise = function(denormalisedData,
                     areaVar = "geographicAreaM49",
                     itemVar = "measuredItemCPC",
                     elementVar = "measuredElement",
                     yearVar = "timePointYears",
                     flagObsVar = "flagObservationStatus",
                     flagMethodVar = "flagMethod",
                     valueVar = "Value",
                     removeNonExistingRecords = TRUE){

    is_dt <- is.data.table(denormalisedData)
    
    if(!is_dt && !is.data.frame(denormalisedData)){
        stop("denormalisedData must be a data.table or data.frame")
    } else if(!is_dt){
        # If it's a data.frame, make it a data.table for the duration of this
        # function
        denormalisedData <- as.data.table(denormalisedData)
    }
    
    measuredTriplet = c(valueVar, flagObsVar, flagMethodVar)
    allKey = c(areaVar, itemVar, elementVar, yearVar)
    ## denormalisedData = copy(step2Data)
    normalisedKey = intersect(allKey, colnames(denormalisedData))
    denormalisedKey = setdiff(allKey, normalisedKey)
    normalisedList =
        lapply(measuredTriplet,
               FUN = function(x){
                   splitDenormalised =
                       denormalisedData[, c(normalisedKey,
                                            grep(x, colnames(denormalisedData),
                                                 value = TRUE)),
                                        with = FALSE]
                   splitNormalised =
                       melt(splitDenormalised, id.vars = normalisedKey,
                            variable.name = denormalisedKey, value.name = x)
                   substitutePattern = paste0("(", x, "|", denormalisedKey, "|_)")
                   splitNormalised[, `:=`(c(denormalisedKey),
                                          gsub(substitutePattern, "",
                                               .SD[[denormalisedKey]]))]
                   setkeyv(splitNormalised,
                           cols = c(normalisedKey, denormalisedKey))
               })

    normalisedData =
        Reduce(merge, x = normalisedList)
    if(removeNonExistingRecords){
        normalisedData = removeNonExistingRecord(normalisedData)
    }
    if(!is_dt){
        # If it came in as a data.frame, turn it back
        normalisedData <- as.data.frame(denormalisedData)
    }
    
    return(normalisedData[])
}
