##' This function denormalise data in the SWS format
##'
##' @param normalisedData normalised data to be denormalise
##' @param denormaliseKey The key/variable that will be denormalised.
##' @param areaVar The column name corresponding to the geographic area.
##' @param itemVar The column name corresponding to the commodity item.
##' @param elementVar The column name corresponds to the measured element.
##' @param yearVar The column name corresponds to the time dimension.
##' @param flagObsVar The column name corresponds to the observation status
##'     flag.
##' @param flagMethodVar The column name corresponds to the method flag.
##' @param valueVar The column name corresponds to the value columne.
##' @param fillEmptyRecords logical, whether empty record created due to
##'     denormalisation should be replaced with missing value.
##'
##' @return The denormalised data
##'
##' @export
##'



denormalise = function(normalisedData,
                       denormaliseKey,
                       areaVar = "geographicAreaM49",
                       itemVar = "measuredItemCPC",
                       elementVar = "measuredElement",
                       yearVar = "timePointYears",
                       flagObsVar = "flagObservationStatus",
                       flagMethodVar = "flagMethod",
                       valueVar = "Value",
                       fillEmptyRecords = FALSE){

    allKey = c(areaVar, itemVar, elementVar, yearVar)
    measuredTriplet = c(valueVar, flagObsVar, flagMethodVar)
    normaliseKey = setdiff(allKey, denormaliseKey)

    denormaliseFormula =
        as.formula(paste0(paste0(normaliseKey, collapse = " + "), " ~ ",
                          denormaliseKey))
    seperator = paste0("_", denormaliseKey, "_")


    denormalised = dcast(normalisedData, formula = denormaliseFormula,
                         value.var = measuredTriplet,
                         sep = seperator)

    if(fillEmptyRecords){
        denormalised = fillRecord(denormalised)
    }

    uniqueElementCodes =
        unique(gsub("[^0-9]", "",
                    grep("[0-9]{4}", colnames(denormalised),
                         value = TRUE)))

    setcolorder(x = denormalised,
                neworder = c("geographicAreaM49",
                             "measuredItemCPC",
                             "timePointYears",
                             sapply(uniqueElementCodes,
                                    FUN = function(x){
                                 grep(paste0(x, "$"),
                                      colnames(denormalised),
                                      value = TRUE)
                             }
                             )))
    denormalised
}
