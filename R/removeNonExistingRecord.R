##' Function to remove non-existing record
##'
##' Sometimes non-existing records (observation status flag and method flag and
##' value all NA) are returned by the database, or can be created by
##' denormalising the data. This function removes these records.
##'
##' @param data The data containing non-existing record
##' @param areaVar The column name corresponding to the geographic
##'     area.
##' @param itemVar The column name corresponding to the commodity
##'     item.
##' @param elementVar The column name corresponding to the measured
##'     element.
##' @param yearVar The column name corresponding to the year.
##' @param flagObsVar The column name corresponding to the observation
##'     status flag.
##' @param flagMethodVar The column name corresponding to the method
##'     flag.
##' @param valueVar The column name corresponding to the value.
##'
##' @return Data with non-existing records omitted.
##'
##' @export
##'

removeNonExistingRecord = function(data,
                             areaVar = "geographicAreaM49",
                             itemVar = "measuredItemCPC",
                             elementVar = "measuredElement",
                             yearVar = "timePointYears",
                             flagObsVar = "flagObservationStatus",
                             flagMethodVar = "flagMethod",
                             valueVar = "Value"){
    requiredColumn = c(areaVar, itemVar, elementVar, yearVar,
                       flagObsVar, flagMethodVar, valueVar)
    if(!all(requiredColumn %in% colnames(data)))
        stop("Required column not in data, data has to be normalised!")

    data[!is.na(data[[flagObsVar]]) & !is.na(data[[flagMethodVar]]), ]
}
