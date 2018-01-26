##' This function obtains the share data.
##'
##' @param geographicAreaM49 A character vector of area codes. The trees
##'     returned are specific to country and year; thus, providing this
##'     parameter limits which trees are pulled. If NULL, all are used.
##' @param timePointYearsSP A character vector of years. See geographicAreaM49.
##' @param scaleShare whether the share should be scaled from [0, 100] to [0,
##'     1].
##' @param measuredItemParentCPC Parent commodity.
##' @param measuredItemChildCPC Child Commodity.
##' @return A table of the share
##' @export
##'

getShareData = function(geographicAreaM49,
                        measuredItemChildCPC,
                        measuredItemParentCPC,
                        timePointYearsSP,
                        scaleShare = TRUE){
    ## Need to double check this.
    elementKey = "1"

    ## Build the query key for specific shares
    specificShareKey =
        DatasetKey(
            domain = "agriculture",
            dataset = "aupus_share",
            dimensions =
                list(
                    geographicAreaM49 = Dimension(name = "geographicAreaM49",
                                                  keys = geographicAreaM49),
                    measuredElement = Dimension(name = "measuredShare",
                                                keys = elementKey),
                    measuredItemChildCPC = Dimension(name = "measuredItemChildCPC",
                                                     keys = measuredItemChildCPC),
                    measuredItemParentCPC = Dimension(name = "measuredItemParentCPC",
                                                      key = measuredItemParentCPC),
                    timePointYearsSP = Dimension(name = "timePointYearsSP",
                                                 keys = timePointYearsSP)
                ))
    specificShareData = GetData(specificShareKey)
    specificShareData[, c("measuredShare") := NULL]
    specificShareData[, `:=`(c("flagShare"), as.character(flagShare))]

    ## Extract the wild card shares (wild card is only for years)
    wildCardShareKey = specificShareKey
    wildCardShareKey@dimensions$timePointYearsSP@keys = "0"
    wildCardShareData = GetData(wildCardShareKey)
    wildCardShareData[, c("measuredShare", "timePointYearsSP") := NULL]
    wildCardShareData[, `:=`(c("flagShare"), as.character(flagShare))]
    setnames(wildCardShareData,
             old = c("Value", "flagShare"),
             new = c("year_wild_value", "year_wild_flagShare"))

    ## Create the complete expanded key table
    expandedKey =
        data.table(expand.grid(geographicAreaM49 = geographicAreaM49,
                               measuredItemChildCPC = measuredItemChildCPC,
                               measuredItemParentCPC = measuredItemParentCPC,
                               timePointYearsSP = timePointYearsSP,
                               stringsAsFactors = FALSE,
                               KEEP.OUT.ATTRS = FALSE))

    ## Fill the table with specific share
    specificExpanded =
        merge(expandedKey, specificShareData, all.x = TRUE,
              by = intersect(colnames(expandedKey),
                             colnames(specificShareData)))

    ## Fill the table with year wild card share
    completeShareData =
        merge(specificExpanded, wildCardShareData, all.x = TRUE,
              by = intersect(colnames(specificExpanded),
                             colnames(wildCardShareData)))
    completeShareData[is.na(completeShareData$Value),
                      `:=`(c("Value", "flagShare"),
                           list(year_wild_value, year_wild_flagShare))]

    ## Default to 100 percent if missing
    completeShareData[is.na(completeShareData$Value),
                      `:=`(c("Value", "flagShare"),
                           list(100, "create by function"))]
    if(scaleShare)
        completeShareData[, `:=`("Value", Value/100)]

    ## Remove the column
    completeShareData[, `:=`(c("year_wild_value", "year_wild_flagShare"), NULL)]
    completeShareData
}
