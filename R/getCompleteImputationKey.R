##' This function reads the complete imputation key from the Datatable
##' 'fbs_[domain]_comm_codes'
##'
##' @param domain The domain to read from.
##' @return A DatasetKey object containing all the keys for production
##'     imputation
##'
##' @export

getCompleteImputationKey = function(domain = "production"){
    supportedDomain = c("production", "trade", "seed", "loss", "industrial",
                        "food", "feed", "stocks", "tourist", "sua")
    if(!domain %in% supportedDomain)
        stop("'domain' specified is no supported, please select another domain",
             " or contact the Engineering team to set up a new table",
             " supported domains are: \n\t",
             paste0(supportedDomain, collapse = "\n\t"))

    tableName = paste0("fbs_", domain, "_comm_codes")
    completeImputationCodes = ReadDatatable(tableName)
    allCountryCodes =
        completeImputationCodes[fbs_key == "geographicAreaM49", fbs_code]
    allItemCodes =
        completeImputationCodes[fbs_key == "measuredItemCPC", fbs_code]
    allElementCodes =
        completeImputationCodes[fbs_key == "measuredElement", fbs_code]
    allYearCodes =
        completeImputationCodes[fbs_key == "timePointYears", fbs_code]
    completeImputationKey =
        DatasetKey(domain = "agriculture",
                   dataset = "aproduction",
                   dimensions =
                       list(geographicAreaM49 =
                                Dimension(name = "geographicAreaM49",
                                          keys = allCountryCodes),
                            measuredItemCPC =
                                Dimension(name = "measuredItemCPC",
                                          keys = allItemCodes),
                            measuredElement =
                                Dimension(name = "measuredElement",
                                          keys = allElementCodes),
                            timePointYears =
                                Dimension(name = "timePointYears",
                                          keys = allYearCodes)))
    completeImputationKey
}
