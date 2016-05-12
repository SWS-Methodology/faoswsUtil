##' This function removes countries that has only one observation
##'
##' TODO (Michael): The by key (param$byKey) should be extended, the
##'                 criteria should be 1 observation per
##'                 country/item/element combination.
##'
##' NOTE (Michael): The application of the function is questionable. I
##'                 would impute the series even if there is only one
##'                 dataset. It is the best given the information set,
##'                 and we should aim to improve the data collection.
##'
##' @param data The data.table object containing the data.
##'
##' @return A data.table where countries with only a single
##'     observation are omitted from the original data.
##' @export
##'

removeSingleEntryCountry = function(data){
    ## NOTE(Michael): This function makes imputation of
    ##                all data not possible, as country
    ##                with only one observation will not
    ##                be imputed.
    dataCopy = copy(data)
    dataCopy[, countryCnt := .N, by = "geographicAreaM49"]
    dataCopy = dataCopy[countryCnt > 1, ]
    dataCopy[, countryCnt := NULL]
    dataCopy
}
