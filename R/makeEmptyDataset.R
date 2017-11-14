#' Make empty data.table based on SWS table.
#' 
#' @param domain character. SWS domain
#' @param dataset character. SWS dataset
#' 
#' @export makeEmptyDataset

makeEmptyDataset <- function(domain, dataset){
    config <- GetDatasetConfig(domain, dataset)
    # A dataset contains keys, a value and flags
    makeEmptyTable(config$dimensions, config$flags)
}

makeEmptyTable <- function(keys, flags){
    
    stopifnot(length(keys) > 0, length(flags) > 0)
    
    keylist <- makeTypeList(keys, "character")
    valuelist <- makeTypeList("Value", "numeric")
    flaglist <- makeTypeList(flags, "character")
    as.data.table(c(keylist, valuelist, flaglist))
}

makeTypeList <- function(chars, type){
    setNames(replicate(length(chars), get(type)()), 
             chars)
}
