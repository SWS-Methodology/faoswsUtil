#' Get all data from a table in the Statistical Working System
#' 
#' In some rare cases, you may want to fetch all data from a Statistical Working
#' System table. This function uses the code list from the dataset and makes a 
#' key will all codes from all dimensions. It then gets that data. For obvious 
#' reasons, this can take some time. The domain and dataset name can be
#' retrieved from the \code{swsContext.datasets} or you can use the
#' \code{\link{GetDomainNames}} family of functions.
#' 
#' @param domain character. Name of the domain
#' @param dataset character. Name of the dataset
#' 
#' @export

getAllData <- function(domain, dataset){
    
    config <- GetDatasetConfig(domain, dataset)
    
    dims <- lapply(config$dimensions, function(x) Dimension(name = x, keys = GetCodeList(domain, dataset, x)[, code]))
    
    dsk <- DatasetKey(domain = domain, 
                      dataset = dataset,
                      dimensions = dims)
    
    GetData(dsk)[]
}