##' This function reads the complete imputation key from the Datatable
##' 'fbs_[table]_comm_codes'
##'
##' @param table The Datatable to read from.
##' @return A DatasetKey object containing all the keys for production
##'     imputation
##'
##' @export

getCompleteImputationKey = function(table = "production"){
    supportedDomain = c("production", "trade", "seed", "loss", "industrial",
                        "food", "feed", "stocks", "tourist", "sua")
    if(!table %in% supportedDomain)
        stop("'domain' specified is not supported, please select another domain",
             " or contact the Engineering team to set up a new table",
             " supported domains are: \n\t",
             paste0(supportedDomain, collapse = "\n\t"))

    ## Read the table
    tableName = paste0("fbs_", table, "_comm_codes")
    completeImputationCodes = ReadDatatable(tableName)
    if(nrow(completeImputationCodes) == 0)
        stop("The reference data contains no entry")

    ## Extract the information
    domain = unique(completeImputationCodes$fbs_domain)
    dataset = unique(completeImputationCodes$fbs_dataset)
    datasetConfig = GetDatasetConfig(domainCode = domain,
                                     datasetCode = dataset)

    n.dimensions = length(datasetConfig$dimensions)
    dimensions = vector("list", n.dimensions)
    names(dimensions) = datasetConfig$dimensions
    for(i in seq(n.dimensions)){
        dimension_name = datasetConfig$dimensions[[i]]
        dimensions[[i]] =
            Dimension(name = dimension_name,
                       keys = completeImputationCodes[fbs_key == dimension_name, fbs_code])
    }
    ## Build the key
    completeImputationKey =
        DatasetKey(domain = domain,
                   dataset = dataset,
                   dimensions = dimensions)
    completeImputationKey
}
