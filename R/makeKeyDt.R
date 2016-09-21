#' Convert DatasetKey to data.table
#' 
#' Keys themselves can be hard to work with and share. This function converts a 
#' DatasetKey to a table format to make it easier to work with.
#' 
#' @param dskey DatasetKey. An object that you've either created yourself or
#'   retrieved from the swsContext.datasets[[1]] object
#' @author Sebastian Campbell <sebastian.campbell@@fao.org>
#' @export makeKeyDt

makeKeyDt <- function(dskey){
    
    stopifnot(inherits(dskey, "DatasetKey"))
    
    keylist <- makeKeylist(dskey)
    
    with(keylist, data.table(domain = domain,
                             dataset = dataset,
                             key = dimensions[["name"]],
                             code = dimensions[["keys"]])
    )
    
}

makeKeylist <- function(dskey){
    
    dimlist <- lapply(dskey@dimensions, Dimension2Dt)
    dims <- data.table::rbindlist(dimlist)
    
    list(domain = dskey@domain,
         dataset = dskey@dataset,
         dimensions = dims,
         sessionId = dskey@sessionId)
}

Dimension2Dt <- function(dim){
    data.table(name = dim@name, keys = dim@keys)
}
