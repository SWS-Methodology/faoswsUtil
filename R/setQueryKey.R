##' Set Query Key
##'
##' Set a data key for the query
##'
##' @param dimension character. Name of dimension you wish to set from
##'     DatasetKey
##' @param datasetkey Datasetkey. DatasetKey from which you wish to set
##'   dimensions
##' @param newKey character. Keys you wish to set
##'
##' @export
##'

setQueryKey <- function(dimension,
                        datasetkey = swsContext.datasets[[1]],
                        newKey){
    slot(datasetkey@dimensions[[dimension]], "keys") = newKey
}
