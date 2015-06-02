##' FS to M49 Codes
##' 
##' This function maps FS to M49 (country) codes using the table defined within
##' the SWS.
##' 
##' @param fsCodes A character vector containing the FS codes.
##'   
##' @return A character vector whose ith element is the M49 code corresponding 
##'   to the passed FS code in position i.  If no valid mapping is found, an NA
##'   is returned in this place.
##'   
##' @examples
##' \dontrun{
##' fs2m49(c("1", "2", "3", "4"))
##' m492fs(fs2m49(c("1", "2", "3", "4")))
##' }
##' 
##' @export
##' 

fs2m49 = function(fsCodes){
    ## Data Quality Checks
    stopifnot(is(fsCodes, "character"))
    if(!exists("swsContext.datasets")){
        stop("No swsContext.datasets object defined.  Thus, you probably ",
             "won't be able to read from the SWS and so this function won't ",
             "work.")
    }
    
    areaMapping = faosws::GetTableData(schemaName = "ess",
                                       tableName = "fal_2_m49")
    ## Check that areaMapping is still 1 to 1
    if(length(unique(areaMapping$fal)) < nrow(areaMapping))
        stop("ess.fal_2_m49 (on the SWS) is no longer a one-to-one map and ",
             "the R code needs to be updated to reflect that!")
    
    ## Merge the fsCodes with the mapping table
    out = merge(data.table(fal = unique(fsCodes)), areaMapping, by = "fal",
                all.x = TRUE)
    setkeyv(out, "fal")
    result = out[fsCodes, m49, allow.cartesian = TRUE]
    result
}