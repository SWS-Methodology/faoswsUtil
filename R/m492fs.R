##' M49 to FS Codes
##' 
##' This function maps M49 to FS (country) codes using the table defined within
##' the SWS.
##' 
##' @param m49Codes A character vector containing the M49 codes.
##'   
##' @return A character vector whose ith element is the FS code corresponding 
##'   to the passed M49 code in position i.  If no valid mapping is found, an NA
##'   is returned in this place.
##'   
##' @examples
##' \dontrun{
##' m492fs(c("1", "2", "3", "4"))
##' m492fs(fs2m49(c("1", "2", "3", "4")))
##' }
##' 
##' @export
##' 

m492fs = function(m49Codes){
    ## Data Quality Checks
    stopifnot(is(m49Codes, "character"))
    if(!exists("swsContext.datasets")){
        stop("No swsContext.datasets object defined.  Thus, you probably ",
             "won't be able to read from the SWS and so this function won't ",
             "work.")
    }
    
    areaMapping = GetTableData(schemaName = "ess", tableName = "fal_2_m49")
    ## Check that areaMapping is still 1 to 1
    if(length(unique(areaMapping$fal)) < nrow(areaMapping) |
       length(unique(areaMapping$m49)) < nrow(areaMapping))
        stop("ess.fal_2_m49 (on the SWS) is no longer a one-to-one map and ",
             "the R code needs to be updated to reflect that!")
    
    ## Merge the m49Codes with the mapping table
    out = merge(data.table(m49 = unique(m49Codes)), areaMapping, by = "m49",
                all.x = TRUE)
    setkeyv(out, "m49")
    result = out[m49Codes, fal, allow.cartesian = TRUE]
    result
}