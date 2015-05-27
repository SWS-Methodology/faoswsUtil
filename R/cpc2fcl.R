##' CPC to FCL Codes
##' 
##' This function maps CPC to FCL codes using the table defined within the SWS.
##' 
##' @param cpcCodes A character vector containing the CPC codes.
##'   
##' @return A character vector whose ith element is the FCL code corresponding
##'   to the passed CPC code in position i.  If no valid mapping is found, an NA
##'   is returned in this place.
##' 
##' @examples
##' \dontrun{
##' cpc2fcl("0111", "23110", "01447")
##' cpc2fcl(fcl2cpc(c("0015", "0016", "0265")))
##' }
##' 
##' @export
##' 

cpc2fcl = function(cpcCodes){
    ## Data Quality Checks
    stopifnot(is(cpcCodes, "character"))
    if(!exists("swsContext.datasets"))
        stop("No swsContext.datasets object defined.  Thus, you probably ",
             "won't be able to read from the SWS and so this function won't ",
             "work.")
    
    ## Load the mapping table
    map = faosws::GetTableData(schemaName = "ess", tableName = "fcl_2_cpc")
    
    ## Merge the fclCodes with the mapping table
    out = merge(data.table(cpc = cpcCodes), map, by = "cpc", all.x = TRUE)
    ## Set the key to FCL so we can sort by passing in the vector
    setkeyv(out, "cpc")
    out[cpcCodes, fcl]
}