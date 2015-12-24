##' FCL to CPC Codes
##' 
##' This function maps FCL to CPC codes using the table defined within the SWS.
##' 
##' @param fclCodes A character vector containing the FCL codes.
##'   
##' @return A character vector whose ith element is the CPC code corresponding 
##'   to the passed FCL code in position i.  If no valid mapping is found, an NA
##'   is returned in this place.
##' 
##' @examples
##' \dontrun{
##' fcl2cpc(c("0015", "0016", "0265"))
##' fcl2cpc(cpc2fcl(c("0111", "23110", "01447")))
##' }
##' 
##' @export
##' 

fcl2cpc = function(fclCodes){
    ## Data Quality Checks
    stopifnot(is(fclCodes, "character"))
    if(!exists("swsContext.datasets"))
        stop("No swsContext.datasets object defined.  Thus, you probably ",
             "won't be able to read from the SWS and so this function won't ",
             "work.")
    codeLength = sapply(fclCodes, nchar)
    if(any(codeLength != 4))
        stop("All FCL codes must be 4 characters long!  You probably need to ",
             "pad your current codes with zeroes.")
    
    ## Load the mapping table
    map = faosws::ReadDatatable(table = "fcl_2_cpc")
    
    ## Merge the fclCodes with the mapping table
    out = merge(data.table(fcl = unique(fclCodes)), map, by = "fcl",
                all.x = TRUE)
    ## Set the key to FCL so we can sort by passing in the vector
    setkeyv(out, "fcl")
    out[fclCodes, cpc, allow.cartesian = TRUE]
}