##' FCL to CPC Codes
##' 
##' This function maps FCL to CPC codes using the table defined within the SWS.
##' 
##' @param fclCodes A character vector containing the FCL codes.
##' @param version character. This specifies the version of the conversion 
##'   tables between fcl and cpc. The only ones available, so far, are 
##'   '2.1preliminary', the default one, and '2.1' which is the update and 
##'   official one. Ideally in the future the default value should be '2.1'.
##' @param version character. This specifies the version of the conversion 
##'   tables between fcl and cpc. To use the latest version, you can use
##'   'latest'. The other tables available are '2.1preliminary', the default one
##'   and '2.1' which is the update and official one.
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

fcl2cpc = function(fclCodes, version = "2.1preliminary"){
    ## Data Quality Checks
    
    validversions = c("latest", "2.1preliminary", "2.1")
    latestversion = "2.1"
    stopifnot(version %in% validversions)
    if(version == "latest"){
        version = latestversion
    }
    
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
    
    map = faosws::ReadDatatable(table = switch(version,
        "2.1preliminary" = "fcl_2_cpc",
                   "2.1" = "fcl2cpc_ver_2_1"
    )) 

    
    ## Merge the fclCodes with the mapping table
    out = merge(data.table(fcl = unique(fclCodes)), map, by = "fcl",
                all.x = TRUE)
    ## Set the key to FCL so we can sort by passing in the vector
    setkeyv(out, "fcl")
    out[fclCodes, cpc, allow.cartesian = TRUE]
}