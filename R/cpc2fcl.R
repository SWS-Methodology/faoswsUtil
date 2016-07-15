##' CPC to FCL Codes
##' 
##' This function maps CPC to FCL codes using the table defined within the SWS.
##' 
##' @param cpcCodes A character vector containing the CPC codes.
##' @param returnFirst Logical.  If one CPC code maps to multiple FCL codes, we 
##'   have no way of correctly mapping a single code without more information. 
##'   The code is designed to throw an error at this point, but if you want to 
##'   force a result (and return the lowest numerical code of all the matches) 
##'   then set this to TRUE.  In that case, only a warning will be issued if
##'   this type of mapping occurs.  In general, setting this to TRUE is bad
##'   practice.  However, the mapping at the time of this writing is entirely
##'   one-to-one except for maize, which is mapped to maize, white maize, and
##'   popcorn.  It's probably not very bad to ignore this issue.
##'  @param version character. This specifies the version of the conversion 
##'   tables between fcl and cpc. The only ones available, so far, are '2.1preliminary',
##'   the default one, and '2.1' which is the update and official one. Ideally in 
##'   the future the default value should be '2.1'.
##'   
##' @return A character vector whose ith element is the FCL code corresponding
##'   to the passed CPC code in position i.  If no valid mapping is found, an NA
##'   is returned in this place.
##' 
##' @examples
##' \dontrun{
##' cpc2fcl(c("0111", "23110", "01447"))
##' cpc2fcl(fcl2cpc(c("0015", "0016", "0265")))
##' }
##' 
##' @export
##' 

cpc2fcl = function(cpcCodes, returnFirst = FALSE, version = "2.1preliminary"){
    ## Data Quality Checks
    stopifnot(is(cpcCodes, "character"))
    if(!exists("swsContext.datasets")){
        stop("No swsContext.datasets object defined.  Thus, you probably ",
             "won't be able to read from the SWS and so this function won't ",
             "work.")
    }
    
    ## Load the mapping table
    if (version == "2.1preliminary") {
        map = faosws::ReadDatatable(table = "fcl_2_cpc")
    } else if (version == "2.1") {
        map = faosws::ReadDatatable(table = "fcl2cpc_ver_2_1")
    } else {
        stop("Version of the conversion fcl_2_cpc table not available")
    }
    
    ## Merge the fclCodes with the mapping table Note: allow.cartesian = TRUE
    ## because cpc 0112 maps to FCL 56, 67, and 68.  This is handled later, but
    ## it can throw an error here...
    out = merge(data.table(cpc = unique(cpcCodes)), map, by = "cpc",
                all.x = TRUE, allow.cartesian = TRUE)
    ## Set the key to FCL so we can sort by passing in the vector
    setkeyv(out, "cpc")
    result = out[cpcCodes, fcl, allow.cartesian = TRUE]
    
    ## The mapping isn't a 1-1 relationship.  If you didn't hit any of the non
    ## 1-1 maps, just return the data.
    if(length(result) == length(cpcCodes)){
        return(result)
    ## If a value fails to map, it should be returned with an NA.  Thus, an
    ## error should be raised if this doesn't happen.
    } else if(length(result) < length(cpcCodes)){
        stop("Something strange happened.  Unmatched codes should be returned ",
             "as NAs, not omitted completely...")
    ## If we do have an issue with a 1 to many mapping, then either:
    ## - ignore it by replacing the 1 to many with a 1-1 using the first code.
    ## - return an error.
    ## Do the first case if returnFirst is TRUE (defaults to FALSE, as this is
    ## not good practice).
    } else if(length(result) > length(cpcCodes)){
        if(returnFirst){
            warning("At least one CPC code maps to multiple FCL codes.  No ",
                 "good solution is available, but the code is using first ",
                 "numeric code to map to.  At the time of the writing of the ",
                 "code, however, the only issue was that maize is mapped to ",
                 "maize, white maize, and popcorn.  Mapping maize to maize ",
                 "and ignoring white maize/popcorn probably isn't too bad.")
            map = map[, .SD[which.min(fcl)], by = cpc]
            map = unique(map)
            ## Merge the fclCodes with the mapping table
            out = merge(data.table(cpc = unique(cpcCodes)), map, by = "cpc",
                        all.x = TRUE)
            ## Set the key to FCL so we can sort by passing in the vector
            setkeyv(out, "cpc")
            return(out[cpcCodes, fcl, allow.cartesian = TRUE])
        } else {
            stop("At least one CPC code maps to multiple FCL codes.  No easy ",
                 "mapping is available, you may have to map manually.")
        }
    }
}