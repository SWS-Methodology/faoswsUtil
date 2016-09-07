##' Name SWS dataset
##'
##' Adds human-readable names to keys automatically.
##' 
##' @param domain character. Domain of dataset
##' @param dataset character. Name of dataset
##' @param dt data.table. The data.table to which you wish to add names
##' @param except character. Vector of names in the table that you do not wish 
##'   named. For example, "timePointYears"
##' @param append character. Suffix to add to column name of names. Not applied 
##'   if \code{append = FALSE}
##' @param returnCodes logical. If TRUE, descriptions are returned as columns 
##'   after the codes. If FALSE, the codes are omitted from the output.
##'   
##'  @examples
##'  \dontrun{
##'  dat = structure(list(geographicAreaM49 = c("276", "36", "380", "4",
##' "716"), Value = c(0, 0.0555555555555556, 0, 0, 0.2)), row.names = c(NA,
##'  -5L), class = c("data.table", "data.frame"), .Names = c("geographicAreaM49",                                                                                                                         
##'  "Value"), sorted = "geographicAreaM49")
##'  
##'  nameData("agriculture", "aproduction", dat, except = "timePointYears")
##'  }
##'  
##' @export

nameData <- function(domain, dataset, dt, except, append = "_description", returnCodes = TRUE){
    
    stopifnot(is.character(domain), is.character(dataset))
    stopifnot(is.character(append), nchar(append) < 1)
    
    conf <- GetDatasetConfig(domain, dataset)
    ##Which keys are actually dimensions
    keys <- intersect(names(dt), conf[["dimensions"]])
    
    ##Except this key
    if(missing(except)) except <- character()
    keys <- setdiff(keys, except)
    
    # If there are no shared keys, just return the data
    if(length(keys) < 1){
        return(dt)
    }
    
    codes2merge <- lapply(keys, GetCodeDescription, domain = domain, 
                          dataset = dataset, data = dt, append = append)
    newdata <- copy(dt)
    for(i in seq_along(codes2merge)){
        newdata_names <- names(newdata)
        shared_name <- intersect(newdata_names, names(codes2merge[[i]]))
        stopifnot(length(shared_name) == 1)
        newdata <- merge(newdata, codes2merge[[i]], by = shared_name, all.x = TRUE)
        
        setcolorder(newdata,
                    append(newdata_names, paste0(shared_name,
                                                 append),
                           after = which(newdata_names == shared_name)
                    )
        )
        
        if(!returnCodes){
            newdata[, (shared_name) := NULL]
            setnames(newdata, paste0(shared_name, append), shared_name)
        } 
        
    }
    newdata
}

GetCodeDescription <- function(domain, dataset, key, data, append = "_description"){
    codeTable <- GetCodeList(domain, dataset, key, data[, get(key)])[,.(code, description)]
    setnames(codeTable, c("code", "description"), c(key, paste0(key, append)))
    codeTable
}
