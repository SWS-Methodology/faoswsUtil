##' Get FBS Code
##' 
##' This function takes a FCL commodity code and returns the FBS aggregate code 
##' that contains this data.
##' 
##' @param commCodeFCL A vector of commodity codes in the FCL coding format.
##'   
##' @return A vector of FBS codes corresponding to the passed FCL codes. 
##'   Missing codes are returned with a NA.
##'   
##' @export
##' 

getFBSCode = function(commCodeFCL){
    if(is.character(commCodeFCL)){
        commCodeFCL = as.numeric(commCodeFCL)
    }
    
    map = faosws::ReadDatatable(table = "extraction_rates")
    map[, target_code := as.character(target_code)]
    ## If we have an top node (i.e. no parents) with a target_code of NA, our
    ## while loop below will never end.  So, instead, assign some non-FBS code
    ## (like CPC-ID) and these will get filtered out later.
    map[item == parent & is.na(target_code),
        target_code := paste0("CPC", item)]
    setkeyv(map, "item")
    
    #     map[item == 1041, parent := 1035]
    #     map[item == 1042, parent := 1035]
    #     
    result = merge(map, data.table(item = commCodeFCL, index = 1:length(commCodeFCL)), 
                   by="item", all.y=T)
    setkeyv(result, "item")
    result <- result[order(result$index)]
    ## Processed products won't have FBS codes.  So, assign the parent (primary 
    ## products have themselves as parents) to the item, and continue to merge
    ## back with the map until you have all FBS codes.
    while(any(is.na(result$target_code))){
        result[, item := parent]
        result = result[, list(item, index)] # "list" to stay data.table
        setkey(result, item)
        result = merge(map, result, by="item", all.y=T)
        result <- result[order(result$index)]
    }
    
    result[, target_code]
}