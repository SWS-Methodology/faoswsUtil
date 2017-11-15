##' Get Yield Codes
##' 
##' This function takes a vector of CPC codes and returns a data.table with the 
##' corresponding production/yield/area harvested codes for those commodities.
##' 
##' @param itemCode A character vector of CPC codes.
##'   
##' @return A data.table with the passed CPC codes as well as the corresponding
##'   element codes for the production, yield, and area harvested elements.  In
##'   addition, "factor" is returned, and this satisfies the formula
##'   
##' Yield = Production / (Area Harvested) * factor
##'   
##' @export
##' 

getYieldCodes = function(itemCode){
    
    itemData = GetCodeList(domain = "agriculture", dataset = "aproduction",
                           dimension = "measuredItemCPC", codes = itemCode)
    if(any(!itemCode %in% itemData[, code])){
        missingCodes = itemCode[!itemCode %in% itemData[, code]] 
        if(length(missingCodes) > 10){
            missingCodes = missingCodes[1:10]
        }
        warning(paste0("Not all itemCodes are in the database!  Example(s):\n",
                paste(missingCodes, collapse = "\n")))
    }
    codeMap = faosws::ReadDatatable(table = "item_type_yield_elements")
    setnames(itemData, "type", "item_type")
    out = merge(itemData, codeMap, by = "item_type")
    out = out[, c("code", "element_31", "element_41", "element_51", "factor"),
              with = FALSE]
    setnames(out, c("element_31", "element_41", "element_51"),
             c("area", "yield", "production"))
    out
}