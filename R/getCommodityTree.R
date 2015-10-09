##' Get Commodity Tree
##' 
##' This function pulls the commodity trees in the new CPC system.
##' 
##' @param geographicAreaM49 A character vector of area codes.  The trees 
##'   returned are specific to country and year; thus, providing this parameter 
##'   limits which trees are pulled.  If NULL, all are used.
##' @param timePointYears A character vector of years.  See geographicAreaM49.
##'   
##' @return A data.table object containing the commodity tree.  The dimension 
##'   columns correspond to the country, year, parent, and child commodity.  Two
##'   value columns are available: extraction rate and share.  The logic of the
##'   old system MAY have been that missing extraction rates were assumed to be
##'   1.
##'   

getCommodityTree = function(geographicAreaM49 = NULL, timePointYears = NULL){
    ## Data Quality Checks
    if(!exists("swsContext.datasets")){
        stop("No swsContext.datasets object defined.  Thus, you probably ",
             "won't be able to read from the SWS and so this function won't ",
             "work.")
    }
    
    ## Define constants
    extractionCode = "5423"
    shareCode = "1"
    
    ## Define the dimensions and check for input errors
    allAreaCodes = GetCodeList(domain = "agriculture", dataset = "aupus_ratio",
                               dimension = "geographicAreaM49")
    allAreaCodes = allAreaCodes[type == "country", code]
    allYears = GetCodeList(domain = "agriculture", dataset = "aupus_ratio",
                               dimension = "timePointYears")[, code]
    allItemCodes = GetCodeList(domain = "agriculture", dataset = "aupus_ratio",
                               dimension = "measuredItemCPC")[, code]
    if(!is.null(geographicAreaM49)){
        stopifnot(geographicAreaM49 %in% allAreaCodes)
        allAreaCodes = geographicAreaM49
    }
    if(!is.null(timePointYears)){
        stopifnot(timePointYears %in% allYears)
        allYears = timePointYears
    }
    
    ## Extract the data
    ratioKey = DatasetKey(
        domain = "agriculture", dataset = "aupus_ratio",
        dimensions = list(
            geographicAreaM49 = Dimension(name = "geographicAreaM49", keys = allAreaCodes),
            measuredElement = Dimension(name = "measuredElement", keys = extractionCode),
            measuredItemCPC = Dimension(name = "measuredItemCPC", keys = allItemCodes),
            timePointYearsSP = Dimension(name = "timePointYearsSP", keys = allYears)
        ))
    ratioData = GetData(ratioKey)
    ratioData[, c("measuredElement", "flagRatio") := NULL]
    shareKey = DatasetKey(
        domain = "agriculture", dataset = "aupus_share",
        dimensions = list(
            geographicAreaM49 = Dimension(name = "geographicAreaM49", keys = allAreaCodes),
            measuredElement = Dimension(name = "measuredShare", keys = shareCode),
            measuredItemChildCPC = Dimension(name = "measuredItemChildCPC", keys = allItemCodes),
            measuredItemParentCPC = Dimension(name = "measuredItemParentCPC", keys = allItemCodes),
            timePointYearsSP = Dimension(name = "timePointYearsSP", keys = allYears)
        ))
    shareData = GetData(shareKey)
    shareData[, c("measuredShare", "flagShare") := NULL]
    
    ## Merge together the trees
    setnames(ratioData, "measuredItemCPC", "measuredItemChildCPC")
    tree = merge(ratioData, shareData,
                 by = c("geographicAreaM49", "measuredItemChildCPC",
                        "timePointYearsSP"), all = TRUE)
    setnames(tree, "Value.x", "extractionRate")
    setnames(tree, "Value.y", "share")
    setcolorder(tree, c("geographicAreaM49", "timePointYearsSP",
                        "measuredItemParentCPC", "measuredItemChildCPC",
                        "extractionRate", "share"))
    tree[, extractionRate := extractionRate / 100]
    tree[, share := share / 100]
    tree
}