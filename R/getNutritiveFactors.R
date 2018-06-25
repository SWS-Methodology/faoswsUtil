##' Get Nutritive Factors
##' 
##' This function pulls nutrient factor data from the SWS.  The tricky part is 
##' that some nutrient factors are specific to country/year, and some apply to 
##' all years or all countries or both.  Thus, we must combine these factors in 
##' a reasonable way and generate a useable table.
##' 
##' @param nutrientDomain A character specifig the SWS domain where the nutrient factor 
##'   table is stores in. The default value points to the "agriculture" domain.
##' @param nutrientDataset A character specifig the SWS dataset where the nutrient factor 
##'   table is stores in. The default value points to the "aupus_ratio" domain.  
##' @param geographicAreaM49 A character vector of area codes.  "0" (wildcard) 
##'   will always be included if it is not passed, but passing it does not cause
##'   any issues.  If NULL, the function pulls all area codes from the dimension
##'   table.
##' @param measuredElement A character vector of measuredElement codes.  These 
##'   are specific to the nutrient factors table (i.e. Aupus Ratios).  Calories,
##'   proteins, and fats are 1001, 1003, and 1005, respectively.  If NULL, the 
##'   function pulls all area codes from the dimension table.
##' @param measuredItemCPC A character vector of CPC codes for the commodities 
##'   of interest.  If NULL, the function pulls all area codes from the 
##'   dimension table.
##' @param timePointYearsSP A character vector of years.  As with 
##'   geographicAreaM49, "0" need not be passed but can be.  If NULL, the 
##'   function pulls all area codes from the dimension table.
##' @param flagVector Vector of charactes identifing the colnames referring to the flag
##'  column/columns. This parameter has been added in order to allow the pull nutrient-data from diffent 
##'  dataset  characterised from different flag systems. For example the default option has been set
##'  in order to pull  nutrient-data from the aupus-ratio dataset.
##'   
##'   NOTE: This function chooses the nutrient factor for a particular 
##'   country/year by first looking for data specific to that country/year, then
##'   data specific to that country and all years, then data specific to that 
##'   year and all countries, then data for all years and all countries.  This
##'   order may not be appropriate, but it was my best guess at the time of
##'   writing the code.
##'   
##' @return A data.table containing the nutrient factors.  Any nutrient factor 
##'   that applies to multiple years/countries is now represented as multiple 
##'   rows in the table.
##'   
##' @export
##' 

getNutritiveFactors = function(nutrientDomain = "agriculture" , nutrientDataset = "aupus_ratio",
                               geographicAreaM49 = NULL, measuredElement = NULL,
                               measuredItemCPC = NULL, timePointYearsSP = NULL,
                               flagVector = c("flagRatio")){
    ## Input Checks
    if(!exists("swsContext.datasets")){
        stop("swsContext objects not defined!  Please run GetTestEnvironment.")
    }
    
    ## If NULL is passed, use all codes
    if(is.null(geographicAreaM49)){
        geographicAreaM49 = GetCodeList(nutrientDomain, nutrientDataset,
                                        "geographicAreaM49")[, code]
    }
    if(is.null(measuredElement)){
        measuredElement = GetCodeList(nutrientDomain, nutrientDataset,
                                      "measuredElement")[, code]
    }
    if(is.null(measuredItemCPC)){
        measuredItemCPC = GetCodeList(nutrientDomain, nutrientDataset,
                                      "measuredItemCPC")[, code]
    }
    if(is.null(timePointYearsSP)){
        timePointYearsSP = GetCodeList(nutrientDomain, nutrientDataset,
                                       "timePointYearsSP")[, code]
    }
    
    ## Add wildcard values
    areaKeys = unique(c(geographicAreaM49, 0))
    yearKeys = unique(c(timePointYearsSP, 0))
    
    ## Create the DatasetKey object to pull the data
    nutrientKey = DatasetKey(domain = nutrientDomain, dataset = nutrientDataset,
                             dimensions = list(
                                 geographicAreaM49 = Dimension(name = "geographicAreaM49",
                                                               keys = areaKeys),
                                 measuredElement = Dimension(name = "measuredElement",
                                                             keys = measuredElement),
                                 measuredItemCPC = Dimension(name = "measuredItemCPC",
                                                             keys = measuredItemCPC),
                                 timePointYearsSP = Dimension(name = "timePointYearsSP",
                                                              keys = yearKeys))
                             )
    allData = GetData(nutrientKey)
    
    ## Start with country/year specific data
    output = allData[timePointYearsSP != "0" & geographicAreaM49 != "0", ]

    ## Include country specific data
    toInclude = allData[timePointYearsSP == "0" & geographicAreaM49 != "0", ]
    toInclude[, timePointYearsSP := NULL]
    toInclude[, mergeDummy := 1]
    yearDT = data.table(timePointYearsSP = yearKeys[yearKeys != "0"],
                        mergeDummy = 1)
    toInclude = merge(toInclude, yearDT, by = "mergeDummy",
                      allow.cartesian = TRUE)
    toInclude[, mergeDummy := NULL]
    output = merge(output, toInclude, all = TRUE,
                   by = c("geographicAreaM49", "measuredElement",
                          "measuredItemCPC", "timePointYearsSP"),
                   suffixes = c("", ".new"))
    flagVector.new=paste0(flagVector, ".new")
    
    if(length(flagVector.new)>1){
        for(i in seq_len(length(flagVector.new))){
        output[is.na(Value), c( flagVector[i]) := list(get(flagVector.new[i]))]  
        }
        output[is.na(Value), c("Value") := list(Value.new)]    
        
    }else{
    output[is.na(Value), c("Value", flagVector) := list(Value.new, get(flagVector.new))]
        }
    output[, c("Value.new", flagVector.new) := NULL]
    
    
    ## Include year specific data
    toInclude = allData[timePointYearsSP != "0" & geographicAreaM49 == "0", ]
    toInclude[, geographicAreaM49 := NULL]
    toInclude[, mergeDummy := 1]
    areaDT = data.table(geographicAreaM49 = areaKeys[areaKeys != "0"],
                        mergeDummy = 1)
    toInclude = merge(toInclude, areaDT, by = "mergeDummy",
                      allow.cartesian = TRUE)
    toInclude[, mergeDummy := NULL]
    output = merge(output, toInclude, all = TRUE,
                   by = c("geographicAreaM49", "measuredElement",
                          "measuredItemCPC", "timePointYearsSP"),
                   suffixes = c("", ".new"))
    if(length(flagVector.new)>1){
        for(i in seq_len(length(flagVector.new))){
            output[is.na(Value), c( flagVector[i]) := list(get(flagVector.new[i]))]  
        }
        output[is.na(Value), c("Value") := list(Value.new)]    
        
    }else{
        output[is.na(Value), c("Value", flagVector) := list(Value.new, get(flagVector.new))]
    }
    output[, c("Value.new", flagVector.new) := NULL]
    
    ## Include generic data
    toInclude = allData[timePointYearsSP == "0" & geographicAreaM49 == "0", ]
    toInclude[, c("geographicAreaM49", "timePointYearsSP") := NULL]
    toInclude[, mergeDummy := 1]
    toInclude = merge(toInclude, areaDT, by = "mergeDummy",
                      allow.cartesian = TRUE)
    toInclude = merge(toInclude, yearDT, by = "mergeDummy",
                      allow.cartesian = TRUE)
    toInclude[, mergeDummy := NULL]
    output = merge(output, toInclude, all = TRUE,
                   by = c("geographicAreaM49", "measuredElement",
                          "measuredItemCPC", "timePointYearsSP"),
                   suffixes = c("", ".new"))
    if(length(flagVector.new)>1){
        for(i in seq_len(length(flagVector.new))){
            output[is.na(Value), c( flagVector[i]) := list(get(flagVector.new[i]))]  
        }
        output[is.na(Value), c("Value") := list(Value.new)]    
        
    }else{
        output[is.na(Value), c("Value", flagVector) := list(Value.new, get(flagVector.new))]
    }
    output[, c("Value.new", flagVector.new) := NULL]
    
    return(output)
}