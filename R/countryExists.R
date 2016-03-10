#' Find if country exists for a given year
#' 
#' Some countries like the former Soviet union or South Sudan dissolve or are 
#' formed during the period where we collect data. This function when given a 
#' country or a year returns a boolean expressing if the country exists for that
#' year.
#' 
#' @param countries vector of countries. Can be either names of M49 codes
#' @param years vector of years, must be same length as countries
#' @param names logical. TRUE when names are given. They must correspond to the 
#'   descriptions in agriculture:agriculture
#' @param warnNA logical. By default, a warning will be given when an NA is in 
#'   countries or year. Setting this to \code{FALSE} suppresses it.
#'   
#' @return A vector (TRUE/FALSE) of the same length as countries and year 
#'   corresponding to if they are a country or not. A warning - if this has NAs,
#'   subsetting by an NA can be problematic, i.e. result in no matches when
#'   subsetting.
#'   
#' @export

countryExists <- function(countries, years, names = FALSE, warnNA=TRUE){
  
  if(any(is.na(countries)|is.na(years)) & warnNA){
    warning("NA in country or year - if you're subsetting, this will probably return nothing")
  }
  stopifnot(length(countries) == length(years))
  
  # Use description column for names, code otherwise when merging with the
  # country start and end dates
  col <- ifelse(names, "description", "code")

  # Get list of codes and country start and end dates
  codeList <- GetCodeList("agriculture", "aproduction", "geographicAreaM49")
  # Only countries
  codeList <- codeList[get(col) %in% countries & type ==  "country",]
  # Convert all dates to years
  codeList[,`:=`(startDate = as.numeric(format(as.Date(startDate), "%Y")),
                 endDate = as.numeric(format(as.Date(endDate), "%Y")))
          ]
  # All years not specified are effectively infinite in the past or future
  codeList[is.na(startDate), startDate := -Inf]
  codeList[is.na(endDate), endDate := Inf]
  
  # The as.character is in case years is a factor
  callDf <- data.table(country = as.character(countries), year = as.numeric(as.character(years)))
  mergedDf <- merge(callDf,codeList, by.y = col, by.x = "country", all.x = T, sort = F)
  
  #Return logical for if the country exists
  mergedDf[,year >= startDate & year <= endDate]
}