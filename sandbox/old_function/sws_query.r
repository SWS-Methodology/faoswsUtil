#' Get data from FAO statistical working system (SWS).
#' 
#' Using this function is discouraged and shouldn't be done unless making
#' comparisons between the old and new working system. This function will not
#' work on the SWS. This is intentional.
#' 
#' @param area numeric or character vector with countries' ids (numeric) or 
#'   names (character).
#' @param item numeric or character vector with items' ids (numeric) or names 
#'   (character).
#' @param ele numeric or character vector with elements' ids (numeric) or names 
#'   (character).
#' @param year numeric vector with years to fetch.
#' @param symb optional character vector with symbols (flags) to fetch data type
#'   of.
#' @param melted logical, TRUE by default. Should the result data be returned in
#'   long format (instead of one column per every year two columns: year and 
#'   corresponded value).
#' @param value.names logical, TRUE by default. Should identificational vectors 
#'   (area and item) be converted from numeric ids to names.
#' @param stringsAsFactors logical. Should character 
#'   identificational vectors be converted to factors.
#' @param dbquery optional string with SQL-query to request from SWS.
#' @param class.path optional string with path to Oracle Java Data Base
#'   Connectivity library. By default 'ojdbc14.jar' in the working directory.
#' @param user optional string with name of SWS DB user instead of default user 
#'   'demo'.
#' @param pass optional string with password of SWS DB user instead of default 
#'   password 'demo'.
#'    
#' @return data.frame with results from SWS DB.
#' 
#' @export


sws_query <- function(area, item, ele, year, symb = T, melted = TRUE, 
                      value.names = T, 
                      stringsAsFactors = default.stringsAsFactors(),
                      dbquery, class.path = file.path(getwd(),'ojdbc14.jar'),
                      user = 'demo', pass = 'demo') {

  # Check for ojdbc14.jar
  if(!file.exists(class.path)) 
    stop("Oracle JDBC class not found. Please, put file ojdbc14.jar into the ",
         "working directory or specify full path with class.path argument.")
  
  # Check for the internal connection
  # Source of ping function:
  # http://stackoverflow.com/questions/7012796/ping-a-website-in-r
  ping <- function(x){
    pingvec <- system2("ping",paste('-n 1', x),
                       stderr=FALSE, stdout=FALSE)
    pingvec == 0
  }
  
  if(!ping("lprdbwo1.fao.org"))
    stop("SWS DB accepts only internal connections. Please get a cable and find 
the nearest ethernet socket :)")
  
  drv <- RJDBC::JDBC(driverClass = "oracle.jdbc.driver.OracleDriver",
              classPath = class.path)
  conn <- DBI::dbConnect(drv, "jdbc:oracle:thin:@lprdbwo1:3310:fstp",
                    user = user, password = pass)
  
  # This is the exclusive request to DB in the function.
  # All others just invoke sws_query with dbquery argument.
  if(!missing(dbquery)) {
    dboutput <- DBI::dbGetQuery(conn, dbquery)
    DBI::dbDisconnect(conn)
    return(dboutput)
  }

  year_shift <- 1960
  
  # Function to convert year in colnames, e.g. from 00 to 1960
  convertyear <- function(x) {
    # Vectorizing the function
    if(length(x) > 1) {
      return(unlist(plyr::llply(x, convertyear)))
    }
    
    if(!stringr::str_detect(x, '[0-9]{2}$')) return(x)
    orignumb <- as.numeric(stringr::str_extract(x, '[0-9]{2}$'))
    corryear <- orignumb + year_shift
    corrname <- stringr::str_c(stringr::str_replace(x, '(^.*)([0-9]{2}$)', '\\1'),
                      corryear)
    corrname
  }
  
  
  # convert vectors in arguments to collapsed strings.
  if(!missing(area)) area <- stringr::str_c(area, collapse=', ')
  if(!missing(item)) item <- stringr::str_c(item, collapse=', ')
  if(!missing(ele)) ele <- stringr::str_c(ele, collapse=', ')
  
  # In case user specifies only one ele and and one item we want function to 
  # return colname of value as name of ele instead of value.
  # ...
  
  if(!missing(year) & symb) flag <- 
    stringr::str_c('SYMB_', formatC(year - year_shift, width=2, format='d', flag='0'),
          collapse = ', ')
  if(!missing(year)) year <- 
    stringr::str_c('NUM_', 
          formatC(year - year_shift, width=2, format='d', flag='0'), collapse=', ')

  
  # Constructing query
  # Name of data base table. In case of using tsv_ics_work_yr it's not
  # require to convert years from 00 to 1960. But possibly you need to 
  # remove totals.
  dbmain <- 'TS_ICS_WORK_YR'
  
  # WHAT part of query
  if(value.names) 
    whatsql <- stringr::str_c('area.name_e as area',
                     'item.name_e as item', 
                     sep = ', ') else
      whatsql <- stringr::str_c('area', 'item', sep = ', ')
  
  
  whatsql <- stringr::str_c(whatsql, 'ele', sep = ', ')
  
  if(!missing(year)) whatsql <- stringr::str_c(whatsql, year, sep=', ')
  if(!missing(year) & symb) whatsql <- stringr::str_c(whatsql, flag, sep=', ')

  
  # FROM
  fromsql <- dbmain
  if(value.names) fromsql <- stringr::str_c(stringr::str_c('FAOSTAT.',fromsql),
                                   'FAOSTAT.AREA, FAOSTAT.ITEM', sep = ', ')
  
  # WHERE
  wheresql <- list()
  if(!missing(area)) wheresql[length(wheresql) + 1] <- 
    stringr::str_c(dbmain, '.area in (', area, ') ')
  if(!missing(item)) wheresql[length(wheresql) + 1] <-
    stringr::str_c(dbmain, '.item in (', item, ') ')
  if(!missing(ele)) wheresql[length(wheresql) + 1] <-
    stringr::str_c(dbmain, '.ele in (', ele, ') ')
#   if(length(wheresql == 0)) wheresql[1] <- '*'
  
  if(value.names) wheresql[length(wheresql) + 1] <- 
    stringr::str_c('AREA.AREA = ', dbmain, '.AREA and item.item = ', dbmain, '.item')
  
  wheresql <- stringr::str_c(unlist(wheresql), collapse=' and ')
  
  
  constrdbquery <- stringr::str_c('select ', whatsql, ' from ', 
                         fromsql, ' where ', wheresql
  )

  # Ask the DB with constructed query
  dboutput <- sws_query(class.path=class.path, dbquery=constrdbquery)
  
  colnames(dboutput) <- tolower(colnames(dboutput))
#   colnames(dboutput)[1:2] <- c('area', 'item')
  
  # Converting colnames with years from 00 to 1960
  if(tolower(dbmain) == 'ts_ics_work_yr')
    colnames(dboutput) <- convertyear(colnames(dboutput))
  
  # Converting from wide format to long.
  if(melted) {
    
    # Selecting part with values only (without symbols/flags)
    valueswithoutsymb <- dboutput[, colnames(dboutput)[
      stringr::str_detect(colnames(dboutput), stringr::perl('^(?!symb)'))]]
    
    # Melting
    valueswithoutsymb <- 
      reshape2::melt(valueswithoutsymb, measure.vars=
             names(valueswithoutsymb[stringr::str_detect(names(valueswithoutsymb),
                                          '^num_')]),
           variable.name = 'year')
    
    # Convert character vector with year to numeric
    valueswithoutsymb$year <-
        as.numeric(stringr::str_replace(valueswithoutsymb$year, '^num_', ''))
    
    # Converting part with symbols/flags
    if(symb) {
      flags <- dboutput[, colnames(dboutput)[
        stringr::str_detect(colnames(dboutput), stringr::perl('^(?!num)'))]]
      
      flags <- 
        reshape2::melt(flags, measure.vars=
               names(flags[stringr::str_detect(names(flags), '^symb_')]),
             variable.name = 'year', value.name = 'flag')
      
      flags$year <- as.numeric(stringr::str_replace(flags$year, '^symb_', ''))
      
      # Joining values and flags
      dboutput <- plyr::join(valueswithoutsymb, flags,
                             by = c('area', 'item', 'ele', 'year'))
      
    } else {
      dboutput <- valueswithoutsymb
    }
    
  }
    
  dboutput
  
}
