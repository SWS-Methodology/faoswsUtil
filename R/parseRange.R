#' Parse range string
#'
#' As with printer page ranges, sometimes you need to process a range. This is
#' usually of years. A user may specify that they want 2000, 2002 and 2005-2008.
#'
#' @param range character. By default, a character vector with dashes to
#'   delineate ranges and commas to separate them
#' @param sep character Regular expression to for range separator
#' @param rsep character Regular expression for range delineator
#'
#' @examples 
#' parseRange("5, 7-9, 11")
#' # [1] "5"  "7"  "8"  "9"  "11"
#' 
#' parseRange("2*3;0", sep = "; *", rsep = "\\* *")
#' # [1] "2" "3" "0"
#'
#' @return character vector of numbers
#' @export parseRange

parseRange <- function(range, sep = ", *", rsep = " *- *"){
    
    if(range == "" || length(range) == 0L){
        return(character())
    }
    
    proc_range <- unlist(strsplit(range, sep))
    proc_range <- strsplit(proc_range, rsep)
    proc_range <- unlist(lapply(proc_range, function(x){ 
        if(length(x) == 1L) return(x)
        x <- as.numeric(x)
        as.character(seq(x[1], x[2]))
    }))
    
    return(proc_range)
}
