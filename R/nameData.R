##' Name SWS dataset
##'
##' Adds human-readable names to keys automatically.
##'
##' @param domain character. Domain of dataset
##' @param dataset character. Name of dataset
##' @param dt data.table. The data.table to which you wish to add names
##' @param except character. Vector of names in the table that you do not wish
##'   named. For example, "timePointYears"
##' @param append character. suffix to add to column name of names
##'
##' @examples
##' \dontrun{
##' dat = GetData(key)
##' nameData("agriculture", "aproduction", dat, except = "timePointYears")
##' }
##'
##' @export

nameData <- function(domain, dataset, dt, except, append = "_description"){
  conf <- GetDatasetConfig(domain, dataset)
  ##Which keys are actually dimensions
  keys <- intersect(names(dt), conf[["dimensions"]])

  ##Except this key
  if(missing(except)) except <- character(0)
  keys <- setdiff(keys, except)

  codes2merge <- lapply(keys, GetCodeDescription, domain=domain, dataset=dataset, append=append)
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
  }
  newdata
}

GetCodeDescription <- function(domain, dataset, key, append = "_description"){
  codeTable <- GetCodeList(domain, dataset, key)[,.(code, description)]
  setnames(codeTable, c("code", "description"), c(key, paste0(key, append)))
  codeTable
}
