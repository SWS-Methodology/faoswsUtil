% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/getCommodityTreeNewMethod.R
\name{getCommodityTreeNewMethod}
\alias{getCommodityTreeNewMethod}
\title{Get Commodity Tree}
\usage{
getCommodityTreeNewMethod(geographicAreaM49 = NULL, timePointYears = NULL)
}
\arguments{
\item{geographicAreaM49}{A character vector of area codes.  The trees 
returned are specific to country and year; thus, providing this parameter 
limits which trees are pulled. If NULL, all are used.}

\item{timePointYears}{A character vector of years.  See geographicAreaM49.}
}
\value{
A data.table object containing the commodity tree.  The dimension 
  columns correspond to the country, year, parent, and child commodity.  Two
  value columns are available: extraction rate and share.  The logic of the
  NEW system is that 0 ExtractionRates are NA, meaning that that connection
  is not valid for that cpuntry/commodity/year combination
}
\description{
This function pulls the commodity trees in the new CPC system.
}

