% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/getFBSCode.R
\name{getFBSCode}
\alias{getFBSCode}
\title{Get FBS Code}
\usage{
getFBSCode(commCodeFCL)
}
\arguments{
\item{commCodeFCL}{A vector of commodity codes in the FCL coding format.}
}
\value{
A vector of FBS codes corresponding to the passed FCL codes. 
  Missing codes are returned with a NA.
}
\description{
This function takes a FCL commodity code and returns the FBS aggregate code 
that contains this data.
}

