% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/aggregateDimension.R
\name{aggregateDimension}
\alias{aggregateDimension}
\title{SWS Dimension Aggregation}
\usage{
aggregateDimension(data, keys, dim_name, key_tree, agg_flags = NULL,
  high_level = 0)
}
\arguments{
\item{data}{A long form data.table with the data to be aggregated - result
of a \code{faosws::GetData} call}

\item{keys}{A vector containing the data.table key column names (i.e. not
the value of flag columns)}

\item{dim_name}{The name of the dimension to be aggregated}

\item{key_tree}{The aggregation tree in the form data.table (parent,
children)}

\item{agg_flags}{A data.table containing the flags to attach to aggregated
values (optional)}

\item{high_level}{The highest level to which to aggregate (by default, 0,
the highest possible)}
}
\value{
The aggregated rows of data as a data.table
}
\description{
Aggregate the suppled data.table along the named dimension using the
supplied aggregation tree.
}
\details{
Aggregates the data in the supplied data.table according to the supplied key
tree. The function works from the bottom of the key tree upwards, cascading 
the aggregation until it reashes the highest level of the tree structure. 
The level at which the aggregation stops can be controlled using the 
\code{high_level} parameter. If the \code{agg_flags} parameter is supplied,
it should contain a data.table with a single row containing the flag(s) to
be applied to aggregated values; column names should be the same as those 
supplied in the \code{data} parameter. Any orphans are present as an 
attribute.
}
\examples{
\dontrun{
library(faosws)
library(data.table)
data <- GetData(swsContext.datasets[[1]])
keys <- colnames(data)
keys <- keys[!(keys \%in\% c("Value", "flagObservationStatus", "flagMethod"))]
key_tree <- GetCodeTree (domain = "agriculture", dataset = "aproduction", 
                         dimension = "measuredItemCPC")
flags <- data.table(flagObservationStatus = c("I"), flagMethod = c("s"))

ag_data <- aggregateDimension(data, keys, "measuredItemCPC", key_tree, agg_flags = flags)

## Another example

flags <- data.table(myFlag = c("A")) 
key_tree <- data.table(
  parent = c(rep("a", 3), rep ("a.1", 3), rep("b", 3)),
  children = c("a.1", "a.2", "a.3",
               "a.1.x", "a.1.y", "a.1.z",
               "b.1", "b.2", "b.3")
)
data <- data.table(
  targetDimension = c("a.2", "a.3",
                      "a.1.x", "a.1.y", "a.1.z",
                      "b.1", "b.2", "b.3", "c.1"),
  otherDimension = c(rep("10", 9)),
  Value = c(1:9),
  myFlag = c(rep("-", 9))
)
keys <- colnames(data)
keys <- keys[!(keys \%in\% c("Value", "myFlag"))]

## Do the aggregation
aggs <- aggregateDimension(data, keys, "targetDimension",
                           key_tree, agg_flags = flags)

aggs
str(aggs)
attr(aggs, "orphans")
}

}
\author{
N.A.Connell
}

