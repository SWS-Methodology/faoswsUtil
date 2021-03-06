% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/getDescendants.R
\name{getDescendants}
\alias{getDescendants}
\title{Get descendants}
\usage{
getDescendants(tree, parentColname, childColname)
}
\arguments{
\item{tree}{A data.table object.}

\item{parentColname}{The column name of the parent column of tree.}

\item{childColname}{The column name of the child column of tree.}
}
\value{
A data.table object with two columns: the first is the column of all
  the top level nodes and the next contains all the descendants of that
  node.
}
\description{
This function takes a data.table in a tree like structure and returns, for 
each top node, a data.table with all the descendants (children of children 
of ...).
}

