% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/insert_missing.R
\name{insert_missing}
\alias{insert_missing}
\title{Insert missing values}
\usage{
insert_missing(x, p = 0.1, cols = NULL)
}
\arguments{
\item{x}{A data frame or matrix.}

\item{p}{Proportion of values to replace at random with \code{NA}.}

\item{cols}{Either a character vector of columns names or an integer vector 
of column positions specifying which columns to insert mssing values into.
Default is \code{NULL} which will use all avaialble columns.}
}
\description{
Insert missing values into a data frame or matrix.
}
\examples{
insert_missing(iris, 0.5)
insert_missing(iris, 0.5, cols = 1:4)
}
