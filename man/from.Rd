% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/packages.R
\name{from}
\alias{from}
\title{Import specific objects from packages}
\usage{
from(pkg, import, as, envir = .GlobalEnv)
}
\arguments{
\item{pkg}{Character string giving the name of the specific R package from
which the objects will be imported.}

\item{import}{Character vector specifying the particular objects to import
from \code{pkg}.}

\item{as}{Optional character vector specifying the names for which
the imported objects will be assigned to. If missing, then the original
objects (i.e., \code{objects}) names will be used.}

\item{envir}{The environment for which the objects will be loaded into. The
default is \code{.GlobalEnv} meaning load the objects into the global
environment.}
}
\description{
Import specific objects (e.g., functions) from a packages namespace.
}
\details{
Normally, one could implement this via \code{object <- pkg::object}, but that
would require a new assignment for each object imported. This method allows
one to import many objects at once from a particular package. It is also less
error-prone then using \code{pkg::object} each time \code{object} is used.
}
\examples{
# Importing package plyr's entire namespace can cause issues when package
# dplyr is loaded. Using importr can help mitigate these types of issues. The 
# following example assumes that both dplyr and plyr are installed.
library(dplyr)
from("plyr", import = c("ddply", "ldply"))
from("plyr", import = c("ddply", "ldply"), as = c(".ddply", ".ldply"))
identical(ddply, .ddply)
}
