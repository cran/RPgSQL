\name{Utilities}
\alias{format.table.name}
\alias{check.table.name}
\alias{make.db.names}
\alias{list.to.csv}
\alias{single.quote}
\alias{double.quote}
\alias{format.null.values}
\title{RPgSQL utility functions}
\usage{
format.table.name(table.name)
check.table.name(table.name)
make.db.names(name)
list.to.csv(...)
single.quote(...)
double.quote(...)
format.null.values(...)
}
\arguments{
 \item{table.name}{A character string containing the name of a postgres
   table or a proxy object pointing to a postgres table}
 \item{name}{A character string with R names}
 \item{\dots}{A list of values to format}
}
\description{
  \code{format.table.name} converts R names with dot seperators to
  postgres friendly names with underscore separators.\cr
  \code{check.table.name} calls \code{format.table.name} and then
  verifies that the table exists in the current database.\cr
  \code{make.db.namess} replaces periods with underscores.\cr
  \code{list.to.csv} converts a list of values to a string containing
  comma separated values.\cr
  \code{single.quote} wraps strings with single quotes.\cr
  \code{double.quote} wraps strings with double quotes.\cr
  \code{format.null.values} returns R NULL values where needed.\cr
}
\details{
  If a db.proxy object is passed to \code{convert.table.name}, the
  postgres table name will be extracted from the proxy and returned.
  
  When calling \code{check.table.name}, if the table does not exist,
  then \code{\link{stop}} is called and a warning issued.  A db.proxy
  object may also be passed to \code{check.table.name}, in which case
  the table name is extracted from the proxy object. 
}
\author{Timothy H. Keitt}
\keyword{database}
