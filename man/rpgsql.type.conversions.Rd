\name{Type conversions}
\alias{rpgsql.data.type}
\alias{rpgsql.data.type.default}
\alias{rpgsql.format.values}
\alias{rpgsql.format.values.default}
\alias{rpgsql.cast.values}
\alias{rpgsql.cast.values.default}
\title{Conversion between R and PostgreSQL data types}
\usage{
rpgsql.data.type(x)
rpgsql.data.type.default(x)
rpgsql.format.values(x)
rpgsql.format.values.default(x)
rpgsql.cast.values(x)
rpgsql.cast.values.default(x)
}
\arguments{
 \item{x}{A vector of data values}
}
\description{
  \code{rpgsql.data.type} is a generic function that returns a
  PostgreSQL type based on the type of data containted in \code{x}.\cr
  \code{rpgsql.format.values} is a generic function that returns
  appropriate formatted strings compatible with the PgSQL insert
  command.\cr
  \code{rpgsql.cast.values} is a generic function that converts PgSQL
  character output to R objects or types.
}
\details{
  To enable the RPgSQL package to use a new type, you need to do the
  following four things.

  First, you must assign your type a class name.

  Second, you must write a \code{\link{rpgsql.data.type}} method for
  your class.  This function must return a valid PostgreSQL type name
  compatible with the SQL \code{create table} command.

  Third, you must write a \code{\link{rpgsql.format.values}} method for
  your class.  This function will recieve a vector of values of your
  class and must convert these values into strings storable in PgSQL.

  Fourth, you must write a \code{\link{rpgsql.cast.values}} method. This
  method does not get called for your class.  Instead it gets called for
  the class named for the equivalent PgSQL type number (see
  \code{\link{db.result.column.type}}).  For example, the PgSQL "DATE"
  type has number 1082, so you must write a method for class "1082" (see
  example below).  This function will recieve a vector of strings as
  returned from the PgSQL \code{select} command and must convert these
  strings into instances of your class. 
}
\author{Timothy H. Keitt}
\examples{
  # Note: these are already defined in RPgSQL
  # Objects of class "date" are mapped to PgSQL type "DATE"
  # Called in db.write.table.
  rpgsql.data.type.dates <- function(x) return("DATE")

  # Special handler called by insert that formats "date" objects
  # so that they are compatible with PgSQL "DATE" objects.  The
  # default method handles single quoting strings and removing
  # NA, Inf and NaN values.  Called in insert.
  rpgsql.format.values.dates <- function(x) {
    attr(x, "format") <- "day mon year"
    return(rpgsql.format.values.default(x))
  }

  # A handler that converts PgSQL type "1082" (which corresponds
  # to the PgSQL "DATE" type) back to the R "date" type (see the
  # "chron" package).  Called in db.read.column.
  rpgsql.cast.values.1082 <- function(x) return(dates(x, format='m-d-y'))
}
\keyword{database}
