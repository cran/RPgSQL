\name{db.read.table}
\alias{db.read.table}
\title{Read a postgres table into a data frame}
\usage{
db.read.table(name, row.names)
}
\arguments{
 \item{name}{The name of a postgres table or a proxy object}
 \item{row.names}{A list of row names to apply to the data frame}
}
\description{
  \code{db.read.table} reads a database table into a data frame.
}
\details{
  If no arguments are provided, \code{db.read.table} will attempt to
  read the results of the last successful call to
  \code{\link{db.execute}}.  Note that this will fail unless
  \code{\link{db.execute}} was called with 'clear=F'.  If a table name
  or proxy object is provided, all rows and columns of the corresponding
  table will be read.
}
\value{
  A data frame containing the data table.
}
\author{Timothy H. Keitt}
\note{Character string data types stored in a database table column will
be converted to R type "factor".  I need to add an "as.is" option.}
\seealso{\code{\link{db.write.table}}, \code{\link{db.read.column}}}

\examples{
#Note that "pg.user" is converted to "pg_user"
db.read.table("pg.user")
#Can't use "pg.user" here
db.execute("select usename, usesysid from pg_user", clear=F)
db.read.table()
db.clear.result()
}
\keyword{database}
