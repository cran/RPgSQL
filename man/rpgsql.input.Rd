\name{Read from database}
\alias{db.read.table}
\alias{db.fetch.result}
\alias{db.read.column}
\title{Read query results into a data frame}
\usage{
db.read.table(name, row.names, col.names, as.is=F)
db.fetch.result(row.names, col.names, as.is=F)
db.read.column(column=1, as.is=F)
}
\arguments{
 \item{name}{The name of a postgres table or a proxy object}
 \item{row.names}{A list of row names to apply to the data frame}
 \item{col.names}{A list of column names for the resulting data frame}
 \item{as.is}{A boolean or vector of booleans or column indices
   specifying which character type columns should not be converted to
   factors}
 \item{column}{The column number to read from the result buffer}
}
\description{
  \code{db.read.table} reads a database table into a data
  frame.\cr
  \code{db.fetch.result} reads the contents of the query buffer
  into a data frame.\cr
  \code{db.read.column} returns a vector containing
  values from a given column in the result buffer. 
}
\details{
  \code{db.read.table} simply queries the entire table and then calls
  \code{db.fetch.result}.\cr
  \code{db.read.column} does all the work; it
  is useful when you want the results to be a vector and not a data
  frame.
}
\value{
  \code{db.read.table} returns a data frame containing the data table.\cr
  \code{db.fetch.result} returns data frame containing the query
  results.\cr
  \code{db.read.column} returns a vector containing a single
  column of query output.
}
\author{Timothy H. Keitt}
\seealso{\code{\link{db.write.table}}}

\examples{
if (db.connection.open()) {
  db.read.table('pg_user')
  db.execute('select usename, usesysid from pg_user', clear=F)
  db.fetch.result()
  db.read.column(column=2)
  db.clear.result()
}
}
\keyword{database}
