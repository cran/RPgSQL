\name{sql.insert}
\alias{sql.insert}
\title{Append data to a PostgreSQL table}
\usage{
sql.insert(into, column.names, values, query)
}
\arguments{
 \item{into}{A character string with the table name}
 \item{column.names}{An optional list of column names}
 \item{values}{Data values, matrix or data frame to insert from}
 \item{query}{An SQL query}
}
\description{
  \code{sql.insert} is quite useful for appending data to database tables.
}
\details{
  The easiest way to use \code{sql.insert} is to use the \code{into} and
  \code{values} arguments.  Enter the table name in the \code{into}
  argument and set \code{values} to a data frame.  \code{sql.insert} will
  extract the column names from the data frame and update those
  columns.  See example below.
}
\author{Timothy H. Keitt}
\seealso{\code{\link{db.write.table}}}

\examples{
if (db.connection.open()) {
  data(airquality)
  rpgsql.test.data <- airquality[1:5,]
  db.write.table(rpgsql.test.data, no.clobber=F)
  db.read.table('rpgsql.test.data')
  rpgsql.test.data <- airquality[6:10,]
  sql.insert(into='rpgsql.test.data', values=rpgsql.test.data)
  db.read.table('rpgsql.test.data')
  db.rm('rpgsql.test.data', ask=F)
}
}
\keyword{database}
