\name{sql.update}
\alias{sql.update}
\title{Update data in a column of a PostgreSQL table}
\usage{
sql.update(table, column, value, condition)
}
\arguments{
 \item{table}{A character string with the table name}
 \item{column}{The column to be modified}
 \item{value}{Data value to insert from}
 \item{condition}{An optional condition to limit the rows modified}
}
\description{
  \code{sql.update} allows a database table to modified in place.
}
\author{Adam M. Kornick}
\seealso{\code{\link{db.write.table}},\code{\link{sql.insert}},
  \code{\link{sql.select}}}

\examples{
if (db.connection.open()) {
  data(airquality)
  rpgsql.test.data <- airquality[1:5,]
  rm(airquality)
  db.write.table(rpgsql.test.data, no.clobber=F)
  db.read.table('rpgsql.test.data')
  # remove ozone observations where temperature is
  # greater than 70 
  sql.update('rpgsql.test.data', "Ozone", 0, "\"Temp\" >70")
  # note that quotes were embedded around case sensitive variables
  db.read.table('rpgsql.test.data')
  db.rm('rpgsql.test.data', ask=F)
}
}
\keyword{database}
