\name{db.write.table}
\alias{db.write.table}
\title{Write an R data frame into a postgres table}
\usage{
db.write.table(data, name=deparse(substitute(data)), no.clobber=T, write.row.names=F)
}
\arguments{
 \item{data}{A data frame or object that inherits from \code{data.frame}}
 \item{name}{A name for the postgres table}
 \item{no.clobber}{A boolean indicating whether to overwrite existing tables}
 \item{write.row.names}{A boolean indicating whether to write row names
   into the database table}
}
\description{
  \code{db.write.table} writes data stored in an R data frame into a
  postgres table.
}
\author{Timothy H. Keitt}
\note{The current implementation is not very fast.}
\seealso{\code{\link{db.read.table}}}

\examples{
if (db.connection.open()) {
  data(airquality)
  rpgsql.test.data <- airquality[1:10,]
  rm(airquality)
  db.write.table(rpgsql.test.data, no.clobber=F)
  if (db.table.exists('rpgsql_test_data')) {
    db.read.table('rpgsql.test.data')
    db.rm('rpgsql.test.data', ask=F)
  }
}
}
\keyword{database}
