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
data(airquality)
misc.data <- airquality[1:10,]
db.write.table(misc.data, no.clobber=F)
db.read.table("misc.data")
db.rm("misc.data", ask=F)
}
\keyword{database}
