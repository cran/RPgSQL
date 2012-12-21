\name{db.read.column}
\alias{db.read.column}
\title{Read a column of data from the last query result}
\usage{
db.read.column(column=1, as.is=F)
}
\arguments{
 \item{column}{A column name or number}
 \item{as.is}{A boolean indicating whether to convert character types to
 factors}
}
\description{
  \code{db.read.table} read a column of values from the last query
  results.  Column numbers begin at 1.
}
\details{
  Postgres character types are converted to factors by default.  Use
  "as.is=T" to read them as strings.
}
\value{
  A vector of data values.
}
\author{Timothy H. Keitt}
\seealso{\code{\link{db.read.table}}}

\examples{
db.execute("select * from pg_user", clear=F)
db.read.column("usename", as.is=T)
}
\keyword{database}
