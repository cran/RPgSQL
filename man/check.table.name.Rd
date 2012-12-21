\name{check.table.name}
\alias{check.table.name}
\title{verify and format table name}
\usage{
check.table.name(table.name)
}
\arguments{
 \item{table.name}{A character string containing the postgres table name}
}
\description{
  \code{check.table.name} calls \code{\link{convert.table.name}} and
  then verifies that the table exists in the current database.
}
\details{
  If the table does not exist, then \code{\link{stop}} is called and a
  warning issued.  A db.proxy object may also be passed to
  \code{check.table.name}, in which case the table name is extracted
  from the proxy object.
}
\references{PostgreSQL \url{http://www.postgresql.org/}}
\author{Timothy H. Keitt}
\seealso{\code{\link{db.table.exists}}}
\keyword{database}



