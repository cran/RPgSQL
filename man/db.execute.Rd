\name{db.execute}
\alias{db.execute}
\title{Send commands to the postgres backend}
\usage{
db.execute(..., clear=T)
}
\arguments{
 \item{\dots}{A character string containing any valid command or query
   to be sent to the postgres backend.}
 \item{clear}{If true, free any resources allocated by db.execute()
   immediately on returning.}
}
\description{
  db.execute() sends a query to the database backend for processing.  If
  the query returns any results, and clear=F, then the results will be
  available for reading with db.get.value().
}
\note{
  If the postgres library was compiled with -DDEBUG, then all queries
  will be echoed to stdout.
}
\references{\url{http://www.postgresql.org/}}
\author{Timothy H. Keitt}
\seealso{\code{\link{db.get.value}}, \code{\link{db.clear.result}}}

\examples{
db.execute("select * from pg_user", clear=F)
for (i in 1:db.result.rows()) cat(db.get.value(row=i), "\n")
db.clear.result()
}
\keyword{database}
