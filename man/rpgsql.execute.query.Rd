\name{Execute queries}
\alias{db.execute}
\alias{db.result.status}
\alias{db.clear.result}
\alias{db.toggle.echo}
\title{Send commands to the PostgreSQL back end}
\usage{
db.execute(..., clear=T, report.errors=T)
db.result.status()
db.clear.result()
db.toggle.echo()
}
\arguments{
 \item{\dots}{A character string containing any valid command or query
   to be sent to the PostgreSQL back end.}
 \item{clear}{If true, free any resources allocated by db.execute()
   immediately on returning.}
 \item{report.errors}{If true, report fatal and non-fatal errors}
}
\description{
  \code{db.execute} sends a query to the database back end for
  processing.
  \code{db.result.status} returns a result code indicating the status of
  the result buffer.\cr
  \code{db.clear.result} frees the result buffer.\cr
  \code{db.toggle.echo} toggles between echo'ing all communication with
  the back end. 
}
\details{
  See the file "libpq-fe.h" in your PostgreSQL distribution for a
  listing of error codes.
}
\author{Timothy H. Keitt}
\seealso{\code{\link{db.result.get.value}}}

\examples{
if (db.connection.open()) {
  db.execute("select * from pg_user", clear=F)
  db.result.status()
  for (i in 1:db.result.rows()) cat(db.result.get.value(row=i), "\n")
  db.clear.result()
}
}
\keyword{database}


