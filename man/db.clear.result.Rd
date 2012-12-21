\name{db.clear.result}
\alias{db.clear.result}
\title{Free allocated resources}
\usage{
db.clear.result()
}
\description{
  Frees all memory allocated after a successful query.
}
\details{
  Any resources allocated by libpq will automatically be cleared before
  any new query is executed.  Therefore, one only need call
  db.clear.result() if no new query is to be executed.
}
\author{Timothy H. Keitt}
\seealso{\code{\link{db.execute}}}
\keyword{database}
