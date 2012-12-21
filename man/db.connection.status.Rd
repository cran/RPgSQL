\name{db.connection.status}
\alias{db.connection.status}
\title{Get connection status}
\usage{
db.connection.status()
}
\description{
  db.connection.status returns the status of the current database connection.
}
\value{
  \item{status}{A non-zero number indicates an error.  See the
    PostgreSQL documentation for a description of the specific return
    values for different types of errors.}
}
\author{Timothy H. Keitt}
\seealso{\code{\link{db.connect()}}}
\keyword{database}
