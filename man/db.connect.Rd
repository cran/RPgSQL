\name{db.connect}
\alias{db.connect}
\title{Connect to database}
\usage{
db.connect(host, port, dbname, user, password, authtype, options, tty)
}
\arguments{
 \item{host}{postgres server host name}
 \item{port}{postgres server port number}
 \item{dbname}{database name}
 \item{user}{database user name}
 \item{password}{user password}
 \item{authtype}{authentication type}
 \item{options}{additional postgres options}
 \item{tty}{debugging tty}
}
\description{
  db.connect() establishes a connection between the R postgres library
  and the PostgreSQL backend.
}
\details{
  Default values for all of the arguments to \code{db.connect} can be
  set using the following environment variables:
  
  PGHOST sets the default server name. If a non-zero-length string is
  specified, TCP/IP communication is used. Without a host name, libpq will
  connect using a local Unix domain socket.

  PGPORT sets the default port or local Unix domain socket file
  extension for communicating with the Postgres backend.

  PGDATABASE sets the default Postgres database name.

  PGUSER sets the username used to connect to the database and for
  authentication.

  PGPASSWORD sets the password used if the backend demands password
  authentication.

  PGOPTIONS sets additional runtime options for the Postgres backend.
}
\references{\url{http://www.postgresql.org/}}
\author{Timothy H. Keitt}
\seealso{\code{\link{db.disconnect()}}}
\keyword{database}
