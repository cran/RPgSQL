\name{Execute queries}
\alias{db.execute}
\alias{db.clear.result}
\alias{db.result.status}
\alias{db.toggle.echo}
\title{Send commands to the postgres backend}
\usage{
db.execute(..., clear=T, report.errors=T)
db.result.status()
db.clear.result()
db.toggle.echo()
}
\arguments{
 \item{\dots}{A character string containing any valid command or query
   to be sent to the postgres backend.}
 \item{clear}{If true, free any resources allocated by db.execute()
   immediately on returning.}
 \item{report.errors}{If true, report fatal and non-fatal errors}
}
\description{
  \code{db.execute} sends a query to the database backend for
  processing.
  \code{db.result.status} returns a result code indicating the status of
  the result buffer.\cr
  \code{db.clear.result} frees the result buffer.\cr
  \code{db.toggle.echo} toggles between echo'ing all communication with
  the backend. 
}
\notes{
  \details{
  The file "libpq-fe.h" defines the result status as:

  typedef enum\cr
  {\cr
    PGRES_EMPTY_QUERY = 0,\cr
    PGRES_COMMAND_OK,		/* a query command that doesn't return\cr
				 * anything was executed properly by the\cr
				 * backend */\cr
    PGRES_TUPLES_OK,		/* a query command that returns tuples was\cr
				 * executed properly by the backend,\cr
				 * PGresult contains the result tuples */\cr
    PGRES_COPY_OUT,		/* Copy Out data transfer in progress */\cr
    PGRES_COPY_IN,		/* Copy In data transfer in progress */\cr
    PGRES_BAD_RESPONSE,		/* an unexpected response was recv'd from\cr
				 * the backend */\cr
    PGRES_NONFATAL_ERROR,\cr
    PGRES_FATAL_ERROR\cr
  } ExecStatusType;\cr

  (The status numbers are listed in order starting from 0.)
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


