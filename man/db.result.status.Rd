\name{db.result.status}
\alias{db.result.status}
\title{Get status of last query}
\usage{
status <- db.result.status()
}
\description{
  After a query is executed, db.result.status() returns the result status.
}
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
\value{
  The status number.
}
\author{Timothy H. Keitt}
\seealso{\code{\link{db.execute}}}
\keyword{database}
