\name{pgSQLDriver-methods}
\docType{methods}
\alias{pgSQLDriver-methods}
\alias{dbConnect,pgSQLDriver-method}
\title{Methods for the class JDBCDriver in Package `RJDBC'}
\description{
  Methods for the class `pgSQLDriver' in Package `pgSQL'.  In addition to methods
listed here there are methods inherited from \code{JDBCDriver}.

  \code{dbConnect} creates a new pgSQL connection.  
}
\section{Methods}{
  \describe{
    \item{dbConnect}{\code{signature(drv = "pgSQLDriver", ...)}}
}}
\examples{
\dontrun{
# these are the defaults for user, password and dbname so they did not
# have to be specified here
con <- dbConnect(pgSQL(), user = "postgres", password = "", dbname = "test")
dbDisconnect(con)
}
}
\keyword{methods}

