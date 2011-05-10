\name{pgSQLResult-methods}
\docType{methods}
\alias{pgSQLResult-methods}
\alias{fetch,pgSQLResult,numeric-method}
\title{Methods for the class pgSQLResult in Package `pgSQL' }
\description{
  Methods for the class `pgSQLResult' in Package `pgSQL'.

  \code{fetch} retrieves the content of the result set in the form of a
  data frame. 
 
  Other methods are inherited from the RJDBC package.  Note that \code{fetch}
  returns a data frame in which strings are converted to factors unless the
  optional \code{stringsToFactors} argument is \code{FALSE}.

  }
\section{Methods}{
  \describe{
    \item{fetch}{\code{signature(res = "pgSQLResult", ...)}}
}}
\keyword{methods}

