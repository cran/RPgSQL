\name{pgSQLResult-class}
\docType{class}
\alias{pgSQLResult-class}

\title{Class "pgSQLResult"}
\description{Representation of a DBI result set returned from a pgSQL connection.}
\section{Generators}{
  Objects can be created by call to \code{\link[DBI]{dbSendQuery}}.
}
\section{Slots}{
  \describe{
    \item{\code{jr}:}{Java reference to the pgSQL result set}
    \item{\code{md}:}{Java reference to the pgSQL result set meta data}
  }
}
\section{Extends}{
Class \code{"\link[DBI]{DBIResult-class}"}, directly.
}
\section{Methods}{
No methods defined with class "pgSQLResult" in the signature.
}
\seealso{
  \code{\link{pgSQL}}, \code{\link[DBI]{dbSendQuery}}
}
%\examples{
%}
\keyword{classes}
