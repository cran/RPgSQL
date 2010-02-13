\name{pgSQLConnection-class}
\docType{class}
\alias{pgSQLConnection-class}

\title{Class "pgSQLConnection"}
\description{Class representing a (DBI) database connection which uses
  pgSQL to connect to a database.}
\section{Generators}{
  Objects can be created by call to \code{\link[DBI]{dbConnect}} of a
  \code{\link{JDBC}} driver.
}
\section{Slots}{
	 \describe{
	   \item{\code{jc}:}{Java object representing the connection.}
	   \item{\code{identifier.quote}:}{Quote character to use for
	     quoting identifiers in automatically generated SQL
	     statements or \code{NA} for no such quoting.
	     Usually the value is inherited from the
	     \code{"\linkS4class{pgSQLDriver}"}.}
	 }
       }
       
\section{Extends}{
  Class \code{"\link[RJDBC]{JDBCConnection-class}"}, directly.
}
\section{Methods}{
  No methods defined with class "pgSQLConnection" in the signature.
}
%\author{ ~~who you are~~ }
\seealso{
  \code{\link{JDBC}}, \code{"\linkS4class{JDBCDriver}"}
}
%\examples{
%}
\keyword{classes}
