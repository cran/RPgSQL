\name{db.rm}
\alias{db.rm}
\title{Destroy postgres tables}
\usage{
db.rm(..., pattern, ask=T)
}
\arguments{
 \item{\dots}{A list of table names}
 \item{pattern}{A matching pattern passed to \code{\link{db.ls}}}
 \item{ask}{A boolean indicating whether to prompt before destroying a table}
}
\description{
  \code{db.rm} removes tables from a database.
}
\details{
  \code{db.rm(ask=F)} will delete ALL of your tables!
}
\author{Timothy H. Keitt}
\note{\code{db.rm(ask=F)} will delete ALL of your tables!}

\seealso{\code{\link{db.ls}}}

\keyword{database}
