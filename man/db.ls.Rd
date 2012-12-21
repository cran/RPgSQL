\name{db.ls}
\alias{db.ls}
\title{List database tables}
\usage{
db.ls(pattern, all=F)
}
\arguments{
  \item{pattern}{A matching pattern or table name}
  \item{all}{A boolean indicating whether to list all tables or only
    user tables}
}
\description{
  \code{db.ls} lists the database table names in the current database.
}
\value{
  A list of table names
}
\author{Timothy H. Keitt}
\note{By default, only tables that belong to the current postgres user
  id are listed.  This might cause problems when accessing databases
  that are owned by another user.}
\seealso{\code{\link{ls}}, \code{\link{db.rm}}}

\examples{
db.ls(all=T)
}
\keyword{database}
