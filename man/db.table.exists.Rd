\name{db.table.exists}
\alias{db.table.exists}
\title{Return true if table name exists in database}
\usage{
db.table.exists(name)
}
\arguments{
 \item{name}{The name of a postgres table}
}
\description{
  \code{db.table.exists} returns true if the provided table name exists
  in the current database
}
\value{
  A boolean indicating the table exists
}
\author{Timothy H. Keitt}
\seealso{\code{\link{db.ls}}}
\keyword{database}
