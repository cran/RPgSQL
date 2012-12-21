\name{rpgsql.format.values}
\alias{rpgsql.format.values}
\alias{rpgsql.format.values.default}
\title{Stringify R data into PgSQL compatible form}
\usage{
rpgsql.format.values(x)
rpgsql.format.values.default(x)
}
\arguments{
 \item{x}{a vector of some R class or data type}
}
\description{
  \code{rpgsql.format.values} is a generic function that returns
  appropriate formatted strings compatible with the PgSQL insert
  command.
}
\details{
  See \code{\link{rpgsql.data.type}}.
}
\value{
  A vector of string ready to insert into a PgSQL table.
}
\author{Timothy H. Keitt}
\seealso{\code{\link{rpgsql.data.type}},
  \code{\link{rpgsql.cast.values}}}
\keyword{database}
