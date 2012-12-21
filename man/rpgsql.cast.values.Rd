\name{rpgsql.cast.values}
\alias{rpgsql.cast.values}
\alias{rpgsql.cast.values.default}
\title{Convert PgSQL output to R objects or types}
\usage{
rpgsql.cast.values(x)
rpgsql.cast.values.default(x)
}
\arguments{
 \item{x}{a vector of output from the PgSQL select command}
}
\description{
  \code{rpgsql.cast.values} is a generic function that converts PgSQL
  character output to R objects or types.
}
\details{
  See \code{\link{rpgsql.data.type}}.
}
\value{
  A vector of R objects or types.
}
\author{Timothy H. Keitt}
\seealso{\code{\link{rpgsql.data.type}},
  \code{\link{rpgsql.format.values}}}
\keyword{database}

