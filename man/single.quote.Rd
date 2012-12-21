\name{single.quote}
\alias{single.quote}
\title{convert arguments to a single quoted string}
\usage{
single.quote(...)
}
\arguments{
 \item{\dots}{All arguments are passed to paste}
}
\description{
  Many SQL commands require string values to be surrounded by single
  quotes.  Multiple input arguments will be separated by a space.  The
  entire output string is single quoted, not the individual items.
}
\value{
  A single quoted string
}
\author{Timothy H. Keitt}
\keyword{database}
