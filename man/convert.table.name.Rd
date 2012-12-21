\name{convert.table.name}
\alias{convert.table.name}
\title{table name conversion utility}
\usage{
convert.table.name(table.name)
}
\arguments{
 \item{table.name}{A character string containing the name of a postgres
   table or a proxy object pointing to a postgres table.}
}
\description{
  Converts R names with dot seperators to postgres friendly names with
  underscore separators.
}
\details{
  If a db.proxy object is passed to \code{convert.table.name}, the
  postgres table name will be extracted from the proxy and returned.
}
\value{
  A postgres friendly version of the table name.
}
\author{Timothy H. Keitt}
\seealso{\code{\link{check.table.name}}, \code{\link{db.table.name}}}
\keyword{database}
