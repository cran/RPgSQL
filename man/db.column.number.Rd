\name{db.column.number}
\alias{db.column.number}
\title{Return the number of a named column}
\usage{
column.number <- db.column.number(col.name)
}
\arguments{
 \item{col.name}{A character string containing the column name}
}
\description{
  Returns the number of the named column in that resulted from the last
  query.
}
\details{
  Column numbers always begin at 1.
}
\value{
  \item{column.number}{The column number}
}
\author{Timothy H. Keitt}
\seealso{\code{\link{db.result.columns}}}
\keyword{database}
