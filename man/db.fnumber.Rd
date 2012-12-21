\name{db.fnumber}
\alias{db.fnumber}
\title{Return the number of a named column}
\usage{
column.number <- db.fnumber(col.name)
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
\seealso{\code{\link{db.columns}}}
\keyword{database}
