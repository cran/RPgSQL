\name{db.column.type}
\alias{db.column.type}
\title{Returns the data type code for a given column}
\usage{
data.code <- db.column.type(col=1)
}
\arguments{
 \item{col}{The column number.}
}
\description{
  Returns the data type code for a given column.
}
\details{
  Column number always begin at 1.
}
\value{
  \item{data.code}{The data type code.}
}
\references{\url{http://www.postgresql.org/}}
\author{Timothy H. Keitt}
\note{Check the current libpq documents for a listing of code values.}
\seealso{\code{\link{db.column.number}}}
\keyword{database}
