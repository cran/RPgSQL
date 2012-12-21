\name{insert}
\alias{insert}
\title{Append data to a postgres table}
\usage{
insert(into, column.names, values, query)
}
%- maybe also `usage' for other objects documented here.
\arguments{
 \item{into}{A character string with the table name}
 \item{column.names}{An optional list of column names}
 \item{values}{Data values, matrix or data frame to insert from}
 \item{query}{An SQL query}
}
\description{
  \code{insert} is quite useful for appending data to database tables.
}
\details{
  The easiest way to use \code{insert} is to use the \code{into} and
  \code{values} arguments.  Enter the table name in the \code{into}
  argument and set \code{values} to a data frame.  \code{insert} will
  extract the column names from the data frame and update those
  columns.  See example below.
}
\author{Timothy H. Keitt}
\seealso{\code{\link{db.write.table}}}

\examples{
data(airquality)
misc.data <- airquality[1:5,]
db.write.table(misc.data, no.clobber=F)
db.read.table("misc.data")
misc.data <- airquality[6:10,]
insert(into="misc.data", values=misc.data)
db.read.table("misc.data")
db.rm("misc.data", ask=F)
}
\keyword{database}
