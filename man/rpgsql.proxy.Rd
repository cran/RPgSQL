\name{Proxy interface}
\alias{bind.db.proxy}
\alias{$.db.proxy}
\alias{$<-.db.proxy}
\alias{$<-.db.proxy.writable}
\alias{[.db.proxy}
\alias{[<-.db.proxy}
\alias{[<-.db.proxy.writable}
\alias{[[.db.proxy}
\alias{[[<-.db.proxy}
\alias{[[<-.db.proxy.writable}
\alias{as.data.frame.db.proxy}
\alias{as.list.db.proxy}
\alias{as.matrix.db.proxy}
\alias{dim.db.proxy}
\alias{dimnames.db.proxy}
\alias{names.db.proxy}
\alias{print.db.proxy}
\alias{row.names.db.proxy}
\alias{summary.db.proxy}
\alias{db.has.row.names}
\alias{db.table.name}
\title{Bind a proxy object to a PostgreSQL table}
\usage{
bind.db.proxy(table.name, vacuum=F, writable=F)
row.names(proxy)
dimnames(proxy)
as.list(proxy)
as.matrix(proxy)
proxy[[row, col]]
proxy[row, col]
as.data.frame(proxy)
db.has.row.names(proxy)
names(proxy)
dim(proxy)
summary(proxy)
print(proxy)
db.table.name(proxy)
}
\arguments{
 \item{table.name}{A character string with the name of a table}
 \item{vacuum}{If true, update table statistics}
 \item{row}{A row index}
 \item{col}{A column index}
 \item{proxy}{A db.proxy object}
}
\description{
  \code{bind.db.proxy} returns a proxy object bound to a database
  table.\cr
  \code{row.names} returns a name for each row in the proxy frame.\cr
  \code{dimnames} returns the dimension names.\cr
  \code{as.list} converts a proxy frame to a list.\cr
  \code{as.matrix} converts a proxy frame to a matrix.\cr
  \code{[[.db.proxy} extracts columns, rows and subsets of the proxy
  frame.\cr
  \code{[.db.proxy} extracts columns, rows and subsets of the proxy
  frame.\cr
  \code{as.data.frame} convert proxy to data frame.\cr
  \code{db.has.row.names} were row names stored in the database
  table?\cr
  \code{names} get column names.\cr
  \code{dim} get frame dimensions.\cr
  \code{summary} summarize proxy frame.\cr
  \code{print} print some of the proxy frame.\cr
  \code{db.table.name} get the database table name from the proxy frame.
}
\details{
  Proxy object inherit from class "data.frame".  In theory, a proxy
  object should behave in every way like a data frame.  Unfortunately,
  this is difficult the way objects are handled internally by R.
}
\value{
  \code{bind.db.proxy} returns a proxy object.  No data, other than the
  table name, host and database name, are stored in the proxy object.
}
\author{Timothy H. Keitt}
\note{
  In order to make proxy objects work, I had to make
  \code{\link{row.names}} a generic object method.  This may break other
  packages.
}

\examples{
if (db.connection.open()) {
  data(airquality)
  rpgsql.test.data <- airquality
  rm(airquality)
  db.write.table(rpgsql.test.data, no.clobber=F)
  bind.db.proxy("rpgsql.test.data")
  summary(rpgsql.test.data)
  rpgsql.test.data
  rpgsql.test.data[50:55,]
  rpgsql.test.data[["Ozone"]]
  #This doesn't work
  rpgsql.test.data$Ozone
  #Clean up
  db.rm("rpgsql.test.data", ask=F)
}
}
\keyword{database}
