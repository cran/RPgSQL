\name{db.result.get.value}
\alias{db.result.get.value}
\title{Fetch a value from returned tuples}
\usage{
db.result.get.value(row=1, col=1)
}
\arguments{
 \item{row}{The row number}
 \item{col}{The column number}
}
\description{
  \code{db.result.get.value()} returns values contained in tuples that resulted
  from the last query.
}
\value{
  The value of the tuple.
}
\author{Timothy H. Keitt}
\note{Row and column numbers are always 1-based}
\seealso{\code{\link{db.execute}}, \code{\link{db.result.rows}}, \code{\link{db.result.columns}}}

\examples{
if (db.connection.open()) {
  db.execute("select usename, usesysid from pg_user", clear=F);
  row <- db.result.rows();
  col <- db.result.columns();
  cat("Value at last row and column is",
    db.result.get.value(row=row, col=col), "\n")
  db.clear.result()
}
}
\keyword{database}
