\name{db.get.value}
\alias{db.get.value}
\title{Fetch values from returned tuples}
\usage{
db.get.value(row=1, col=1)
}
\arguments{
 \item{row}{The row number}
 \item{col}{The column number}
}
\description{
  \code{db.get.value()} returns values contained in tuples that resulted
  from the last query.
}
\value{
  The value of the tuple.
}
\author{Timothy H. Keitt}
\note{Row and column numbers are always 1-based}
\seealso{\code{\link{db.execute}},\code{\link{db.result.rows}}
  \code{\link{db.result.columns}}}

\examples{
db.execute("select usename, usesysid from pg_user", clear=F);
row <- db.result.rows();
col <- db.result.columns();
cat(db.get.value(row=row, col=col))
db.clear.result()
}
\keyword{database}
