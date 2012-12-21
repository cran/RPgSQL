\name{db.column.names}
\alias{db.column.names}
\title{Return column names}
\usage{
column.names <- db.column.names()
}
\description{
  Return the column labels that result from the last executed query.
}
\value{
  The column names.
}
\author{Timothy H. Keitt}
\seealso{\code{\link{db.execute}}
\examples{
db.execute('select * from pg_user', clear=F);
cat(db.column.names())
db.clear.result()
}
\keyword{database}
