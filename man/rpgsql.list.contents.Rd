\name{List database tables}
\alias{db.ls}
\alias{db.rm}
\alias{db.table.exists}
\title{list database tables}
\usage{
db.ls(pattern, all=F)
db.rm(..., pattern, ask=T)
db.table.exists(name)
}
\arguments{
  \item{pattern}{A matching pattern or table name}
  \item{all}{A boolean indicating whether to list system tables in
    addition to user tables}
  \item{...}{One or more character strings that specify the tables to
    drop from the database}
  \item{ask}{A boolean indicating whether to ask for user approval
    before dropping each table}
  \item{name}{A character string containing a table name}
}
\description{
  \code{db.ls} lists the database table names in the current database.
  \code{db.rm} drops database tables.  Use carefully!
  \code{db.table.exists} check if a table name exists in the current
  database.
}
\value{
  \item{db.ls}{A list of table names}
  \item{db.table.exists}{A boolean}
}
\author{Timothy H. Keitt}
\note{The pattern can be any valid PostgreSQL regular expression, so
  \code{db.rm(pattern='.*', ask=F)} will remove all of your database
  tables.  You have been warned.}
\seealso{\code{\link{ls}}, \code{\link{rm}}}

\examples{
if (db.connection.open()) {
  db.ls(all=T)
  data(OrchardSprays)
  rpgsql.test.data <- OrchardSprays
  rm(OrchardSprays)
  db.write.table(rpgsql.test.data, no.clobber=F)
  if (db.table.exists('rpgsql_test_data')) {
    cat(db.ls(pattern='rpgsql'), "\n")
    db.rm('rpgsql_test_data', ask=F)
    cat(db.ls(pattern='test_data'), "\n")
  }
  rm(rpgsql.test.data)
}
}
\keyword{database}
