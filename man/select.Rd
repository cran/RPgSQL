\name{select}
\alias{select}
\title{Format and optionally execute an SQL select query}
\usage{
select(columns="*", into, from, where, group.by, having, order.by, limit, distinct=F, exec=T)
}
\arguments{
 \item{columns}{A list of column names}
 \item{into}{Put results into a new table with this name}
 \item{from}{Table name to select from}
 \item{where}{A logical expression}
 \item{group.by}{Group results by these columns}
 \item{having}{I'm not sure what this does}
 \item{order.by}{Sort results by these columns}
 \item{limit}{Only return this many rows}
 \item{offset}{Return rows beginning here (zero-based)}
 \item{distinct}{If true, only distinct results are returned}
 \item{exec}{If true, execute the query and return the results}
}
\description{
  \code{select} is a wrapper for the SQL SELECT command.
}
\details{
  Table joins are impossible in \code{select}.  You must use
  \code{\link{db.execute}} and hand edit the query to do a join.
  \code{select} is meant to save a few key strokes and perhaps avoid a
  few typing errors.
}
\value{
  If \code{exec} is true, a data frame containing the results of the
  query.  If \code{exec} is false, the formatted query string.
}
\author{Timothy H. Keitt}
\seealso{\code{\link{db.execute}}}

\examples{
select("usename", from="pg_user", where="usename = 'postgres'")
}
\keyword{database}
