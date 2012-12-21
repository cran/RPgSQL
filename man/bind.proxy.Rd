\name{bind.proxy}
\alias{bind.proxy}
\title{Bind a proxy object to a PostgreSQL table}
\usage{
bind.proxy(table.name, vacuum=F)
}
\arguments{
 \item{table.name}{A character string with the name of a table}
 \item{vacuum}{If true, update postgres' table stats}
}
\description{
  \code{bind.proxy} returns a proxy object bound to a database table
}
\details{
  Proxy object inherit from class "data.frame".  In theory, a proxy
  object should behave in every way like a data frame.  Unfortunately,
  this is difficult the way objects are handled internally by R.
}
\value{
  A proxy object.  No data, other than the table name, host and database
  name, are stored in the proxy object.
}
\author{Timothy H. Keitt}
\note{
  Removing a proxy object does not remove the table that it was bound
  to.  Bug or feature?

  In order to make proxy objects work, I had to redefine the
  \code{\link{row.names}} function.  This may break some other
  packages.
}

\examples{
#Get some data
data(airquality)
db.write.table(airquality, no.clobber=F)
bind.proxy("airquality")
summary(airquality)
airquality
airquality[50:55,]
airquality[["Ozone"]]
#This doesn't work
airquality$Ozone
#Clean up
rm(airquality)
db.rm("airquality", ask=F)
}
\keyword{database}
