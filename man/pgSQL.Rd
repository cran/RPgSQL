\name{pgSQL}
\alias{pgSQL}
\title{
  pgSQL engine
}
\description{
  \code{pgSQL} creates a new DBI driver that can be used to start connections.
}
\usage{ pgSQL(driverClass='org.postgresql.Driver', classPath,
	identifier.quote="\"")
}
\arguments{
  \item{driverClass}{name of the Java class of the driver to
    load. If empty, it is assumed that corresponding drivers were
    loaded by other means.}
  \item{classPath}{class path that needs to be appended in order to
    load the desired driver. Usually it is the path and file to the JAR file
    containing the driver. If omitted and \code{getOption("RpgSQL_JAR")}
    is a path and file it will use that; otherwise, if 
    \code{Sys.getenv("RpgSQL_JAR")}
    is a path and file it will use that.  If neither of those are specified
    or if they are paths rather than path and file then it
    will try to find the appropriate file looking
	at directories listed in the R option \code{"RpgSQL.JAR"}, 
    the environment variable
	\code{RpgSQL_JAR}, the environment variable \code{CLASSPATH} and
    on Windows it will also look in 
	\code{file.path(getenv("PROGRAMFILES"), "PostgreSLQ\\pgJDBC")}
	while on other systems it will also look in
	\code{/usr/local/pgsql/share/java}.
}
  \item{identifier.quote}{character to use for quoting identifiers in
    automatically generated SQL statements or \code{NA} for no
    quoted identifiers.}    
}
\value{
  Returns a \code{pgSQLDriver} object that can be used in calls to
  \code{\link[DBI]{dbConnect}}.
}
\details{
  The \code{pgSQL} function initializes the Java VM, loads the pgSQL driver 
  and creates a proxy R object which can be used to a
  call \code{\link[DBI]{dbConnect}} which actually creates a connection.

}
\seealso{
  \code{\link[DBI]{dbConnect}}
}
\examples{
\dontrun{
# problem creating tables with lower case in the column names
library(RJDBC)

# the user/password/dbname used here are actually the defaults
con <- dbConnect(pgSQL(), user = "postgres", password = "", dbname = "test")

# create table, populate it and display it
s <- 'create table tt("id" int primary key, "name" varchar(255))'
dbSendUpdate(con, s)
dbSendUpdate(con, "insert into tt values(1, 'Hello')")
dbSendUpdate(con, "insert into tt values(2, 'World')")
dbGetQuery(con, "select * from tt")

# transfer a data frame to pgSQL and then display it from the database
# dbWriteTable is case sensitive
dbWriteTable(con, "BOD", BOD)

# table names are lower cased unless double quoted
dbGetQuery(con, 'select * from "BOD"')

dbDisconnect(con)
}
}
\keyword{interface}
