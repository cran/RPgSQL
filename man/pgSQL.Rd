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

  Note that these options can be defined: \code{"RpgSQL.url"}, 
  \code{"RpgSQL.host"}, \code{"RpgSQL.port"}, \code{"RpgSQL.dbname"}
  as illustrated in the examples below.  If \code{"RpgSQL.url"} is
  defined it takes precedence over the others.  
  If the \code{dbConnect} method can explicitly specify any of these then
  the explicitly specified ones override the options.

  Also the option
  \code{"RpgSQL.JAR"} can be defined as the path to the PostgreSQL
  jar file.  It can include the filename or just be a directory in which
  case it will attempt to find the jar file in that directory.  It can
  also be a glob expression in which case it will look among those.
  The jar file location can alternately be specified in an environment
  variable \code{RPGSQL_JAR} which is only used if the option is not
  specified.  Also the \code{pgSQL} function can specify the jar file
  location in which case the option and environment variable are ignored.

}
\seealso{
  \code{\link[DBI]{dbConnect}}
}
\examples{
\dontrun{
# problem creating tables with lower case in the column names
library(RJDBC)

# Can put something like this into .Rprofile if you wish to change the
# JVM parameters.  The value here is the default:
#   options(java.parameters = "-Xmx512m")

# Can put something like this into .Rprofile so
# user and password need not be specified in every session.
# The values in this example are actually the defaults.
#   options(RpgSQL.user = "postgres", RpgSQL.password= "", 
#	 RpgSQL.dbname = "test", RpgSQL.host = "localhost", RpgSQL.port = 5432)
# or just:
#   options(RpgSQL.url = "jdbc:postgresql://localhost:5432/test")

# the user/password/dbname used here are actually the defaults
con <- dbConnect(pgSQL(), user = "postgres", password = "", dbname = "test",
	host = "localhost", port = 5432)

# show databases
dbGetQuery(con, "select datname from pg_database")

# show tables
tabs <- dbGetQuery(con, "SELECT table_name FROM information_schema.tables WHERE table_schema = 'public'")
tabs

# If DF already exists drop it first
# if ("DF" %in% tabs[[1]]) dbSendUpdate(con, "drop table DF")

# create DF and populate it - DF actually stored as df
# This example requires RpgSQL 0.1-4.1 or later.
ct <- 'CREATE TABLE DF ("a" INTEGER, "b" VARCHAR(255), "c" DATE,"d" DOUBLE PRECISION)'
dbSendUpdate(con, ct)
dbSendUpdate(con, "insert into DF values(1, 'Hello', to_date('2000-01-01', 'YYYY-MM-DD'), 32)")
dbSendUpdate(con, "insert into DF values(2, 'Hello', to_date(NULL, 'YYYY-MM-DD'), 33)")

# query DF
dbGetQuery(con, "select * from DF")
res <- dbSendQuery(con, "select * from DF")
dbHasCompleted(res)
fetch(res, 1)
dbHasCompleted(res)
dbClearResult(res)

# cleanup - delete the table we created
dbSendUpdate(con, "drop table DF")

# create table, populate it, display it and delete it
s <- 'create table tt("id" int primary key, "name" varchar(255))'
dbSendUpdate(con, s)
dbSendUpdate(con, "insert into tt values(1, 'Hello')")
dbSendUpdate(con, "insert into tt values(2, 'World')")
dbGetQuery(con, "select * from tt")
dbSendUpdate(con, "drop table tt")

# transfer a data frame to pgSQL and then display it from the database
# dbWriteTable is case sensitive
dbWriteTable(con, "BOD", BOD)

# table names are lower cased unless double quoted
dbGetQuery(con, 'select * from "BOD"')
dbSendUpdate(con, 'drop table "BOD"')

dbDisconnect(con)
}
}
\keyword{interface}
