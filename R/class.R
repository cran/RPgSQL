
setClass("pgSQLDriver", contains = "JDBCDriver")
setClass("pgSQLConnection", contains = "JDBCConnection")
setClass("pgSQLResult", contains = "JDBCResult")


pgSQL <- function(driverClass='org.postgresql.Driver', classPath,
	identifier.quote="\"") {

  if (missing(classPath)) classPath <- NULL

  ## if option RpgSQL.JAR or envir variable RpgSQL_JAR is a file set classPath

  RpgSQL_JAR <- getOption("RpgSQL.JAR")
  if (is.null(RpgSQL_JAR)) RpgSQL_JAR <- unname(Sys.getenv("RpgSQL_JAR"))
  if (identical(RpgSQL_JAR, "")) RpgSQL_JAR <- NULL
  if (!is.null(RpgSQL_JAR) && file.exists(RpgSQL_JAR) && !file.info(RpgSQL_JAR)$isdir) classPath <- RpgSQL_JAR

  if (is.null(classPath)) {

	## this is the path that the PostgreSQL JDBC jar file is searched along

	jar.search.path <- c(RpgSQL_JAR,
		".",
		Sys.getenv("CLASSPATH"), 
		Sys.getenv("PATH"), 
		if (.Platform$OS == "windows") {
			file.path(Sys.getenv("PROGRAMFILES"), "PostgreSQL\\pgJDBC")
			} else c("/usr/local/pgsql/share/java", "/opt/local/share/java"))

#	 find.file <- function(datapath, file) { 
#		out <- if (file == basename(file)) {
#			datapath <- unlist(strsplit(datapath, .Platform$path.sep))
#			unname(unlist(sapply(file.path(datapath, file), Sys.glob)))
#		} else {
#			if (file.exists(file)) file
#		}
#		head(out, 1)
#	 }


	 # x is path/file or path.  glob is matched to it and the last match
	 # is returned.
	 match.glob <- function(x, glob) {
		result <- if (file.exists(x)) {
			if (file.info(x)$isdir) Sys.glob(file.path(x, glob))
			else {
				b <- basename(Sys.glob(file.path(dirname(x), glob)))
				if (basename(x) %in% b) x
		    }
	    }
		tail(result, 1)
	 }

	 # find first occurrence of glob on datapath
	 find.file <- function(datapath, glob) {
		datapath <- unlist(strsplit(datapath, .Platform$path.sep))
		# strip trailing forward or backward slashes
		datapath <- gsub("[\\/]$", "", datapath)
		head(unlist(lapply(datapath, match.glob, glob = glob)), 1)
	 }

     classPath <- find.file(jar.search.path, "postgresql*.jdbc4.jar")

	 if (length(classPath) == 0) classPath <- 
		find.file(jar.search.path, "postgresql*.jar")

     if (length(classPath) == 0) {
        stop("Could not find Postgres JDBC driver on", jar.search.path)
      } else classPath <- normalizePath(classPath[1])

  }
  .jinit(classPath)
  if (nchar(driverClass) && is.jnull(.jfindClass(as.character(driverClass)[1])))
    stop("Cannot find PostgreSQL driver class ",driverClass)
  jdrv <- .jnew(driverClass, check=FALSE)
  .jcheck(TRUE)
  if (is.jnull(jdrv)) jdrv <- .jnull()
  new("pgSQLDriver", identifier.quote=as.character(identifier.quote), jdrv=jdrv)
}

setMethod("dbConnect", "pgSQLDriver", def=function(drv, 
  url = sprintf("jdbc:postgresql:%s:", dbname),
  user = getOption("RpgSQL.user"),
  password = getOption("RpgSQL.password"),
  dbname = getOption("RpgSQL.dbname"), ...) {
  if (is.null(user)) user <- "postgres"
  if (is.null(password)) password <- ""
  if (is.null(dbname)) dbname <- "test"
  jc <- .jcall("java/sql/DriverManager","Ljava/sql/Connection;","getConnection", as.character(url)[1], as.character(user)[1], as.character(password)[1], check=FALSE)
  if (is.jnull(jc) || !is.jnull(drv@jdrv)) {
    # ok one reason for this to fail is its interaction with rJava's
    # class loader. In that case we try to load the driver directly.
    oex <- .jgetEx(TRUE)
    p <- .jnew("java/util/Properties")
    if (length(user)==1 && nchar(user)) .jcall(p,"Ljava/lang/Object;","setProperty","user",user)
    if (length(password)==1 && nchar(password)) .jcall(p,"Ljava/lang/Object;","setProperty","password",password)
    jc <- .jcall(drv@jdrv, "Ljava/sql/Connection;", "connect", as.character(url)[1], p)
  }
  .verify.JDBC.result(jc, "Unable to connect JDBC to ",url)
  new("pgSQLConnection", jc=jc, identifier.quote=drv@identifier.quote)},
          valueClass="pgSQLConnection")

setMethod("dbWriteTable", "pgSQLConnection", def=function(conn, name, value, overwrite=TRUE, ...) {
  ac <- .jcall(conn@jc, "Z", "getAutoCommit")
  if (is.vector(value) && !is.list(value)) value <- data.frame(x=value)
  if (length(value)<1) stop("value must have at least one column")
  if (is.null(names(value))) names(value) <- paste("V",1:length(value),sep='')
  if (length(value[[1]])>0) {
    if (!is.data.frame(value)) value <- as.data.frame(value, row.names=1:length(value[[1]]))
  } else {
    if (!is.data.frame(value)) value <- as.data.frame(value)
  }
  fts <- sapply(value, dbDataType, dbObj=conn)
  if (dbExistsTable(conn, name)) {
    if (overwrite) dbRemoveTable(conn, name)
    else stop("Table `",name,"' already exists")
  }
  fdef <- paste(.sql.qescape(names(value), FALSE, conn@identifier.quote),fts,collapse=',')
  # cat("conn@identifier.quote:", conn@identifier.quote, "\n")
  # qname <- .sql.qescape(name, FALSE, conn@identifier.quote)
  qname <- .sql.qescape(name, TRUE, conn@identifier.quote)
  ct <- paste("CREATE TABLE ",qname," (",fdef,")",sep= '')
  # cat("ct:", ct, "\n")
  if (ac) {
    .jcall(conn@jc, "V", "setAutoCommit", FALSE)
    on.exit(.jcall(conn@jc, "V", "setAutoCommit", ac))
  }
  dbSendUpdate(conn, ct)
  if (length(value[[1]])) {
    inss <- paste("INSERT INTO ",qname," VALUES(", paste(rep("?",length(value)),collapse=','),")",sep='')
    for (j in 1:length(value[[1]]))
      dbSendUpdate(conn, inss, list=as.list(value[j,]))
  }
  if (ac) dbCommit(conn)            
})

setMethod("dbDataType", signature(dbObj="pgSQLConnection", obj = "ANY"),
          def = function(dbObj, obj, ...) {
            if (is.integer(obj)) "INTEGER"
            else if (is.numeric(obj)) "DOUBLE PRECISION"
            else if (inherits(obj, "Date")) "DATE"
			else if (inherits(obj, "POSIXct")) "TIMESTAMP WITH TIME ZONE"
            else "VARCHAR(255)"
          }, valueClass = "character")

setMethod("fetch", signature(res="pgSQLResult", n="numeric"), def=function(res, n, ...) {
  cols <- .jcall(res@md, "I", "getColumnCount")
  if (cols < 1) return(NULL)
  l <- list()
  for (i in 1:cols) {
    ct <- .jcall(res@md, "I", "getColumnType", i)
    l[[i]] <- if (ct == -5 | ct ==-6 | (ct >= 2 & ct <= 8)) { 
           numeric()
       } else if (ct == 91) { 
           structure(numeric(), class = "Date")
       } else if (ct == 93) { 
           structure(numeric(), class = class(Sys.time()))
       } else character()
    names(l)[i] <- .jcall(res@md, "S", "getColumnName", i)
  }

  j <- 0
  while (.jcall(res@jr, "Z", "next")) {
    j <- j + 1
    for (i in 1:cols) {
      l[[i]][j] <- if (is.numeric(l[[i]])) { 
          l[[i]][j] <- .jcall(res@jr, "D", "getDouble", i)
      } else if (inherits(l[[i]], "Date")) {
        l[[i]][j] <- as.Date(.jcall(res@jr, "S", "getString", i))
      } else {
        a <- .jcall(res@jr, "S", "getString", i)
        l[[i]][j] <- if (is.null(a)) NA else a
      }
    }
    if (n > 0 && j >= n) break
  }
  if (j)
    as.data.frame(l, row.names=1:j)
  else
    as.data.frame(l)
})

setMethod("dbSendQuery", signature(conn="pgSQLConnection", statement="character"),  def=function(conn, statement, ..., list=NULL) {
  s <- .jcall(conn@jc, "Ljava/sql/PreparedStatement;", "prepareStatement", as.character(statement)[1], check=FALSE)
  .verify.JDBC.result(s, "Unable to execute JDBC statement ",statement)
  if (length(list(...))) .fillStatementParameters(s, list(...))
  if (!is.null(list)) .fillStatementParameters(s, list)
  r <- .jcall(s, "Ljava/sql/ResultSet;", "executeQuery", check=FALSE)
  .verify.JDBC.result(r, "Unable to retrieve JDBC result set for ",statement)
  md <- .jcall(r, "Ljava/sql/ResultSetMetaData;", "getMetaData", check=FALSE)
  .verify.JDBC.result(md, "Unable to retrieve JDBC result set meta data for ",statement, " in dbSendQuery")
  new("pgSQLResult", jr=r, md=md)
})

### this should really be in RJDBC
setMethod("dbClearResult", "JDBCResult",
          def = function(res, ...) { .jcall(res@jr, "V", "close"); TRUE},
          valueClass = "logical")

### this should really be in RJDBC
setMethod("dbHasCompleted", "JDBCResult",
          def = function(res, ...) { .jcall(res@jr, "Z", "isLast") ||
			  .jcall(res@jr, "Z", "isAfterLast") },
          valueClass = "logical")


