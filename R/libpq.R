# -*- R -*-
# $RCSfile: libpq.R,v $
# $Date: 2000/07/12 19:31:40 $
# $Revision: 1.3 $
# Copyright (C) 1999 Timothy H. Keitt
# Licence: GPL
db.connect <- function(host=NULL, port=NULL, dbname=NULL, user=NULL,
                       password=NULL, authtype=NULL, options=NULL,
                       tty=NULL, verbose=T) {
  conninfo <- character(1)
  if (!is.null(host)) conninfo <- paste(conninfo, "host =", host)
  if (!is.null(port)) conninfo <- paste(conninfo, "port =", port)
  if (!is.null(dbname)) conninfo <- paste(conninfo, "dbname =", dbname)
  if (!is.null(user)) conninfo <- paste(conninfo, "user =", user)
  if (!is.null(password)) conninfo <- paste(conninfo, "password =", password)
  if (!is.null(authtype)) conninfo <- paste(conninfo, "authtype =", authtype)
  if (!is.null(options)) conninfo <- paste(conninfo, "options =", options)
  if (!is.null(tty)) conninfo <- paste(conninfo, "tty =", tty)
  .C("rpgsql_connect", conninfo)
  if (db.connection.open()) {
    if (verbose) {
      cat("Connected to database", double.quote(db.name()),
          "on", double.quote(db.host.name()), "\n")
    }
  } else {
    stop("Connection failed\n")
  }
  db.execute("SET DATESTYLE TO 'Postgres, US'");
  return(invisible())
}

db.disconnect <- function() {
  .C("rpgsql_finish")
  return(invisible())
}

db.connection.status <- function()
  return(.C("rpgsql_status", status=integer(1))$status)

db.connection.open <- function()
  return(db.connection.status() == 0)

db.error.message <- function()
  return(.C("rpgsql_error_message", message=character(1))$message)

db.name <- function()
  return(.C("rpgsql_db_name", name=character(1))$name)

db.host.name <- function() {
  name <- .C("rpgsql_host_name", name=character(1))$name
  if (name == "")
    name <- "localhost"
  return(name)
}

db.execute <- function(..., clear=T, report.errors=T) {
  if (clear) on.exit(db.clear.result())
  .C("rpgsql_exec", paste(...))
  result.status <- db.result.status()
  if (report.errors) {
    if (result.status == 6) warning(db.error.message())
    if (result.status == 7) stop(db.error.message())
  }
  return(invisible())
}

db.clear.result <- function() {
  .C("rpgsql_clear_result")
  return(invisible())
}

db.result.columns <- function()
  return(.C("rpgsql_nfields", columns=integer(1))$columns)

db.result.rows <- function()
  return(.C("rpgsql_ntuples", rows=integer(1))$rows)

db.result.column.number <- function(col.name) {
  return(.C("rpgsql_fnumber", as.character(col.name),
            index=integer(1))$index + 1)
}

db.result.column.type <- function(col=1)
  return(.C("rpgsql_ftype", as.integer(col), type=integer(1))$type)

db.result.status <- function()
  return(.C("rpgsql_result_status", status=integer(1))$status)

db.result.column.names <- function() {
  names <- vector(mode="character")
  for (i in seq(from=1, length=db.result.columns()))
    names[i] <- .C("rpgsql_fname", as.integer(i), name=character(1))$name
  return(names)
}

db.result.get.value <- function(row=1, col=1) {
  null.value.flag <- .C("rpgsql_get_is_null", as.integer(row),
                        as.integer(col), flag=integer(1))$flag
  if (null.value.flag == 0)
    return(.C("rpgsql_get_value", as.integer(row), as.integer(col),
              value=character(1))$value)
  else
    return(NA)
}

db.toggle.echo <- function() {
  .C("rpgsql_toggle_echo")
  return(invisible())
}

