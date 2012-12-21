# -*- R -*-
# $RCSfile: libpq.R,v $
# $Date: 2000/01/08 20:38:33 $
# $Revision: 3.0 $
# Copyright (C) 1999 Timothy H. Keitt
# Licence: GPL
db.connect <- function(host, port, dbname, user, password,
                       authtype, options, tty, verbose=T) {
  conninfo <- character(1)
  if (!missing(host)) conninfo <- paste(conninfo, "host =", host)
  if (!missing(port)) conninfo <- paste(conninfo, "port =", port)
  if (!missing(dbname)) conninfo <- paste(conninfo, "dbname =", dbname)
  if (!missing(user)) conninfo <- paste(conninfo, "user =", user)
  if (!missing(password)) conninfo <- paste(conninfo, "password =", password)
  if (!missing(authtype)) conninfo <- paste(conninfo, "authtype =", authtype)
  if (!missing(options)) conninfo <- paste(conninfo, "options =", options)
  if (!missing(tty)) conninfo <- paste(conninfo, "tty =", tty)
  .C("rpgsql_connect", conninfo)
  if (db.connection.status() != 0) {
    stop("Connection failed\n")
  } else {
    if (verbose) {
      cat("Connected to database", double.quote(db.name()),
          "on", double.quote(db.host.name()), "\n")
    }
  }
  return(invisible())
}

db.disconnect <- function() {
  .C("rpgsql_finish")
  return(invisible())
}

db.connection.status <- function()
  return(.C("rpgsql_status", status=integer(1))$status)

db.error.message <- function()
  return(.C("rpgsql_error_message", message=character(1))$message)

db.name <- function()
  return(.C("rpgsql_db_name", name=character(1))$name)

db.host.name <- function() {
  name <- .C("rpgsql_host_name", name=character(1))$name
  if (name == "")
    name <- system("hostname", intern=T, ignore.stderr=T)
  return(name)
}

db.execute <- function(..., clear=T) {
  if (clear) on.exit(db.clear.result())
  .C("rpgsql_exec", paste(...))
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

db.column.number <- function(col.name) {
  return(.C("rpgsql_fnumber", as.character(col.name),
            index=integer(1))$index + 1)
}

db.column.type <- function(col=1)
  return(.C("rpgsql_ftype", as.integer(col), type=integer(1))$type)

db.result.status <- function()
  return(.C("rpgsql_result_status", status=integer(1))$status)

db.column.names <- function() {
  names <- vector(mode="character")
  for (i in seq(from=1, length=db.result.columns()))
    names[i] <- .C("rpgsql_fname", as.integer(i), name=character(1))$name
  return(names)
}

db.get.value <- function(row=1, col=1) {
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
