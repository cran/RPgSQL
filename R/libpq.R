# -*- R -*-
# $RCSfile: libpq.R,v $
# $Date: 2000/12/12 23:21:56 $
# $Revision: 1.6 $
# Copyright (C) 1999 Timothy H. Keitt
# Licence: GPL
db.connect <- function(host=NULL, hostaddr=NULL, port=NULL,
                       dbname=NULL, user=NULL, password=NULL,
                       options=NULL, tty=NULL, verbose=T) {
  conninfo <- character(1)
  if (!is.null(host)) conninfo <- paste(conninfo, "host =", host)
  if (!is.null(hostaddr)) conninfo <- paste(conninfo, "hostaddr =", hostaddr)
  if (!is.null(port)) conninfo <- paste(conninfo, "port =", port)
  if (!is.null(dbname)) conninfo <- paste(conninfo, "dbname =", dbname)
  if (!is.null(user)) conninfo <- paste(conninfo, "user =", user)
  if (!is.null(password)) conninfo <- paste(conninfo, "password =", password)
  if (!is.null(options)) conninfo <- paste(conninfo, "options =", options)
  if (!is.null(tty)) conninfo <- paste(conninfo, "tty =", tty)
  .C("rpgsql_connect", conninfo)
  if (db.connection.open()) {
    if (verbose) {
      host.name <- db.host.name()
      if (host.name == "") host.name <- "localhost"
      cat("Connected to database", double.quote(db.name()),
          "on", double.quote(host.name), "\n")
    }
  } else {
    stop("Connection failed\n")
  }
  db.execute("SET DATESTYLE TO 'Postgres, US'");
  return(invisible())
}

db.disconnect <- function() {
  .C("rpgsql_connection_finish")
  return(invisible())
}

db.connection.status <- function()
  return(.C("rpgsql_connection_status", status=integer(1))$status)

db.connection.open <- function()
  return(db.connection.status() == 0)

db.error.message <- function()
  return(.C("rpgsql_error_message", message=character(1))$message)

db.name <- function()
  return(.C("rpgsql_db_name", name=character(1))$name)

db.host.name <- function()
  .C("rpgsql_host_name", name=character(1))$name

db.connection.options <- function()
  return(.C("rpgsql_db_options", options=character(1))$options)

db.user.name <- function()
  return(.C("rpgsql_user_name", name=character(1))$name)

db.password <- function()
  return(.C("rpgsql_password", password=character(1))$password)

db.connection.port <- function()
  return(.C("rpgsql_port", port=character(1))$port)

db.debug.tty <- function()
  return(.C("rpgsql_tty", tty=character(1))$tty)

db.execute <- function(..., clear=T, report.errors=T) {
  if (clear) on.exit(db.clear.result())
  .C("rpgsql_exec", paste(...))
  result.status <- db.result.status()
  if (report.errors) {
    if (result.status == 6) warning(db.error.message())
    if (result.status == 7) stop(db.error.message())
  }
  return(invisible(result.status))
}

db.clear.result <- function() {
  .C("rpgsql_clear_result")
  return(invisible())
}

db.result.columns <- function()
  return(.C("rpgsql_result_nfields", columns=integer(1))$columns)

db.result.rows <- function()
  return(.C("rpgsql_result_ntuples", rows=integer(1))$rows)

db.result.column.number <- function(col.name) {
  return(.C("rpgsql_result_fnumber", as.character(col.name),
            index=integer(1))$index + 1)
}

db.result.column.type <- function(col=1)
  return(.C("rpgsql_result_ftype", as.integer(col), type=integer(1))$type)

db.result.status <- function()
  return(.C("rpgsql_result_status", status=integer(1))$status)

db.result.column.names <- function() {
  names <- vector(mode="character")
  for (i in seq(from=1, length=db.result.columns()))
    names[i] <- .C("rpgsql_result_fname", as.integer(i),
                   name=character(1))$name
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

