# -*- R -*-
# $RCSfile: util.R,v $
# $Date: 2001/06/20 19:46:09 $
# $Revision: 1.5 $
# Copyright (C) 1999 Timothy H. Keitt
format.table.name <- function(table.name) {
  if (inherits(table.name, "db.proxy"))
    name <- make.db.names(db.table.name(table.name))
  else
    name <- make.db.names(as.character(table.name))
  name <- double.quote(name)
}

check.table.name <- function(table.name) {
  name <- format.table.name(table.name)
  if (!db.table.exists(name)) stop(paste(name, "is not in database"))
  return(name)
}

make.db.names <- function(name)
  return(gsub("\\.", "_", as.character(name)))

list.to.csv <- function(...)
  return(paste(collapse=", ", ...))

single.quote <- function(...) {
  text <- gsub("'", "\\\\'", as.character(...))
  return(paste("'", unlist(text), "'", sep = ""))
}
         
double.quote <- function(...)
  return(paste('"', unlist(as.character(...)), '"', sep=""))

format.null.values <- function(...) {
  pattern <- "' *NA'|' *NaN'|' *Inf'|' *-Inf'"
  return(gsub(pattern, "NULL", as.character(...)))
}

psql <- function(dbname=NULL, host=NULL, port=NULL) {
  if (db.connection.open()) {
    if (is.null(dbname)) dbname <- db.name()
    if (is.null(host)) host <- db.host.name()
    if (is.null(port)) port <- db.connection.port()
  }
  command <- "psql"
  if (!is.null(dbname)) command <- paste(command, "-d", dbname)
  if (!is.null(host) && length(host) > 1)
    command <- paste(command, "-h", host)
  if (!is.null(port)) command <- paste(command, "-p", port)
  system(command)
}
