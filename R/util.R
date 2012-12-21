# -*- R -*-
# $RCSfile: util.R,v $
# $Date: 2000/01/08 20:38:33 $
# $Revision: 3.0 $
# Copyright (C) 1999 Timothy H. Keitt
convert.table.name <- function(table.name) {
  if (inherits(table.name, "db.proxy"))
    name <- to.db.name(db.table.name(table.name))
  else
    name <- to.db.name(as.character(table.name))
  name <- double.quote(name)
}

check.table.name <- function(table.name) {
  name <- convert.table.name(table.name)
  if (!db.table.exists(name)) stop(paste(name, "is not in database"))
  return(name)
}

to.db.name <- function(name)
  return(gsub("\\.", "_", as.character(name)))

to.R.name <- function(name)
  return(gsub("_", "\\.", as.character(name)))

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

