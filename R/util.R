# -*- R -*-
# $RCSfile: util.R,v $
# $Date: 2000/07/12 19:31:40 $
# $Revision: 1.2 $
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

