# -*- R -*-
# $RCSfile: io.R,v $
# $Date: 2000/01/08 20:38:33 $
# $Revision: 3.0 $
# Copyright (C) 1999 Timothy H. Keitt
# Licence: GPL
db.ls <- function(pattern, all=F) {
  on.exit(db.clear.result())
  if (all) {
    query <- paste("SELECT relname FROM pg_class")
    if (!missing(pattern))
      query <- paste(query, "WHERE relname ~", single.quote(pattern))
  } else {
    query <- paste("SELECT datdba FROM pg_database WHERE datname =",
                   single.quote(db.name()))
    db.execute(query, clear=F)
    uid <- db.get.value()
    query <- paste("SELECT relname FROM pg_class",
                   "WHERE relowner = ", uid,
                   "AND relkind = 'r'")
    if (!missing(pattern))
      query <- paste(query, "AND relname ~", single.quote(pattern))
  }
  db.execute(query, clear=F)
  tables <- vector(mode="character")
  if (db.result.status() != 2) return(tables)
  for (i in seq(from=1, length=db.result.rows()))
    tables[i] <- to.R.name(db.get.value(i))
  return(tables)
}

db.rm <- function(..., pattern, ask=T) {
  table.list <- list(...)
  if (!missing(pattern))
    table.list <- append(table.list, db.ls(pattern=pattern))
  for (i in seq(along=table.list)) {
    name <- to.R.name(table.list[[i]])
    if (db.table.exists(name)) {
      if (ask) {
        ans <- readline(paste(sep="", "Destroy table ", name, "? "))
        if (pmatch(ans, "y", nomatch=F))
          db.execute("DROP TABLE", double.quote(to.db.name(name)))
      } else {
        db.execute("DROP TABLE", double.quote(to.db.name(name)))
      }
    } else {
      warning(paste("Table", name, "does not exist in database"))
    }
  }
  return(invisible())
}

db.table.exists <- function(name) {
  name <- to.db.name(name)
  table.list <- db.ls(pattern=paste(sep="", "^", name, "$"), all=T)
  return(length(table.list) > 0)
}

db.read.table <- function(name, row.names) {
  on.exit(db.clear.result())
  if (!missing(name))
    db.execute("SELECT * FROM", convert.table.name(name), clear=F)
  if (db.result.status() != 2) return(NULL)
  data <- vector(mode="list", length=db.result.columns())
  for (col in seq(1, length=db.result.columns()))
    data[[col]] <- db.read.column(col)
  class(data) <- "data.frame"
  names(data) <- to.R.name(db.column.names())
  if (missing(row.names)) {
    if ("rpgsql.row.names" %in% names(data)) {
      row.names(data) <- data$rpgsql.row.names
      data$rpgsql.row.names <- NULL
    } else {
      row.names(data) <-
        as.character(seq(from=1, length=db.result.rows()))
    }
  } else {
    row.names(data) <- as.character(row.names)
  }
  return(data)
}

db.read.column <- function(column=1, as.is=F) {
  if (is.character(column))
    column <- match(column, db.column.names())
  if (is.na(column) | column < 1 | column > db.result.columns())
    stop("Invalid column\n")
  data.col <- vector(length=db.result.rows())
  for (row in seq(along=data.col))
    data.col[row] <- db.get.value(row, column)
  if (!as.is)
    class(data.col) <- as.character(db.column.type(col=column))
  data.col <- rpgsql.cast.values(data.col)
  return(data.col)
}

db.write.table <- function(data, name=deparse(substitute(data)),
                           no.clobber=T, write.row.names=F) {
  name <- to.db.name(name)
  if (no.clobber) {
    if (db.table.exists(name))
      stop(paste("Table", name, "already in database"))
  } else {
    if (db.table.exists(name)) db.rm(name, ask=F)
  }
  if (!inherits(data, "data.frame")) data <- data.frame(I(data))
  if (write.row.names) data$rpgsql.row.names <- row.names(data)
  query <- paste("CREATE TABLE", double.quote(name))
  column.names <- lapply(to.db.name(names(data)), double.quote)
  data.types <- lapply(data, rpgsql.data.type)
  table.columns <- vector(mode="character")
  for (col in 1:ncol(data)) {
    column.def <- paste(column.names[col], data.types[col])
    table.columns <- append(table.columns, column.def)
  }
  query <- paste(query, "(", list.to.csv(table.columns), ")")
  db.execute(query)
  insert(into=name, values=data)
  db.execute("VACUUM ANALYZE", double.quote(name))
  return(invisible())
}









