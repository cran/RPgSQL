# -*- R -*-
# $RCSfile: io.R,v $
# $Date: 2000/07/12 19:31:40 $
# $Revision: 1.3 $
# Copyright (C) 1999 Timothy H. Keitt
# Licence: GPL
db.ls <- function(pattern=NULL, all=F) {
  if (!db.connection.open()) stop("No database connection")
  query <- paste("SELECT relname FROM pg_class WHERE relkind='r'")
  if (!is.null(pattern))
    query <- paste(query, "AND relname ~", single.quote(pattern))
  if (!all)
    query <- paste(query, "AND NOT relname ~ '^pg_'")
  on.exit(db.clear.result())
  db.execute(query, clear=F)
  tables <- db.read.column(as.is=T)
  tables <- make.names(tables)
  return(tables)
}

db.rm <- function(..., pattern=NULL, ask=T) {
  table.list <- list(...)
  if (!is.null(pattern))
    table.list <- append(table.list, db.ls(pattern=pattern))
  for (i in seq(along=table.list)) {
    name <- make.names(table.list[[i]])
    if (db.table.exists(name)) {
      if (ask) {
        ans <- readline(paste(sep="", "Destroy table ", name, "? "))
        if (pmatch(ans, "y", nomatch=F))
          db.execute("DROP TABLE", double.quote(make.db.names(name)))
      } else {
        db.execute("DROP TABLE", double.quote(make.db.names(name)))
      }
    } else {
      warning(paste("Table", name, "does not exist in database"))
    }
  }
  return(invisible())
}

db.table.exists <- function(name) {
  name <- make.db.names(name)
  db.execute("SELECT relname FROM pg_class WHERE relname =",
             single.quote(name), clear=F)
  return(db.result.rows() > 0)
}

db.read.table <- function(name=NULL, row.names=NULL,
                          col.names=NULL, as.is=F) {
  if (is.null(name)) stop("You must provide a table name")
  on.exit(db.clear.result())
  db.execute("SELECT * FROM", format.table.name(name), clear=F)
  out <- db.fetch.result(row.names, col.names, as.is)
  return(out)
}

db.fetch.result <- function(row.names=NULL, col.names=NULL, as.is=F) {
  cols <- db.result.columns()
  data <- vector(mode="list", length=cols)
  # This is taken verbatim from read.table (v 1.1.0)
  if (is.logical(as.is)) {
    as.is <- rep(as.is, length = cols)
  }
  else if (is.numeric(as.is)) {
    if (any(as.is < 1 | as.is > cols)) 
      stop("invalid numeric as.is expression")
    i <- rep(FALSE, cols)
    i[as.is] <- TRUE
    as.is <- i
  }
  else if (length(as.is) != cols) 
    stop(paste("as.is has the wrong length", length(as.is), 
               "!= cols =", cols))
  # End borrowed code
  for (col in seq(1, length=cols))
    data[[col]] <- db.read.column(col, as.is=as.is[col])
  class(data) <- "data.frame"
  if (is.null(col.names)) {
    names(data) <- make.names(db.result.column.names(), unique=T)
  } else {
    names(data) <- make.names(col.names, unique=T)
  }
  if (is.null(row.names)) {
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
    column <- match(column, db.result.column.names())
  data.col <- vector(length=db.result.rows())
  for (row in seq(along=data.col))
    data.col[row] <- db.result.get.value(row, column)
  if (!as.is)
    class(data.col) <- as.character(db.result.column.type(col=column))
  data.col <- rpgsql.cast.values(data.col)
  return(data.col)
}

db.write.table <- function(data, name=deparse(substitute(data)),
                           no.clobber=T, write.row.names=F) {
  name <- make.db.names(name)
  if (no.clobber) {
    if (db.table.exists(name))
      stop(paste("Table", name, "already in database"))
  } else {
    if (db.table.exists(name)) db.rm(name, ask=F)
  }
  if (!inherits(data, "data.frame")) data <- data.frame(I(data))
  if (write.row.names) data$rpgsql.row.names <- row.names(data)
  query <- paste("CREATE TABLE", double.quote(name))
  column.names <- lapply(make.db.names(names(data)), double.quote)
  data.types <- lapply(data, rpgsql.data.type)
  table.columns <- vector(mode="character")
  for (col in 1:ncol(data)) {
    column.def <- paste(column.names[col], data.types[col])
    table.columns <- append(table.columns, column.def)
  }
  query <- paste(query, "(", list.to.csv(table.columns), ")")
  db.execute(query)
  sql.insert(into=name, values=data)
  db.execute("VACUUM ANALYZE", double.quote(name))
  return(invisible())
}









