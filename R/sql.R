# -*- R -*-
# $RCSfile: sql.R,v $
# $Date: 2000/01/08 20:38:33 $
# $Revision: 3.0 $
# Copyright (C) 1999 Timothy H. Keitt

# You can't use this function to do joins
select <- function(columns="*", into, from, where, group.by, having,
                   order.by, limit, offset, distinct=F, exec=T) {
  preamble <- ifelse(distinct, "SELECT DISTINCT", "SELECT")
  if (columns != "*")
    columns <- sapply(to.db.name(columns), double.quote)
  query <- paste(preamble, list.to.csv(columns))
  if (!missing(into))
    query <- paste(query, "INTO", convert.table.name(into))
  query <- paste(query, "FROM", convert.table.name(from))
  if (!missing(where))
    query <- paste(query, "WHERE", list.to.csv(where))
  if (!missing(group.by)) {
    group.by <- sapply(to.db.name(group.by), double.quote)
    query <- paste(query, "GROUP BY", list.to.csv(group.by))
  }
  if (!missing(limit))
    query <- paste(query, "LIMIT", limit)
  if (!missing(offset))
    query <- paste(query, "OFFSET", offset)
  if (exec) {
    db.execute(query, clear=F)
    if (missing(into)) return(db.read.table()) else return(invisible())
  } else {
    return(query)
  }
}

insert <- function(into, column.names, values, query) {
  if (missing(values) && missing(query))
    stop("Nothing to append to table")
  preamble <- paste("INSERT INTO", convert.table.name(into))
  db.execute("BEGIN TRANSACTION")
  on.exit(db.execute("ROLLBACK TRANSACTION"))
  if (!missing(values)) {
    if (missing(column.names)) column.names <- names(values)
    column.names <- sapply(to.db.name(column.names), double.quote)
    formatted.data <- matrix(nrow=nrow(values), ncol=ncol(values))
    for (i in seq(along=column.names))
      formatted.data[,i] <- rpgsql.format.values(values[[i]])
    boiler <- paste(preamble, "(", list.to.csv(column.names), ")")
    for (i in 1:nrow(values)) {
      insert.values <- list.to.csv(formatted.data[i,])
      db.execute(boiler, "VALUES (", insert.values, ")")
    }
  } else {
    if (!missing(column.names)) {
      column.names <- sapply(to.db.name(column.names), single.quote)
      boiler <- paste(preamble, list.to.csv(column.names))
    } else {
      boiler <- preamble
    }
    db.execute(boiler, query)
  }
  on.exit(db.execute("COMMIT TRANSACTION"))
  return(invisible())
}
  
declare.cursor <- function(table.name, query) {
  table.name <- convert.table.name(table.name)
  cursor.name <- paste(sep="_", table.name, "cursor")
  if (missing(query)) query <- select(from=table.name, exec=F)
  db.execute("DECLARE", cursor.name, "CURSOR FOR", query)
  return(invisible(cursor.name))
}

fetch <- function(cursor.name, n=1, forward=T, move=F) {
  query <- paste(ifelse(move, "MOVE", "FETCH"),
                 ifelse(forward, "FORWARD", "BACKWARD"),
                 ifelse("all" %in% n, "ALL", n),
                 "IN", to.db.name(cursor.name))
  db.execute(query, clear=F)
  if (move) {
    db.clear.result()
    return(invisible())
  }
  return(db.read.table())
}







