# -*- R -*-
# $RCSfile: sql.R,v $
# $Date: 2000/07/14 20:54:46 $
# $Revision: 1.3 $
# Copyright (C) 1999 Timothy H. Keitt

# You can't use this function to do joins
sql.select <- function(columns="*", into, from, where, group.by, having,
                       order.by, limit, offset, distinct=F, exec=T) {
  preamble <- ifelse(distinct, "SELECT DISTINCT", "SELECT")
  if (columns != "*")
    columns <- sapply(make.db.names(columns), double.quote)
  query <- paste(preamble, list.to.csv(columns))
  if (!missing(into))
    query <- paste(query, "INTO", format.table.name(into))
  query <- paste(query, "FROM", format.table.name(from))
  if (!missing(where))
    query <- paste(query, "WHERE", list.to.csv(where))
  if (!missing(group.by)) {
    group.by <- sapply(make.db.names(group.by), double.quote)
    query <- paste(query, "GROUP BY", list.to.csv(group.by))
  }
  if (!missing(limit))
    query <- paste(query, "LIMIT", limit)
  if (!missing(offset))
    query <- paste(query, "OFFSET", offset)
  if (exec) {
    on.exit(db.clear.result())
    db.execute(query, clear=F)
    if (missing(into))
      return(db.fetch.result())
    else
      return(invisible())
  } else {
    return(query)
  }
}

sql.insert <- function(into, column.names, values, query) {
  if (missing(values) && missing(query))
    stop("Nothing to append to table")
  preamble <- paste("INSERT INTO", format.table.name(into))
  db.execute("BEGIN TRANSACTION")
  on.exit(db.execute("ROLLBACK TRANSACTION"))
  if (!missing(values)) {
    if (missing(column.names)) column.names <- names(values)
    column.names <- sapply(make.db.names(column.names), double.quote)
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
      column.names <- sapply(make.db.names(column.names), single.quote)
      boiler <- paste(preamble, list.to.csv(column.names))
    } else {
      boiler <- preamble
    }
    db.execute(boiler, query)
  }
  on.exit(db.execute("COMMIT TRANSACTION"))
  return(invisible())
}
  







