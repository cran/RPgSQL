# -*- R -*-
# $RCSfile: proxy.R,v $
# $Date: 2001/06/20 19:46:09 $
# $Revision: 1.14 $
# Copyright (C) 1999,2000  Timothy H. Keitt, Adam M Kornick

# The vacuum function is postgres specific
bind.db.proxy <- function(table.names, vacuum=F, writable=F) {
  table.names <- make.names(table.names)
  for (i in seq(along=table.names)) {
    proxy <- list(table.name=table.names[i], host=db.host.name(),
                  port=db.connection.port(), dbname=db.name(),
                  user=db.user.name(), password=db.password())
    class(proxy) <- c("db.proxy", "data.frame")
    if (writable) class(proxy) <- c("db.proxy.writable", class(proxy))
    if (vacuum) db.execute("VACUUM ANALYZE", table.names[i])
    assign(table.names[i], proxy, envir=sys.frame(sys.parent()))
  }
  return(invisible())
}

row.names.db.proxy <- function(x) {
  if (db.has.row.names(x)) {
    return(row.names(sql.select("rpgsql.row.names", from=x)))
  } else {
    return(as.character(seq(from=1, length=nrow(x))))
  }
}

dimnames.db.proxy <- function(x) list(row.names(x), names(x))

as.list.db.proxy <- function(x) as.list(as.data.frame(x))

as.matrix.db.proxy <- function(proxy) return(as.matrix(db.read.table(proxy)))

as.data.frame.db.proxy <- function(x) return(db.read.table(x))

db.has.row.names <- function(proxy)
  return("rpgsql.row.names" %in% names(proxy))

names.db.proxy <- function(proxy) {
  on.exit(db.clear.result())
  db.execute("SELECT * FROM", db.table.name(proxy), "LIMIT 1", clear=F)
  return(make.names(db.result.column.names()))
}

dim.db.proxy <- function(proxy) {
  on.exit(db.clear.result())
  name <- make.db.names(db.table.name(proxy))
  db.execute("SELECT reltuples, relnatts FROM pg_class",
             "WHERE relname =", single.quote(name), clear=F)
  return(as.integer(c(db.result.get.value(), db.result.get.value(col=2))))
}

summary.db.proxy <- function(proxy) {
  cat("Table name:", db.table.name(proxy), "\n")
  cat("Database:", unclass(proxy)$dbname, "\n")
  cat("Host:", unclass(proxy)$host, "\n")
  cat("Dimensions:", ncol(proxy), "(columns)",
      nrow(proxy), "(rows)\n\n")
}

print.db.proxy <- function(proxy, max.rows=10, max.columns=10, ...) {
  dimension <- dim(proxy)
  rows <- dimension[1]
  columns <- dimension[2]
  if (rows > max.rows && columns > max.columns) {
    print(proxy[1:max.rows, 1:max.columns], ...)
    cat("Continues for", rows-max.rows, "more rows and",
        columns-max.columns, "more cols...\n")
  } else if (rows > max.rows) {
    print(proxy[1:max.rows, ], ...)
    cat("Continues for", rows-max.rows, "more rows...\n")
  } else if (columns > max.columns) {
    print(proxy[ , 1:max.columns], ...)
    cat("Continues for", columns-max.columns, "more cols...\n")
  } else {
    print(proxy[], ...)
  }
  return(invisible())
}

db.table.name <- function(proxy)
  return(make.db.names(unclass(proxy)$table.name))

"$.db.proxy" <- function(proxy, col) return(proxy[[col]])

"[[.db.proxy" <- function(proxy, i, j) {
  if (missing(i)) stop("no subscript")
  if (length(i) > 1) stop("invalid subscript")
  if (nargs() > 2) {
    if (is.character(i)) i <- match(i, row.names(proxy))
    if (length(j) > 1) stop("invalid subscript")
    if (mode(j) == "numeric") j <- names(proxy)[j]
    return(sql.select(j, from=proxy, limit=1, offset=i-1)[[1,1]])
  }
  if (is.numeric(i)) i <- names(proxy)[i]
  return(sql.select(columns=i, from=proxy)[[1]])
}

"[.db.proxy" <- function(proxy, rows, cols) {
  if (nargs() < 2) return(db.read.table(proxy))
  if (!missing(rows)) {
    if (mode(rows) == "character") {
      rows <- match(rows, row.names(proxy))
    } else {
      if (any(rows < 0)) {
        if (any(rows > 0))
          stop("cannot mix negative and positive indices")
        rows <- seq(from=1, length=nrow(proxy))[rows]
      }
    }
  }
  if (!missing(cols)) {
    if (mode(cols) == "character") {
      columns <- cols
    } else {
      if (any(cols < 0)) {
        if (any(cols > 0))
          stop("cannot mix negative and positive indices")
        cols <- (1:ncol(proxy))[cols]
      }
      columns <- names(proxy)[cols]
    }
  } else {
    columns <- "*"
  }
  if (missing(rows)) {
    return(sql.select(columns, from=proxy))
  } else {
    # This needs to be generalized to allow extraction
    # of non-contiguous rows of data
    first.row <- min(rows)
    n <- max(rows) - first.row + 1
    data <- sql.select(columns, from=proxy, limit=n,
                       offset=first.row-1)
    row.names(data) <- seq(from=first.row, length=n)
    return(data)
  }
}

"$<-.db.proxy" <- function(...) stop("Object is read-only")
"[<-.db.proxy" <- function(...) stop("Object is read-only")
"[[<-.db.proxy" <- function(...) stop("Object is read-only")

# Methods for writable proxies defined in this block

"$<-.db.proxy.writable" <- function(...) stop("Not implemented")
"[<-.db.proxy.writable" <- function(...) stop("Not implemented")
"[[<-.db.proxy.writable" <- function(...) stop("Not implemented")


