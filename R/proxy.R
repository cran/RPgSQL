# -*- R -*-
# $RCSfile: proxy.R,v $
# $Date: 2000/07/12 19:31:40 $
# $Revision: 1.2 $
# Copyright (C) 1999 Timothy H. Keitt

# The vacuum function is postgres specific
bind.db.proxy <- function(table.name, vacuum=F) {
  for (i in seq(along=table.name)) {
    name <- make.names(table.name[[i]])
    class(name) <- c("db.proxy", "data.frame")
    attr(name, "dbname") <- db.name()
    attr(name, "host") <- db.host.name()
    if (vacuum) db.execute("VACUUM ANALYZE", name)
    parent.environment <- sys.frame(sys.parent())
    assign(name, name, envir=parent.environment)
  }
  return(invisible())
}

# This may break some other packages, but there is no other option,
# and row.names really should be generic
row.names <- function(x) UseMethod("row.names")
row.names.default <- function(x) attr(x, "row.names")
row.names.db.proxy <- function(x) {
  if (db.has.row.names(x)) {
    return(row.names(sql.select("rpgsql.row.names", from=x)))
  } else {
    return(as.character(seq(from=1, length=nrow(x))))
  }
}

# This is required because of stupid definition of dimnames.data.frame
dimnames.db.proxy <- function(x) list(row.names(x), names(x))

# Again
as.list.db.proxy <- function(x) as.list(as.data.frame(x))

as.matrix.db.proxy <- function(proxy)
  return(as.matrix(db.read.table(proxy)))

# One can only wish
"$.db.proxy" <- function(proxy, col) return(proxy[[col]])

"[[.db.proxy" <- function(proxy, i, j) {
  if (missing(i)) stop("no subscript")
  if (length(i) > 1) stop("invalid subscript")
  if (nargs() > 2) {
    if (is.character(i)) i <- match(i, row.names(proxy))
    if (length(j) > 1) stop("invalid subscript")
    if (mode(j) == "numeric") j <- names(proxy)[j]
    query <- sql.select(j, from=proxy, limit=1, offset=i-1, exec=F)
    db.execute(query, clear=F)
    return(db.result.get.value())
  }
  if (is.numeric(i)) i <- names(proxy)[i]
  query <- sql.select(columns=i, from=proxy, exec=F)
  db.execute(query, clear=F)
  return(db.read.column())
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

"[<-.db.proxy" <- function(x) stop("Not implemented")
"[[<-.db.proxy" <- function(x) stop("Not implemented")

as.data.frame.db.proxy <- function(x) return(db.read.table(x))

db.has.row.names <- function(proxy)
  return("rpgsql.row.names" %in% names(proxy))

names.db.proxy <- function(proxy) {
  db.execute("SELECT typrelid FROM pg_type WHERE typname =",
             single.quote(make.db.names(db.table.name(proxy))), clear=F)
  relid <- db.result.get.value()
  db.execute("SELECT attname FROM pg_attribute WHERE attrelid =",
             relid, "AND attnum > 0", clear=F)
  return(make.names(db.read.column(as.is=T)))
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
  cat("Database:", attr(proxy, "dbname"), "\n")
  cat("Host:", attr(proxy, "host"), "\n")
  cat("Dimensions:", ncol(proxy), "(columns)",
      nrow(proxy), "(rows)\n\n")
}

print.db.proxy <- function(proxy, max.rows=10, max.columns=5, ...) {
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
    print(proxy[ , 1:5], ...)
    cat("Continues for", columns-max.columns, "more cols...\n")
  } else {
    print(proxy[], ...)
  }
  return(invisible())
}

db.table.name <- function(proxy) return(as.character(unclass(proxy)))









