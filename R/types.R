# -*- R -*-
# $RCSfile: types.R,v $
# $Date: 2000/07/29 22:03:24 $
# $Revision: 1.2 $
# Copyright (C) 1999 Timothy H. Keitt

rpgsql.data.type <- function(x) UseMethod("rpgsql.data.type")

rpgsql.data.type.default <- function(x) {
  if(is.factor(x)) return("TEXT")
  if(is.integer(x)) return("INTEGER")
  if(is.double(x)) return("REAL")
  if(is.logical(x)) return("BOOL")
  if(is.complex(x)) stop("complex data not supported")
  return("TEXT")
}

rpgsql.data.type.dates <- function(x) return("DATE")
rpgsql.data.type.times <- function(x) return("TIME")

rpgsql.format.values <- function(x) UseMethod("rpgsql.format.values")

rpgsql.format.values.default <- function(x)
  return(format.null.values(single.quote(x)))

rpgsql.format.values.dates <- function(x) {
  attr(x, "format") <- "day mon year"
  return(rpgsql.format.values.default(x))
}

rpgsql.format.values.times <- function(x) {
  attr(x, "format") <- "h:m:s"
  return(rpgsql.format.values.default(x))
}

rpgsql.cast.values <- function(x) UseMethod("rpgsql.cast.values")

rpgsql.cast.values.default <- function(x) {
  if(!is.null(class(x)))
    warning(paste("Coercing PGSQL type", class(x), "to character"))
  return(as.character(x))
}

rpgsql.cast.values.16 <- function(x) return(x == "t")
rpgsql.cast.values.19 <- function(x) return(as.factor(x))
rpgsql.cast.values.20 <- function(x) return(as.integer(x))
rpgsql.cast.values.21 <- function(x) return(as.integer(x))
rpgsql.cast.values.23 <- function(x) return(as.integer(x))
rpgsql.cast.values.25 <- function(x) return(as.factor(x))
rpgsql.cast.values.700 <- function(x) return(as.double(x))
rpgsql.cast.values.701 <- function(x) return(as.double(x))
rpgsql.cast.values.1042 <- function(x) return(as.factor(x))
rpgsql.cast.values.1043 <- function(x) return(as.factor(x))
rpgsql.cast.values.1082 <- function(x) return(dates(x, format='m-d-y'))
rpgsql.cast.values.1083 <- function(x) return(times(x))
rpgsql.cast.values.1700 <- function(x) return(as.double(x))





