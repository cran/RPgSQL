## -*- R -*-
## $RCSfile: zzz.R,v $
## $Date: 2000/07/10 23:17:33 $
## $Revision: 1.2 $
## Copyright (C) 1999 Timothy H. Keitt
.First.lib <- function(lib, pkg) {
  library.dynam("RPgSQL", pkg, lib)
  autoload("times", "chron")
  autoload("dates", "chron")
  return(invisible())
}

