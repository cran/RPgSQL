## -*- R -*-
## $RCSfile: zzz.R,v $
## $Date: 2000/01/08 20:38:33 $
## $Revision: 3.0 $
## Copyright (C) 1999 Timothy H. Keitt
.First.lib <- function(lib, pkg) {
  provide(RPgSQL)
  library.dynam("RPgSQL", pkg, lib)
  autoload("times", "chron")
  autoload("dates", "chron")
  return(invisible())
}

