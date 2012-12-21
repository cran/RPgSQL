\name{to.db.name}
\alias{to.db.name}
\title{Convert an R name to a postgres friendly name}
\usage{
to.db.name(name)
}
\arguments{
 \item{name}{a character string}
}
\description{
  This function converts R dot "." separators to underscores.
}
\value{
  The reformatted name
}
\author{Timothy H. Keitt}
\seealso{\code{\link{to.R.name}}}
\keyword{database}
