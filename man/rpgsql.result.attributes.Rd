\name{Result attributes}
\alias{db.result.columns}
\alias{db.result.rows}
\alias{db.result.column.number}
\alias{db.result.column.names}
\alias{db.result.column.type}
\title{Information about the result buffer}
\usage{
db.result.columns()
db.result.rows()
db.result.column.number(col.name)
db.result.column.type(col=1)
db.result.column.names()
}
\arguments{
 \item{col.name}{a character string containing a column name}
 \item{col}{a column number}
}
\description{
  \code{db.result.columns} get number of columns in result.\cr
  \code{db.result.rows} get number of rows in result.\cr
  \code{db.result.column.number} convert column a name to a column
  number.\cr  
  \code{db.result.column.type} get the type-code for a result column.\cr 
  \code{db.result.column.names} get the column names from the result
  buffer.
}
\value{
  \code{db.result.columns} the number of columns in the result
  buffer.\cr
  \code{db.result.rows} the number of rows in the result buffer.\cr
  \code{db.result.column.number} a column number. \cr  
  \code{db.result.column.type} a column type code.\cr 
  \code{db.result.column.names} a list of column names.
}
\author{Timothy H. Keitt}
\keyword{database}
