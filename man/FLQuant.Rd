\name{FLQuant}
\docType{methods}
\alias{FLQuant}
\alias{FLQuant-methods}
\alias{FLQuant,missing-method}
 \alias{FLQuant,vector-method}
 \alias{FLQuant,array-method}
 \alias{FLQuant,matrix-method}
 \alias{FLQuant,FLQuant-method}
\title{Create FLQuant objects}
\description{
  The \code{FLQuant} constructor method allows simple creation of new FLQuant objects
  from vectors, matrices and arrays.

  R standard recyling rules (row first) are applied in most cases, so if an input object 
  smaller than the required dimensions is given, it will be reused. An exception is made
  for objects of class vector, which will be considered to go along the second dimension
  (\code{year}) instead of the first one. To recover the standard behaviour, a matrix can
  be created.

  The \code{iter} argument takes preference over \code{dim} but not over \code{dimnames},
  so it is preferable to ensure consistency between arguments. \code{dim} and 
  \code{dimnames} must match and an error is thrown otherwise.


}
\section{Arguments}{
  \describe{
  \item{object}{An input object.}
  \item{dim}{The dimensions of the object. A vector of class \code{numeric}.}
  \item{dimnames}{The dimension names, as a \code{list}.}
  \item{quant}{A \code{character} string with the name of the first dimension. Defaulta to 'quant'}
  \item{units}{A \code{character} string with the units of measurement. Defaults to 'NA'}
  \item{iter}{The number of iterations, i.e. the length of the sixth dimension, on the output object.}
  \item{fill.iter}{TRUE}
  }
}
\section{Generic function}{
 FLQuant(object) 
}
\section{Methods}{\describe{
\item{ signature(object=missing) :}{Describe method}
\item{ signature(object=vector) :}{Describe method}
\item{ signature(object=array) :}{Describe method}
\item{ signature(object=matrix) :}{Describe method}
\item{ signature(object=FLQuant) :}{Describe method}
}}
\author{The FLR Team}
\seealso{ \link{FLQuant-class} }
\examples{
flq <- FLQuant()
flq <- FLQuant(1:10, dim=c(2,5))
summary(flq)

# Vectors are used column first...
dim(FLQuant(1:10))
# ...while matrices go row first.
dim(FLQuant(matrix(1:10)))

FLQuant(matrix(rnorm(100), ncol=20))

FLQuant(array(rnorm(100), dim=c(5,2,1,1,1,10)))
FLQuant(array(rnorm(100), dim=c(5,2)), iter=10)
}
\keyword{methods}
