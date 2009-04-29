\name{dimnames<-}
\docType{methods}
\alias{dimnames<-,ANY,missing-method}
\alias{dimnames<-,FLQuant,list-method}
\alias{dimnames<-,FLStock,list-method}
\title{Modify dimnames of an FLQuant}
\description{
  The \code{dimnames<-} method for objects of class \code{\link{FLQuant}} modifies the
  \code{dimnames} attribute. In contrast with the method for class \code{array}, an
  incomplete named list of dimension names can be provided. Only the relevant dimensions
  will be modify.

  It is posible to modify the name of the first dimension (by default \code{quant}) using
  this method.
}
\section{Generic function}{
 dimnames<-(x,value) 
}
\section{Methods}{\describe{
\item{signature(x=FLQuant, value=list) :}{Modify FLQuant dimnames.}
\item{signature(x=FLStock, value=list) :}{Modify dimnames of all FLQuant slots inside an FLStock object.}
}}
\author{The FLR Team}
\seealso{\code{\link[base]{dimnames}}, \code{\link{FLQuant}}, \code{\link[base]{array}}}
\examples{
flq <- FLQuant(rnorm(80), dim=c(4,10,2))
dimnames(flq) <- list(unit=c('male', 'female'))
# This modifies both dimnames and dimnames name
dimnames(flq) <- list(age=0:3)
dimnames(flq)
}
\keyword{methods}
