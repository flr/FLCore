\name{spr0}
\docType{methods}
\alias{spr0}
\alias{spr0-methods}
\alias{spr0,FLQuant,FLQuant,FLQuant-method}
\alias{spr0,FLStock,missing,missing-method}
\alias{spr0,FLSR,missing,FLQuant-method}
\title{Method spr0}
\description{
  Calculates spawners per recruit at F=0. 

  This method currently does not work if any of the input objects have multiple units,
  seasons or areas (if dim(object)[3:5] > 1).
}
\section{Generic function}{
 quant(ssb, rec, fbar) 
}
\section{Methods}{\describe{
\item{ signature(ssb=FLQuant, rec=FLQuant, f=FLQuant) :}{\code{ssb}, \code{rec} and \code{fbar} as \code{FLQuant}(s)}
\item{ signature(ssb=FLStock, rec=missing, f=missing) :}{\code{ssb}, \code{rec} and \code{fbar} are obtained from the slots of an \code{FLStock} object. \code{harvest} must have \code{units='f'}}
\item{ signature(ssb=FLSR, rec=missing, f=FLQuant) :}{\code{rec} and \code{ssb} are obtained from an \code{FLSR} object, while \code{fbar} must be provided.}
}}
\author{The FLR Team}
\seealso{ \link{FLStock-class},  \link{FLSR-class}}
\examples{
# example FLStock dataset
spr0(ple4)
}
\keyword{methods}
