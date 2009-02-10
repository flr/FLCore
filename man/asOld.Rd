\name{asOld}
\docType{methods}
\alias{as.FLBiol}
\alias{as.FLBiol-methods}
\alias{as.FLBiol,FLBiol-method}
\alias{as.FLBiol,FLStock-method}
\alias{as.FLIndex}
\alias{as.FLIndex-methods}
\alias{as.FLIndex,FLFleet-method}
\alias{as.FLStock}
\alias{as.FLSR}
\alias{as.FLSR-methods}
\alias{as.FLSR,FLStock-method}
\alias{as.FLSR,FLBiol-method}
\alias{as.FLQuant}
\alias{as.FLQuant-methods}
\alias{as.FLQuant,array-method}
\alias{as.FLQuant,matrix-method}
\alias{as.FLQuant,FLQuant-method}
\alias{as.FLQuant,vector-method}
\alias{as.FLQuant,data.frame-method}
\title{Old S3 coercion methods}
\description{
  These methods convert or coerce an object of a given class into another class. They 
  follow the S3 syntax for coercion methods and are being slowly substituted by 
  \code{as()} (see \code{\link[methods]{coerce}}. 
}
\section{Generic function}{
  \describe{
    \item{as.FLBiol(object)}{Convert to an \code{\link{FLBiol-class}}}
    \item{as.FLIndex(object)}{Convert to an \code{\link{FLIndex-class}}}
    \item{as.FLSR(object)}{Convert to an \code{\link{FLSR-class}}}
    \item{as.FLQuant(x)}{Convert to an \code{\link{FLQuant-class}}}
  }
}
\section{Methods}{\describe{
\item{ signature(object=FLBiol) :}{Describe method}
\item{ signature(object=FLStock) :}{Describe method}
\item{ signature(object=FLFleet) :}{Describe method}
\item{ signature(object=FLStock) :}{Describe method}
\item{ signature(object=FLBiol) :}{Describe method}
\item{ signature(x=array) :}{Describe method}
\item{ signature(x=matrix) :}{Describe method}
\item{ signature(x=FLQuant) :}{Describe method}
\item{ signature(x=vector) :}{Describe method}
\item{ signature(x=data.frame) :}{Describe method}
}}
\author{The FLR Team}
\seealso{ \link{FLComp} }
\examples{}
\keyword{methods}
