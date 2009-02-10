\name{FLlst}
\docType{methods}
\alias{FLlst}
\alias{FLlst-methods}
\alias{FLlst,ANY-method}
\alias{FLlst,missing-method}
\alias{FLlst,list-method}
\title{Method FLlst}
\description{
  Basic creator method for objects of class \code{\link{FLlst-class}}. This is not 
  intended to be called directly bu the user, as this class is extended by the other
  plural classes (e.g. \code{\link{FLQuants-class}}, \code{\link{FLStocks-class}}, ...)
  and this method is for internal use of the various constructors.
}
\section{Generic function}{
 FLlst(object) 
}
\section{Methods}{\describe{
\item{ signature(object=ANY) :}{Returns an \code{FLlst} object with the provided elements if they are all o the same class.}
\item{ signature(object=missing) :}{Returns an empty \code{FLlst} object}
\item{ signature(object=list) :}{Returns and \code{FLlst} object with the input list at its core}
}}
\author{The FLR Team}
\seealso{ \link{FLComp} }
\keyword{methods}
