\name{FLBiol}
\docType{methods}
\alias{FLBiol}
\alias{FLBiol-methods}
\alias{FLBiol,missing-method}
\alias{FLBiol,FLQuant-method}
\title{Create FLBiol objects}
\description{
  Constructor method for the FLBiol class
}
\section{Arguments}{
  \describe{
  \item{object}{An initial FLQuant only used for sizing.}
  \item{plusgroup}{The age/length to be considered as plusgroup.}
  \item{...}{Extra objects to be allocated to given slots.}
  }
}
\section{Generic function}{
 FLBiol(object, ...)
}
\section{Methods}{\describe{
\item{ signature(object=missing) :}{Returns an \code{FLBiol} using the first provided 
  argument of class \code{FLQuant} for object sizing, or \code{FLQuant()} if none given.}
\item{ signature(object=FLQuant) :}{Returns and \code{FLBiol} that uses \emph{object} for
  sizing.}
}}
\author{The FLR Team}
\seealso{ \link{FLComp} }
\examples{
flb <- FLBiol(name="MAC", m=FLQuant(0.2, dimnames=list(age=1:10, year=1950:2005)))
summary(flb)
}
\keyword{methods}
