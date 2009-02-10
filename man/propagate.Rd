\name{propagate}
\docType{methods}
\alias{propagate}
\alias{propagate-methods}
\alias{propagate,FLQuant-method}
\alias{propagate,FLComp-method}
\alias{propagate,FLCohort-method}
\alias{propagate,FLMetier-method}
\alias{propagate,FLFleet-method}
\alias{propagate,FLPar-method}
\title{Extend an FLQuant along the iter dimension}
\description{
  FLR objects with a single iteration (length of 1 in the sixth dimension) can be
  extended using the \code{propagate} method. The \code{type} argument
  selects whether the new iterations are filled with the content of the first iteration 
  (\code{type='all'}) or left empty (\code{type='first'}).

  For objects of class \code{\link{FLPar-class}}, propagate will extend the object along
  the first dimension, \code{iter}. The newly created space will alway be empty 
  (\code{NA}).
}
\section{Generic function}{
 propagate(object) 
}
\section{Arguments}{
  \describe{
    \item{object :}{An FLR object to extend.}
    \item{fill.iter :}{Copy the first iteration along the new ones ('TRUE'), the default,
    or leave them empty ('FALSE').}
    \item{iter :}{Number of iterations.}
  }
}
\section{Methods}{\describe{
\item{signature(object=FLQuant) :}{Extends object along the \code{iter} dimension}
\item{signature(object=FLComp) :}{Extends all \code{\link{FLQuant-class}} slots in the
  object along the \code{iter} dimension}
\item{signature(object=FLMetier) :}{Extends all \code{\link{FLQuant-class}} slots in the
  object along the \code{iter} dimension, as well as those of those objects of class
  \code{\link{FLCatch-class}} inside the \code{catches} slot}
\item{signature(object=FLFleet) :}{Extends all \code{\link{FLQuant-class}} slots in the
  object along the \code{iter} dimension, as well as those of those objects of class
  \code{\link{FLMetier-class}} inside the \code{metiers} slot}
\item{signature(object=FLPar) :}{Extends object along the \code{iter} dimension}
}}
\author{The FLR Team}
\seealso{\link{FLComp}}
\examples{
flq <- FLQuant(rnorm(50), dim=c(5,10))
propagate(flq, 10)
# Look at the %NA in summary
summary(propagate(flq, 10, fill.iter=FALSE))
}
\keyword{methods}
