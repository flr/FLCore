\name{FLStock}
\docType{methods}
\alias{FLStock}
\alias{FLStock-methods}
\alias{FLStock,missing-method}
\alias{FLStock,FLQuant-method}
\title{Create FLStock objects}
\description{
  Constructor method for the FLStock class
}
\section{Arguments}{
  \describe{
  \item{object}{An initial FLQuant only used for sizing.}
  \item{plusgroup}{The age/length to be considered as plusgroup.}
  \item{...}{Extra objects to be allocated to given slots.}
  }
}
\section{Generic function}{
 FLStock(object, ...)
}
\section{Methods}{\describe{
\item{ signature(object=missing) :}{Returns an \code{FLStock} using the first provided 
  argument of class \code{FLQuant} for object sizing, or \code{FLQuant()} if none given.}
\item{ signature(object=FLQuant) :}{Returns and \code{FLStock} that uses \emph{object} only for sizing.}
}}
\author{The FLR Team}
\seealso{ \link{FLComp} }
\examples{
fls <- FLStock(name='MAC', catch=FLQuant(c(200,300,400), dimnames=list(year=1999:2001)))
}
\keyword{methods}
