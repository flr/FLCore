\name{iter}
\docType{methods}
\alias{iter}
\alias{iter-methods}
\alias{iter,FLQuant,ANY-method}
\alias{iter,FLArray-method}
\alias{iter,vector-method}
\alias{iter,FLComp-method}
\alias{iter,FLModel-method}
\alias{iter,FLQuants-method}
\alias{iter,FLPar-method}
\alias{iter,FLFleet-method}
\alias{iter,FLMetier-method}
\alias{iter,logLik-method}
\alias{iter<-}
\alias{iter<--methods}
\alias{iter<-,FLQuant,FLQuant-method}
\alias{iter<-,FLCohort,FLCohort-method}
\alias{iter<-,FLComp,FLComp-method}
\alias{iter<-,FLPar,FLPar-method}
\alias{iter<-,FLPar,numeric-method}

\title{Select or modify iterations of an FLR object}
\description{
  To extract or modify a subset of the iterations contained in an FLR object, the 
  \code{iter} and \code{iter<-} methods can be used.

  In complex with various \code{FLQuant} slots, the \code{iter} method checks whether
  individual slots contain more than one iteration, i.e. \code{dims(object)[6] > 1}. If
  a particular slot contains a single iteration, that is returned, otherwise the chosen
  one is selected. This is in contrast with the subset operator \code{[}, which does not
  carry out this check.

  For objects of class \code{\link{FLModel}}, iters are extracted for slots of classes
  \code{FLQuant}, \code{FLCohort} and \code{FLPar}.

}
\section{Generic function}{
 iter(object) 
 iter<-(object,value) 
}
\section{Methods}{\describe{
\item{ signature(object=FLQuant) :}{Describe method}
\item{ signature(object=FLComp) :}{Describe method}
\item{ signature(object=FLQuants) :}{Describe method}
\item{ signature(object=FLPar) :}{Describe method}
}}
\author{The FLR Team}
\seealso{ \linkS4class{FLComp}, \linkS4class{FLQuant} }
\examples{
flq <- FLQuant(rnorm(800), dim=c(4,10,2), iter=10)
iter(flq, 2)

fls <- FLStock(catch.n=flq, m=FLQuant(0.2, dim=c(4,10,2)))
fls2 <- iter(fls, 2)
summary(fls2)
}
\keyword{methods}
