% $Id$
\name{AIC}
\docType{methods}
\alias{AIC,FLModel,numeric-method}
\alias{AIC,FLModel,missing-method}
\title{Akaike information criterion (AIC) method}
\description{
	A method to calculate the Akaike information criterion (AIC) of an \link{FLModel} object
	from the value of the obtained log-likelihood stored in its \code{logLik} slot.
}
\section{Generic function}{
 AIC(object, k) 
}
\section{Method arguments}{\describe{
\item{\code{object} :}{an object of class \link{FLModel} or of one that inherits from it.}
\item{\code{k} :}{numeric, the "penalty" per parameter to be used; the default k = 2 is the classical AIC.}
}}
\section{Methods}{\describe{
\item{ signature(object=FLModel, k=numeric) :}{AIC of an FLModel object with an specified 
	value for the "penalty".}
\item{ signature(object=FLModel, k=missing) :}{AIC of an FLModel object with the default 
	"penalty".}
}}
\author{The FLR Team}
\seealso{\link[stats]{AIC}, \link[stats]{logLik}, \link{FLModel}}
\examples{
data(nsher)
AIC(nsher)
}
\keyword{methods}
