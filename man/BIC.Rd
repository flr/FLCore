% $Id$
\name{BIC}
\docType{methods}
\alias{BIC,FLModel-method}
\title{Bayesian information criterion (BIC) method}
\description{
	A method to calculate the Bayesian information criterion (BIC), also known as Schwarz's 
	Bayesian criterion of an \link{FLModel} object from the value of the obtained
  log-likelihood stored in its \code{logLik} slot.
}
\section{Generic function}{
 BIC(object) 
}
\section{Methods}{\describe{
\item{ signature(object=FLModel) :}{BIC of an FLModel object with an specified 
	value for the "penalty".}
\item{ signature(object=FLModel, k=missing) :}{AIC of an FLModel object for which no
	value of "penalty" is specified, thus k=2.}
}}
\author{The FLR Team}
\seealso{ \link[stats]{BIC}, \link[stats]{AIC}, \link{FLModel}, \link[stats]{logLik}}
\examples{
data(nsher)
BIC(nsher)
}
\keyword{methods}
