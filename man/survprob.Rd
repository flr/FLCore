\name{survprob}
\alias{survprob}
\alias{survprob-methods}
\alias{survprob,FLBiol-method}
\title{Calculating survival probabilties given mortality in the FLBiol object}
\description{
	For an \code{FLBiol} object with the mortality-at-quant this method calculates the 
  associated survival probability-at-quant. This can be later used by the \code{r()} 
  method. The calculation can be carried out either by year or by cohort.
}
\usage{
survprob(object, ...)
}
\arguments{
  \item{object}{ An object of type \code{\link{FLBiol-class}}.}
  \item{\dots}{ Extra arguments accepted by each implementation.}
}
\details{
  Calculates the survival probability-at-quant given the mortality information in an 
  \code{\link{FLBiol-class}}  object - survival probability from one year to the next is 
  simply exp(-M) and the survival probaiblity to a given quant is merely the product
  along the quant dimension of the individual survival probabilities. 
}
\value{
	An object of class \code{\link{FLQuant-class}}.
}
\author{FLR Team}
\seealso{\code{\link{FLBiol-class}}}
\examples{\dontrun{
data(nsher.biol)
nsh.ps <- survprob(nsh.biol,by='year')
}
}
\keyword{methods}
