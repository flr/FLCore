\name{mean.lifespan}
\alias{mean.lifespan}
\alias{mean.lifespan-methods}
\alias{mean.lifespan,FLBiol-method}
\title{Method for calculating mean lifespan, given the natural mortality}
\description{
	For an \code{FLBiol} object with the natural mortality-at-age
	present in the object.
}
\usage{
mean.lifespan(x, ...)
}
\arguments{
  \item{x}{ An object of type \code{\linkS4class{FLBiol}}.}
  \item{\dots}{ Extra arguments accepted by each implementation.}
}
\details{
Using actuarial definitions for the expected life-span of a given species, for a given 
survival rate-at-age (natural mortality), we can compute the expected life-span, \eqn{\ell_x}{logL(x)}, of a species, from a given reference age \eqn{x}, using the following equation:

\deqn{\ell_x = \sum\limits_{t=1}^{\infty} \exp\left(-\sum\limits_{i=x}^{x+t}M_i\right)}{logL(x) = SUM_1_inf exp(SUM_i=x_x+t m_i)}.

The method accepts objects of class \code{FLBiol} of any particular dimension. If the 
object has a seasonal structure to the population dynamics, then we sum over all seasons 
to get the yearly survival rate.
}
\value{
	An object of class \code{\linkS4class{FLQuant}} whose first and second dimension is of length one.
}
\author{FLR Team}
\seealso{\code{\linkS4class{FLBiol}}}
\examples{\dontrun{
data(ple4biol)
lfs.ple4 <- mean.lifespan(ple4,ref.age=1)
}
}
\keyword{methods}
