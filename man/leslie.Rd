\name{leslie}
\alias{leslie}
\alias{leslie-methods}
\alias{leslie,FLBiol-method}
\title{Method for calculating Leslie matrix dynamics of an FLBiol object}
\description{
	For an \code{FLBiol} object with the natural mortality-at-age, fecundity and spwn data 
	present in the object.
}
\usage{
leslie(object, ...)
}
\arguments{
  \item{object}{ An object of type \code{\link{FLBiol-class}}.}
  \item{\dots}{ Extra arguments accepted by each implementation.}
}
\details{
Usual Leslie matrix type dynamics for ab \code{FLBiol} object. 
}
\value{
	An object of class \code{\link{FLBiol-class}}.
}
\references{}
\author{FLR Team}
\note{}
\seealso{\code{\link{FLBiol-class}}}
\examples{\dontrun{
data(ple4biol)
ple4.l <- leslie(ple4.biol,plusgroup=FALSE)
}
}
\keyword{methods}
