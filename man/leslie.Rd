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
  \item{object}{ An object of type \code{\linkS4class{FLBiol}}.}
  \item{\dots}{ Extra arguments accepted by each implementation.}
}
\details{
Usual Leslie matrix type dynamics for a \code{FLBiol} object. 
}
\value{
	An object of class \code{\linkS4class{FLBiol}}.
}
\references{
Leslie, P.H. (1945) The use of matrices in certain population mathematics. Biometrika,33(3), 183-212.

Leslie, P.H. (1948) Some further notes on the use of matrices in population mathematics. Biometrika, 35(3-4), 213-245.
}
\author{FLR Team}
\seealso{\code{\linkS4class{FLBiol}}}
\examples{
data(ple4.biol)
ple4.l <- leslie(ple4.biol,plusgroup=FALSE)
}
\keyword{methods}
