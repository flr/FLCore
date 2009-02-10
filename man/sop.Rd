\name{sop}
\alias{sop}
\title{ Calculates the sum of products correction }
\description{
  Calculates the sum of products correction for quantities such a catch, discards, 
  landings.  For example in an object of class \code{\link{FLStock-class}} there are 
  slots \emph{catch.n}, \emph{catch.wt} and \emph{catch}. \emph{catch} should equal
  the products of catch.n*catch.wt summed over ages. This function returns the ratio
  (i.e. the correction) of \emph{catch.n}*\emph{catch.wt} : \emph{catch}, which can
  then be used to correct either \emph{catch.n} or \emph{catch.wt}.
}
\usage{
sop(stock, slot)
}
\arguments{
  \item{stock}{An FLStock object}
  \item{slot}{Name of the slot group, i.e. "catch", "landings" or "discards" for an FLStock object.}
}
\details{
  Can be used for any class or slot where there are the three FLQuant slots \emph{foo}, \emph{foo.n} and \emph{foo.wt}, representing totals added over all quants (ages), numbers by quant, and weight by quant.
}
\value{
  Returns the ratio as an FLQuant
}
\author{FLR Team}
\examples{
data(ple4)
sop(ple4,"catch")
}
\keyword{methods}
