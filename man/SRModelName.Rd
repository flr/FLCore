\name{SRModelName}
\alias{SRModelName}
\title{Convenience function to identify an SR model by its formula}
\description{
  A supplied formula, representing an stock-recruitment relationship, is matched
  against the list of all models defined in \code{FLCore} (See \link{SRModels}).

  If a match is found, a tring character with the name of the model is returned,
  otherwise \code{FALSE} is obtained.
}
\usage{
SRModelName(formula)
}
\arguments{
  \item{model}{A formula defining the model}
}
\value{
  \item{name}{A character string or NULL}
}
\author{FLR Team}
\seealso{\code{\link{SRModels}}}
\examples{
SRModelName(rec ~ a * ssb * exp(-b * ssb))
}
\keyword{models}
\keyword{utilities}
