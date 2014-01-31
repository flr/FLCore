\docType{class}
\name{FLArray}
\alias{FLArray}
\alias{FLArray-class}
\title{Class FLArray}
\description{
A basic class, extended by \code{\linkS4class{FLQuant}} and
\code{\linkS4class{FLCohort}}. No objects of this class are
created in FLCore, as it is used only for method
inheritance.
}
\section{Slots}{
  \describe{ \item{.Data}{Internal S4 data representation,
  of class \code{array}.} \item{units}{The data units in
  some understandable metric, as a \code{character}
  vector.} }
}

\section{Validity}{
  \describe{ \item{Dimensions}{Array must have 6
  dimensions} \item{Content}{Array must be of class
  \code{numeric}} }
}
\author{
The FLR Team
}
\seealso{
\code{\linkS4class{FLQuant}}, \code{\linkS4class{FLCohort}}
}
\keyword{classes}

