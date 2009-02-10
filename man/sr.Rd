% $ Id: $
\name{sr}
\alias{sr}
\title{Stock-recruitment model function}
\description{
    The \code{sr()} function acts as a front end to the various functions available
    that implement fitting mechanisms for various stock/recruitment models. 
    \code{\link{fmle}} is called if a likelihood function is present in the \emph{logl} 
    slot, otherwise \code{\link{nls}} is used instead.
}
\usage{
	sr(sr, ...)
}
\arguments{
  \item{sr}{An FLSR object.}
  \item{\dots}{Other parameters, depending on the model selected.}
}
\value{
    An object of class \code{\link{FLSR-class}}
}
\author{The FLR Team}
\seealso{\code{\link{FLSR-class}}}
\keyword{models}
