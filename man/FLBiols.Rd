\name{FLBiols}
\docType{methods}
\alias{FLBiols-methods}
\alias{FLBiols,ANY-method}
 \alias{FLBiols,missing-method}
 \alias{FLBiols,list-method}
\title{Method FLBiols}
\description{
 Method for creating an object of class \code{FLBiols}.
}
\section{Generic function}{
 FLBiols(object, ...) 
}
\section{Methods}{\describe{
\item{ signature(object=ANY) :}{Returns an FLBiols object}
\item{ signature(object=missing) :}{Returns an empty FLBiols object}
\item{ signature(object=list) :}{Returns an FLBiols object}
}}
\author{The FLR Team}
\seealso{\link{FLBiols-class},  \link{FLlst-class}, \link{FLBiol-class},}
\examples{
bio <- FLBiol(n=FLQuant(1:10))
FLBiols(north=bio, south=bio)
}
\keyword{methods}
