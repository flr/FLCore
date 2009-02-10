\name{median}
\docType{methods}
\alias{median,FLPar,missing-method}
\alias{median,FLPar-method}
\title{Method median}
\description{
  Calculates the median.
}
\section{Generic function}{
 median(x,na.rm) 
}
\section{Methods}{\describe{
\item{ signature(x=FLQuantPoint,na.rm=missing) :}{Returns the median of x, see 
\code{link}{median,FLQuantPoint-method}}.}
\item{ signature(x=FLPar,na.rm=missing) :}{Returns the median of x along the \emph{iter} dimension.}
}
\author{The FLR Team}
\seealso{\link[stats]{median}, \link[base]{apply}}
\examples{
flp <- FLPar(rnorm(80), params=c('a', 'b'), iter=1:40)
median(flp)
}
\keyword{methods}
