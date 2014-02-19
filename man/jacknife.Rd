\name{jacknife}
\docType{methods}
\alias{jacknife}
\alias{jacknife-methods}
\alias{jacknife,FLQuant-method}
\title{Jacknife resampling}
\description{The \code{jacknife} method sets up objects ready for jacknifing, i.e. to systematically recompute a given statistic leaving out one observation at a time. From this new set of "observations" for the statistic an estimate for the bias can be calculated as well as an estimate for the variance of the statistic.

Input objects cannot have length > 1 along the \code{iter} dimension, and the resulting 
object will have as many \code{iterations} as elements in the original object.
}
\section{Generic function}{
 jacknife(object, ...) 
}
\section{Methods}{\describe{
\item{signature(object=FLQuant) :}{Returns an FLQuant with iter=length(object), where in each \code{iteration} one element has been sequantally converted to \code{NA}.}
}}
\author{The FLR Team}
\seealso{\code{\link{FLQuant}} }
\examples{
flq <- FLQuant(1:8)
iters(jacknife(flq))}
\keyword{methods}
