\name{rlnorm}
\docType{methods}
\alias{rlnorm,numeric,FLQuant,FLQuant-method}
\alias{rlnorm,numeric,FLQuant,numeric-method}
\alias{rlnorm,numeric,numeric,FLQuant-method}
\alias{rlnorm,numeric,FLQuantPoint,missing-method}
\title{Method rlnorm}
\description{
Random generation for the log normal distribution whose logarithm has mean equal to \emph{meanlog} and standard deviation equal to \emph{sdlog}.  \emph{meanlog} and \emph{sdlog} can be given as FLQuant objects.  If both are given as FLQuant objects their dimensions must be the same.  If either of these arguments are FLQuant objects, rlnorm returns an FLQuant.
}
\section{Generic function}{
 rlnorm(n,meanlog,sdlog) 
}
\section{Methods}{\describe{
\item{ signature(n=numeric,meanlog=FLQuant,sdlog=FLQuant) :}{Generates random deviates for the log normal distribution.  FLQuant arguments must have the same dimension.  Returns an FLQuant object.}
\item{ signature(n=numeric,meanlog=FLQuant,sdlog=numeric) :}{Generates random deviates for the log normal distribution.  Returns an FLQuant object.}
\item{ signature(n=numeric,meanlog=numeric,sdlog=FLQuant) :}{Generates random deviates for the log normal distribution.  Returns an FLQuant object.}
\item{ signature(n=numeric,meanlog=FLQuantPoint,sdlog=missing) :}{Generates random deviates for the log normal distribution.  Returns an FLQuant object.}
}}
\author{The FLR Team}
\seealso{ \link[stats]{rlnorm}, \link{FLQuant-class}, \link{FLQuantPoint-class}  }
\examples{
out <- rlnorm(1000, meanlog=FLQuant(c(5,5,5,5,5)),sdlog=FLQuant(c(0,1,2,3,4)))
apply(log(out),2,sd)
apply(log(out),2,mean)
}
\keyword{methods}
