\name{Arith}
\docType{methods}
\alias{Arith,FLArray,FLArray-method}
\alias{Arith,numeric,FLArray-method}
\alias{Arith,FLArray,numeric-method}
\alias{Arith,FLQuant,FLQuant-method}
\alias{Arith,FLCohort,FLCohort-method}
\alias{Arith,FLQuants,FLQuants-method}
\title{Arithmetic methods for FLQuant objects}
\description{
	The \code{Arith} group of methods, comprising addition, substraction, product, division,
	exponentiation, and integer division (\code{+}, \code{-}, \code{*}, \code{/}, \code{^},
	\code{\%\%} and \code{\%/\%}). These methods work exactly as in an object of class 
	\code{array}, but always return an \code{FLQuant} object.
}
\section{Generic function}{
 Arith(e1,e2) 
}
\section{Methods}{\describe{
\item{ signature(e1=FLQuant,e2=FLQuant) :}{Operations between two FLQuant objects}
}}
\author{The FLR Team}
\seealso{\code{\link[base]{Arithmetic}}, \code{\link[methods]{Arith}}}
\examples{
flq <- FLQuant(rnorm(10), dim=c(2,5))
fl2 <- FLQuant(2, dim=c(2,5))
flq*fl2
flq/fl2}
\keyword{methods}
