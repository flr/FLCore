%$Id$
\name{apply}
\docType{methods}
\alias{apply,ANY,missing,missing-method}
\alias{apply,FLQuant,numeric,function-method}
\title{Method apply}
\description{
	Functions can be applied to margins of an FLQuant array using this method. In contrast 
	with the standard method, dimensions are not collapsed in the output object.

	\code{FUN} in the case of an \code{FLQuant} must collapse at least one dimension when
	applied over an array.

 	For further details see \link[base]{apply}.
}
\section{Generic function}{
 apply(X,MARGIN,FUN) 
}
\section{Method arguments}{\describe{
\item{\code{X} :}{an object of a class for which this method has been defined.}
\item{\code{MARGIN} :}{a vector giving the subscripts which the function will be applied
	over. 1 indicates rows, 2 indicates columns, c(1,2) indicates rows and columns, etc.}
\item{\code{FUN} :}{the function to be applied.}
\item{\code{...} :}{optional arguments to FUN.}
}}
\section{Methods}{\describe{
\item{ signature(X=ANY,MARGIN=missing,FUN=missing) :}{New S4 generic based on base::apply}
\item{ signature(X=FLQuant,MARGIN=numeric,FUN=function) :}{apply a given function over the
	selected dimensions of an \link{FLQuant-class}. Returns an object of class \link{FLQuant-class}.}
}}
\author{The FLR Team}
\seealso{ \link[base]{apply} }
\examples{
flq <- FLQuant(rlnorm(100), dim=c(10,20,1,1,1,5))
apply(flq, 1, sum)
apply(flq, 2:6, sum)
}
\keyword{methods}
