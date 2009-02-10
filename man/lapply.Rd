\name{lapply}
\docType{methods}
\alias{lapply,FLlst,missing-method}
\alias{lapply,FLlst-method}
\title{Method lapply}
\description{
 lapply returns a list of the same length as X, each element of which is the result of applying FUN to the corresponding element of X. 
}
\section{Generic function}{
 lapply(X,FUN) 
}
\section{Methods}{\describe{
\item{ signature(X=FLlst,FUN=missing) :}{lapply returns a list or FLlst of the same length as X, each element of which is the result of applying FUN to the corresponding element of X of class FLlst.}
}}
\author{The FLR Team}
\seealso{ \link{FLComp} }
\examples{
# On an FLQuants object
flqs <- FLQuants(a=FLQuant(1:10), b=FLQuant(1:20))
# lapply could return another FLQuants object
lapply(flqs, yearSums)
# or a simple list, depending on the function  being called
lapply(flqs, dim)
}
\keyword{methods}
