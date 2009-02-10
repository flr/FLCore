\name{FLCatches}
\docType{methods}
\alias{FLCatches}
\alias{FLCatches-methods}
\alias{FLCatches,ANY-method}
\alias{FLCatches,missing-method}
\alias{FLCatches,list-method}
\title{Method FLCatches}
\description{
 Creator method for FLCatches object
}
\section{Generic function}{
 FLCatches(object) 
}
\section{Methods}{\describe{
\item{ signature(object=ANY) :}{create a FLMetiers object from a set of FLMetier objects}
\item{ signature(object=missing) :}{create a FLMetiers object as a void list for future use}
\item{ signature(object=list) :}{create a FLMetiers object from a list of FLMetier objects}
}}
\author{The FLR Team}
\seealso{ \link{FLCatch}  \link{FLComp} }
\examples{
catch1 <- FLCatch(name='catch1')
catch2 <- FLCatch(name='catch2')
lst.catches <- FLCatches(catch1,catch2)
summary(lst.catches)
lapply(lst.catches,summary)                      

}
\keyword{methods}
