\name{FLMetiers}
\docType{methods}
\alias{FLMetiers}
\alias{FLMetiers-methods}
\alias{FLMetiers,ANY-method}
 \alias{FLMetiers,missing-method}
 \alias{FLMetiers,list-method}
\title{Method FLMetiers}
\description{
 Creator method for FLMetiers object
}
\section{Generic function}{
 FLMetiers(object) 
}
\section{Methods}{\describe{
\item{ signature(object=ANY) :}{create a FLMetiers object from a set of FLMetier objects}
\item{ signature(object=missing) :}{create a FLMetiers object as a void list for future use}
\item{ signature(object=list) :}{create a FLMetiers object from a list of FLMetier objects}
}}
\author{The FLR Team}
\seealso{   \link{FLMetier} \link{FLCatches} \link{FLCatch} \link{FLComp} }
\examples{

met1 <- FLMetier(name='met1',catches=FLCatches(FLCatch()))
met2 <- FLMetier(name='met2',catches=FLCatches(FLCatch()))
class(met1)
lst.metiers <- FLMetiers(met1,met2)
summary(lst.metiers)
lapply(lst.metiers,summary)

}
\keyword{methods}
