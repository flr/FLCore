\name{FLQuantPoint-accesors}
\docType{methods}
\alias{FLQuantPoint-accesors}
\alias{lowq}
\alias{lowq-methods}
\alias{lowq,FLQuantPoint-method}
\alias{lowq<-}
\alias{lowq<--methods}
\alias{lowq<-,FLQuantPoint-method}
\alias{mean,FLQuantPoint-method}
\alias{mean<-}
\alias{mean<--methods}
\alias{mean<-,FLQuantPoint-method}
\alias{median,FLQuantPoint,missing-method}
\alias{median,FLQuantPoint-method}
\alias{median<-}
\alias{median<--methods}
\alias{median<-,FLQuantPoint-method}
\alias{uppq}
\alias{uppq-methods}
\alias{uppq,FLQuantPoint-method}
\alias{uppq<-}
\alias{uppq<--methods}
\alias{uppq<-,FLQuantPoint-method}
\alias{var,FLQuantPoint-method}
\alias{var,FLQuantPoint,missing,missing,missing-method}
\alias{var<-}
\alias{var<--methods}
\alias{var<-,FLQuantPoint-method}
\title{Method lowq}
\description{
  These are the accesor and replacement methods for the various elements stored in an 
  \code{\linkS4class{FLQuantPoint}} object along the sixth dimension.
}
\section{Generic function}{
 lowq(x) 
 lowq<-(x,value) 
 mean(x) 
 mean<-(x,value) 
 median(x,na.rm) 
 median<-(x,value) 
 uppq(x) 
 uppq<-(x,value) 
 var(x,y,na.rm,use) 
 var<-(x,value) 
}
\section{Methods}{\describe{
\item{signature(x=FLQuantPoint) :}{Returns the given \emph{iter}}
\item{signature(x=FLQuantPoint, value=FLQuant) :}{Replaces the given \emph{iter} with the 
  \emph{value} FLQuant}
}}
\author{The FLR Team}
\seealso{ \link{FLComp} }
\examples{
flq <- FLQuant(rnorm(2000), dim=c(10,20,1,1,1,200))
flqp <- FLQuantPoint(flq)
mean(flqp)
mean(flqp) <- FLQuant(rnorm(200, 10, 3), dim=c(10,20))
}
\keyword{methods}
