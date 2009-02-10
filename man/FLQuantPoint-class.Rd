\name{FLQuantPoint}
\docType{class}
\alias{FLQuantPoint-class}
\title{Class FLQuantPoint}
\description{
  The \code{FLQuantPoint} class summarizes the contents of an \code{FLQuant} object with
  multiple iterations along its sixth dimension using a number of descriptive statistics.

  An object of this class has a set structure along its sixth dimension (\emph{iter}), 
  which will always be of length 5, and with dimnames \emph{mean},  \emph{median},
  \emph{var}, \emph{uppq} and \emph{lowq}. They refer, respectively, to the sample mean,
  sample median, variance, and lower (0.25) and upper (0.75) quantiles.
  
  Objects of this class wil be typically created from an \code{FLQuant}. The various
  statistics are calculated along the \emph{iter} dimension of the original 
  \code{FLQuant} using \code{\link[base]{apply}}.

}
\section{Slots}{\describe{
\item{.Data}{The main array holding the computed statistics. \code{array}.}
\item{units}{Units of measurement. \code{character}.}
}}
\section{Extends}{\describe{
FLQuant
array
}}
\section{Methods}{\describe{
\item{\link{lowq}(FLCore) :}{Accesor for \code{lowq} iteration}
\item{}{\code{ signature(x=FLQuantPoint) }}
\item{\link{lowq<-}(FLCore) :}{Modifies the \code{lowq} iteration}
\item{}{\code{ signature(x=FLQuantPoint,value=FLQuant) }}
\item{\link[base]{mean}(base) :}{Accesor for \code{mean} iteration}
\item{}{\code{ signature(x=FLQuantPoint) }}
\item{\link{mean<-}(FLCore) :}{Modifies the \code{mean} iteration}
\item{}{\code{ signature(x=FLQuantPoint,value=FLQuant) }}
\item{\link[stats]{median}(stats) :}{Accesor for \code{median} iteration}
\item{}{\code{ signature(x=FLQuantPoint,na.rm=missing) }}
\item{\link{median<-}(FLCore) :}{Modifies the \code{median} iteration}
\item{}{\code{ signature(x=FLQuantPoint,value=FLQuant) }}
\item{\link[graphics]{plot}(graphics) :}{Plot an object of the class}
\item{}{\code{ signature(x=FLQuantPoint,y=missing) }}
\item{\link[stats]{quantile}(stats) :}{Accesor for \code{lowq} or \code{uppq} iteration}
\item{}{\code{ signature(x=FLQuantPoint) }}
\item{\link[stats]{rlnorm}(stats) :}{Creates a lognormally-distributed \code{FLQuant} using the information on an \code{FLQuantPoint}}
\item{}{\code{ signature(n=numeric,meanlog=FLQuantPoint,sdlog=missing) }}
\item{\link[stats]{rnorm}(stats) :}{Creates a normally-distributed \code{FLQuant} using the information on an \code{FLQuantPoint}}
\item{}{\code{ signature(n=numeric,mean=FLQuantPoint,sd=missing) }}
\item{\link[methods]{show}(methods) :}{Outputs object in screen}
\item{}{\code{ signature(object=FLQuantPoint) }}
\item{\link[base]{summary}(base) :}{Prints a summary of the object}
\item{}{\code{ signature(object=FLQuantPoint) }}
\item{\link{uppq}(FLCore) :}{Accesor for \code{uppq} iteration}
\item{}{\code{ signature(x=FLQuantPoint) }}
\item{\link{uppq<-}(FLCore) :}{Modifies the \code{uppq} iteration}
\item{}{\code{ signature(x=FLQuantPoint,value=FLQuant) }}
\item{\link[stats]{var}(stats) :}{Accesor for \code{var} iteration}
\item{}{\code{ signature(x=FLQuantPoint,y=missing,na.rm=missing,use=missing) }}
\item{\link{var<-}(FLCore) :}{Modifies the \code{var} iteration}
\item{}{\code{ signature(x=FLQuantPoint,value=FLQuant) }}
}}
\author{The FLR Team}
\seealso{ \link{lowq}, \link{lowq<-}, \link[base]{mean}, \link{mean<-}, \link[stats]{median}, \link{median<-}, \link[graphics]{plot}, \link[stats]{quantile}, \link[stats]{rlnorm}, \link[stats]{rnorm}, \link[methods]{show}, \link[base]{summary}, \link{uppq}, \link{uppq<-}, \link[stats]{var}, \link{var<-}, \link{FLQuant}, \link[base]{array}}
\examples{
flq <- FLQuant(rnorm(2000), dim=c(10,20,1,1,1,200))
flqp <- FLQuantPoint(flq)
summary(flqp)
mean(flqp)
var(flqp)
rnorm(200, flqp)
}
\keyword{classes}
