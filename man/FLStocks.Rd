\name{FLStocks}
\docType{methods}
\alias{FLStocks}
\alias{FLStocks-methods}
\alias{FLStocks,ANY-method}
 \alias{FLStocks,missing-method}
 \alias{FLStocks,list-method}
\title{Method FLStocks}
\description{
 Method for creating an object of class \code{FLStocks}.
}
\section{Generic function}{
 FLStocks(object) 
}
\section{Methods}{\describe{
\item{ signature(object=ANY) :}{Returns an FLStocks object with the given named elements}
\item{ signature(object=missing) :}{Returns an FLStocks object}
\item{ signature(object=list) :}{Returns an FLStocks object with the provided list as its core}
}}
\author{The FLR Team}
\seealso{\link{FLStocks-class},  \link{FLlst-class}, \link{FLStock-class}}
\examples{
stock <- window(ple4, 1980, 2000)
summary(FLStocks(ple4=ple4, shortple4=stock))
}
\keyword{methods}
