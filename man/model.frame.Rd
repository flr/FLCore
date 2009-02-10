%$Id$
\name{model.frame}
\docType{methods}
\alias{model.frame,FLlst-method}
\alias{model.frame,FLComp-method}
\title{Method model.frame}
\description{
 \code{model.frame} returns a \code{\link[base]{data.frame}} with the variables in a wide 
 format, to be used by a \code{formula} in any model method.
}
\section{Generic function}{
 model.frame(formula) 
}
\section{Methods}{\describe{
\item{ signature(formula=FLlst) :}{Returns a wide data.frame}
\item{ signature(formula=FLComp) :}{Returns a wide data.frame}
}}
\author{The FLR Team}
\seealso{ \link[base]{model.frame}, \link{FLQuants-class}, \link{FLlst-class}}
\examples{
data(ple4)
flqs <- FLQuants(stock=stock.n(ple4), catch=catch.n(ple4))
fmf <- model.frame(flqs)
head(fmf)}
\keyword{methods}
