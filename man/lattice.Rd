\name{lattice}
\docType{methods}
\alias{lattice-FLCore}
\alias{barchart,formula,FLQuant-method}
\alias{barchart,formula,FLComp-method}
\alias{bwplot,formula,FLQuant-method}
\alias{bwplot,formula,FLComp-method}
\alias{densityplot,formula,FLPar-method}
\alias{dotplot,formula,FLQuant-method}
\alias{dotplot,formula,FLComp-method}
\alias{histogram,formula,FLQuant-method}
\alias{histogram,formula,FLQuants-method}
\alias{histogram,formula,FLComp-method}
\alias{histogram,formula,FLPar-method}
\alias{stripplot,formula,FLQuant-method}
\alias{stripplot,formula,FLComp-method}
\alias{xyplot,formula,FLQuant-method}
\alias{xyplot,formula,FLQuants-method}
\alias{xyplot,formula,FLCohort-method}
\alias{xyplot,formula,FLComp-method}
\title{Lattice plots}
\description{
 Implementation of Trellis graphics in FLR. Plot methods in the 
 \code{\link[lattice]{lattice}} package are  available for object of class 
 \code{FLQuant}, \code{FLQuants} or those derive from \code{FLComp}.

 See the help page in \code{\link[lattice]{lattice}} for a full description of each plot 
 method and of all possible arguments.

 Plot methods from lattice are called by passing a \link[base]{data.frame} obtained by 
 converting the FLR objects using as.data.frame. For details on this transformation, see 
 \link{as.data.frame-FLCore}.
}
\section{Generic function}{
 barchart(x, data, ...) 

 bwplot(x, data, ...) 

 densityplot(x, data, ...) 

 dotplot(x, data, ...) 

 histogram(x, data, ...) 

 stripplot(x, data, ...) 

 xyplot(x, data, ...) 
}
\section{Methods}{\describe{
\item{ signature(x=formula, data=FLQuant) :}{Use the lattice functionality for objects of class FLQuant}
\item{ signature(x=formula, data=FLQuants) :}{Use the lattice functionality for objects of class FLQuants}
\item{ signature(x=formula, data=FLComp) :}{Use the lattice functionality for objects of class FLComp}
\item{ signature(x=formula, data=FLPar) :}{Use the lattice functionality for objects of class FLPar}
}}
\author{The FLR Team}
\seealso{\link[lattice]{xyplot}, \link[lattice]{barchart}, \link[lattice]{bwplot}, \link[lattice]{densityplot}, \link[lattice]{dotplot}, \link[lattice]{histogram}, \link[lattice]{stripplot}}
\examples{
data(ple4)
# xyplot on FLQuant
xyplot(data~year|age, catch.n(ple4)[, 1:20])

xyplot(data~year|as.factor(age), catch.n(ple4)[, 1:20], type='b', pch=19, cex=0.5)

# bwplot on FLQuant with iter
flq <- rnorm(100, catch.n(ple4)[, 1:20], catch.n(ple4)[,1:20])
bwplot(data~year|as.factor(age), flq)

# now with same style modifications
bwplot(data~year|as.factor(age), flq, scales=list(relation='free',
  x=list(at=seq(1, 20, by=5), labels=dimnames(catch.n(ple4)[,1:20])$year[seq(1, 20,
  by=5)])), cex=0.5, strip = strip.custom(strip.names = TRUE, strip.levels = TRUE,
  var.name='age'))
}
\keyword{methods}
