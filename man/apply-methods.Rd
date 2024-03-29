% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/FLArray.R, R/FLPar.R, R/jackknife.R
\name{apply,FLArray,numeric,function-method}
\alias{apply,FLArray,numeric,function-method}
\alias{apply,FLPar,ANY,ANY-method}
\alias{apply,FLQuantJK,numeric,function-method}
\alias{apply,FLParJK,numeric,function-method}
\title{apply method for FLCore classes}
\usage{
\S4method{apply}{FLArray,numeric,function}(X, MARGIN, FUN, ..., simplify = TRUE)

\S4method{apply}{FLPar,ANY,ANY}(X, MARGIN, FUN, ..., simplify = TRUE)

\S4method{apply}{FLQuantJK,numeric,function}(X, MARGIN, FUN, ..., simplify = TRUE)

\S4method{apply}{FLParJK,numeric,function}(X, MARGIN, FUN, ..., simplify = TRUE)
}
\description{
Applies a function over the margins of an array-based FLCore class
}
\details{
These methods call R's \link[base:apply]{base::apply} on an \link{FLArray} the standard arithmetic operators included in the
\code{\link[methods]{Arith}} group ("+", "-", "*", `"^", "\%\%", "\%/\%", and
"/"), so that they return an object of the appropriate class.

When the operation involves objects of two classes (e.g. \code{\link{FLPar}} and \code{\link{FLQuant}}),
the class is the returned object is that of the more complexs object, in this
case \code{\link{FLQuant}}.
}
\examples{

flq <- FLQuant(rlnorm(90), dim=c(3,10), units='kg')
flp <- FLPar(a=99)

# FLQuant and numeric
flq * 25
# Two FLQuant objects
flq + flq

}
\seealso{
\link[base:apply]{base::apply}
}
\author{
The FLR Team
}
\keyword{methods}
