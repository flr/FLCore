\name{lowess}
\docType{methods}
\alias{lowess,FLSR,missing-method}
\title{Method lowess}
\description{
  LOWESS smoother based on locally-weighted polynomial regression for objects of class
  \code{\linkS4class{FLSR}}. The model fitted is of the form rec(x)~ssb(x). Returns an
  object of class \code{FLQuants} with elements named \code{ssb} and \code{rec}.
}
\section{Generic function}{
 lowess(x, y, f=2/3, iter=3, delta=0.01 * diff(range(xy\$x[o])))

}
\section{Methods}{\describe{
\item{signature(x=FLSR,y=missing) :}{lowess(x, y, f=2/3, iter=3, delta=0.01 * diff(range(ssb(x))))
}
}}
\author{The FLR Team}
\seealso{ \link[stats]{lowess}, \code{\linkS4class{FLSR}}}
\examples{
# use the North Sea herring SR dataset
data(nsher)

# fitting a rec ~ ssb lowess
nshlos <- lowess(nsher)
}
\keyword{methods}
