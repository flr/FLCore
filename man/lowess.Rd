\name{lowess}
\docType{methods}
\alias{lowess,FLSR,missing-method}
\title{Method lowess}
\description{
  LOWESS smoother based on locally-weighted polynomial regression for objects of class
  \code{\link{FLSR-class}}. The model fitted is of the form rec(x)~ssb(x).
}
\section{Generic function}{
 lowess(x, y, f=2/3, iter=3, delta=0.01 * diff(range(xy\$x[o])))

}
\section{Methods}{\describe{
\item{signature(x=FLSR,y=missing) :}{lowessb(x, y, f=2/3, iter=3, delta=0.01 * diff(range(ssb(x))))
}
}}
\author{The FLR Team}
\seealso{ \link[stats]{lowess} }
\examples{
# use the North Sea herring SR dataset
data(nsher)

# fitting a rec ~ ssb lowess
nsher <- lowess(nsher)

summary(nsher)

\dontrun{
plot(nsher)
}



}
\keyword{methods}
