\name{show}
\docType{methods}
\alias{show,FLArray-method}
\alias{show,FLQuant-method}
\alias{show,FLQuantPoint-method}
\alias{show,FLQuants-method}
\title{Method show}
\description{
  Standard display of an object contents in an interactive session. Objects of class
  \code{\link{FLQuant-class}} with length > 1 along the sixth dimension (\emph{iter})
  are output in a summarised form, as \code{median (mad)}, where mad is the median 
  absolute deviation. See \code{\link[stats]{mad}}.
}
\section{Generic function}{
 show(object) 
}
\section{Methods}{\describe{
\item{ signature(object=FLQuant) :}{Describe method}
\item{ signature(object=FLQuantPoint) :}{Describe method}
\item{ signature(object=FLQuants) :}{Describe method}
}}
\author{The FLR Team}
\seealso{ \link{FLComp} }
\examples{
# no 'iter'
flq <- FLQuant(rnorm(80), dim=c(4,20), quant='age', units='kg')
flq

# with 'iter'
flq <- FLQuant(rnorm(800), dim=c(4,20,1,1,1,10), quant='age', units='kg')
flq
}
\keyword{methods}
