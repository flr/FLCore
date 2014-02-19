\name{pv}
\docType{methods}
\alias{pv}
\alias{pv-methods}
\alias{pv,FLQuant-method}
\title{Population variability}
\description{
  The \code{pv} method computes the population variability (\emph{pv}) of an 
  \code{FLQuant} object.
}
\section{Generic function}{
 pv(object) 
}
\section{Methods}{\describe{
\item{ signature(object=FLQuant) :}{Describe method}
}}
\references{
  Heath, J.P. 2006. Quantifying temporal variability in population abundances. \emph{Oikos} \bold{115 (3)}: 573--581.
}
\author{The FLR Team}
\seealso{ \link{FLComp} }
\examples{
flq <- FLQuant(rnorm(40), dim=c(1,40))
pv(flq)

data(ple4)
pv(stock(ple4))
}
\keyword{methods}
