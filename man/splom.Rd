\name{splom}
\docType{methods}
\alias{splom,FLPar,missing-method}
\title{Method splom}
\description{
  Draws a conditional scatter plot matrix.

 See the help page in \code{\link[lattice]{lattice}} for a full description of each plot and all
 possible arguments.
}
\section{Generic function}{
 splom(x,data) 
}
\section{Methods}{\describe{
\item{ signature(x=FLPar,data=missing) :}{Conditional scatter plot matrix for all combinations of \emph{params}.}
}}
\author{The FLR Team}
\seealso{ \link[lattice]{splom} }
\examples{
data(nsher)
splom(params(nsher))
}
\keyword{methods}
