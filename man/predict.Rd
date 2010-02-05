\name{predict}
\docType{methods}
\alias{predict,FLModel-method}
\title{Method predict}
\description{
  \emph{predict} returns predicted values according to the parameter values and 
  model formula in an \code{\linkS4class{FLModel}} object. If no extra input is given,
  \emph{predict} will use the input values contained in the relevant slots. If any extra
  named argument is provided, this is used instead and the correspodning predicted values
  are returned.
}
\section{Generic function}{
 predict(object, ...)
}
\section{Methods}{\describe{
\item{ signature(object=FLModel) :}{Calculates predicted values according to the fitted model}
}}
\author{The FLR Team}
\seealso{ \link{FLComp} }
\examples{
# nsher FLSR dataset
data(nsher)

# predict with no extra arguments returns the values
# predicted during model fitting
predict(nsher)

# which can also be extracted from the 'fitted' slot
fitted(nsher)

# a different ssb vector can be provided
predict(nsher, ssb=FLQuant(seq(10, 150, by=5)))
}
\keyword{methods}
