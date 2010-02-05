\name{update}
\docType{methods}
\alias{update,FLModel-method}
\title{Method update}
\description{
 \code{update} is a generic function for updating a model fitting using the same call
 that generated it. Input arguments can be provided that will alter the FLModel object 
 accordingly.
}
\section{Generic function}{
 update(object, ...)
}
\section{Methods}{\describe{
\item{ signature(object=FLModel) :}{Rerun using details(object)[['call']]}
}}
\author{The FLR Team}
\seealso{\linkS4class{FLModel}, \link[stats]{update}}
\examples{
\dontrun{
data(nsher)
nsher <- update(nsher, ssb=ssb(nsher) * 1.4)
}}
\keyword{methods}
