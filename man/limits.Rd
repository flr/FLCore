\name{limits}
\docType{methods}
\alias{upper}
\alias{upper,FLModel-method}
\alias{upper<-}
\alias{upper<-,FLModel,numeric-method}
\alias{lower}
\alias{lower,FLModel-method}
\alias{lower<-}
\alias{lower<-,FLModel,numeric-method}
\title{Methods upper and lower}
\description{
  Accesor and replacement methods for the \code{lower} and \code{upper} attributes of
  objects of class \code{\link{FLModel-class}}. These are stored as part of the structure
  inside the \code{initial} slot. This slot contains a function to be used to provide 
  initial values to any of the fitting method (e.g. \code{\link{fmle}}).

  The values in \code{lower} and \code{upper} are only used if the method selected for
  \code{\link[stats]{optim}} is able to make use of them, like for example "L-BFGS-B", 
  which is the default for \code{\link{fmle}}.

  The exact location of this information could be changed (i.e. a separate slot might be
  created), so code accessing it is encouraged to use these accesor methods.

}
\section{Generic function}{
upper(object) 
upper<-(object, value) 
lower(object) 
lower<-(object, value) 
}
\section{Methods}{\describe{
\item{signature(object=FLModel) :}{Describe method}
\item{signature(object=FLModel, value=numeric) :}{Describe method}
}}
\author{The FLR Team}
\seealso{ \link{FLModel-class} }
\examples{
data(nsher)
lower(nsher)
upper(nsher)
}
\keyword{methods}
