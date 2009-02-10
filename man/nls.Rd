\name{nls}
\docType{methods}
\alias{nls,FLModel,missing,missing,missing,missing,missing,missing,missing,missing,missing,missing,missing-method}
\alias{nls,FLModel,missing-method}
\alias{nls-FLCore}
\title{Method nls}
\description{
 For a given forumla (describing a model) and data this method applies the simple non-
 linear least squares algorithm - this calculates the parameters that minimisethe sum of 
 squares difference between the observed (data) and predicted (model) values.

 The algorithm can be sensitive to the initial values of the problem so do try different 
 start points and check they converge to the same estimates.
}
\section{Generic function}{
 nls(formula,data,start,control,algorithm,trace,subset,weights,na.action,model,lower,upper) 
}
\section{Methods}{\describe{
\item{ signature(formula=FLModel,data=missing,start=missing,control=missing,algorithm=missing,trace=missing,subset=missing,weights=missing,na.action=missing,model=missing,lower=missing,upper=missing) :}{Applies non-linear sum of squares to
the model and data in the input \link{FLModel-class} object.}
}}
\author{The FLR Team}
\seealso{ \link{FLComp} }
\examples{
# An example FLSR (FLModel) object
data(nsher)

#set bevholt model
model(nsher) <- bevholt

# fit through nls
nsher <- nls(nsher)

summary(nsher)
}
\keyword{methods}
