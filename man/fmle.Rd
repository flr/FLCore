\name{fmle}
\docType{methods}
\alias{fmle}
\alias{fmle,ANY,missing-method}
\alias{fmle,FLModel,ANY-method}
\alias{fmle,FLModel,FLPar-method}
\alias{fmle,FLModel-method}
\title{Method fmle}
\description{
 The \code{fmle} method fits the model specified in an \code{FLModel} object using MLE
 by minimizing the negative of the log-likelihood function, in the \code{logl} slot,
 through calls to the \code{\link[stats]{optim}} minimizaton routine.

 For a given model and log-likelihood function, the \code{fmle} method will use the 
 \code{optim} function in R to calculate the parameter vector which maximises the log-
 likelihood (and, hence,  the likelihood function) and is as such the optimum parameter 
 value for the given problem  and data.

 Be advised that for non-informative of conflicting data the maximum likelihood estimate 
 can  be dependent on the initial starting value and if we begin the optimiser with a 
 poor initial estimate it may converge falsely. Always try multiple start points and be 
 assured that you ave found the true MLE.
}
\section{Generic function}{
 fmle(object,start) 
}
\section{Methods}{\describe{
\item{ signature(object=ANY,start=missing) :}{Generic method.}
\item{ signature(object=FLModel,start=ANY) :}{Input object of class \code{FLModel} contains input data, logl function and function to provide initial values.}
\item{ signature(object=FLModel,start=FLPar) :}{Input object of class \code{FLModel} contains input data and logl function, but fitting is started from parameter estimates in the \code{FLPar} object provided.}
}}
\author{The FLR Team}
\seealso{ \link{FLComp} }
\examples{
# use an example FLModel object
data(nsher)

summary(nsher)

# inspect the logl function
logl(nsher)

# and the function providing initial values to the optimizer
initial(nsher)

# lower and upper limits for the parameters are set, and used if method
# 'L-BFGS-B' is used in the call to optim, as is default in fmle
lower(nsher)
upper(nsher)

# fit it with fmle
nsher <- fmle(nsher)

# fixed values can be chosen for any parameter
nsher_fixed_a <- fmle(nsher, fixed=list(a=125))

# and results compared, for example using AIC
AIC(nsher)
AIC(nsher_fixed_a)

\dontrun{
# an initial run with one optimization method, e.g. 'SANN'
nsher_one <- fmle(nsher, method='SANN')

# can then be used as starting value for other runs
# This might fail if
nsher_two <- fmle(nsher_one, start=params(nsher_one), method='L-BFGS-B')
}
}
\keyword{methods}
