\name{FLModel}
\docType{class}
\alias{FLModel-class}
\title{Class FLModel for statistical models}
\description{
  The FLModel class provides a virtual class that developers of various statistical
  models can use to implement classes that allow those models to be tested, fitted
  and presented.

  Slots in this class attempt to map all the usual outputs for a modelling
  exercise, together with the standard inputs. Input data is stored in slots
  created by each of those classes based on FLModel.See, for example
  \code{\link{FLSR-class}} for a class used for stock-recruitment models.

  Various fitting algorithms, similar to those present in the basic R packages are 
  currently available for FLModel, including \code{\link{fmle}}, \code{\link{nls-FLCore}} 
  and \code{\link[stats]{glm}}.
}
\section{Slots}{\describe{
\item{name}{Name of the object. \code{character}.}
\item{desc}{Description of the object. \code{character}.}
\item{range}{Range. \code{numeric}.}
\item{fitted}{Estimated values for rec. \code{FLQuant}.}
\item{residuals}{Residuals obtained from the model fit. \code{FLQuant}.}
\item{model}{Model formula. \code{formula}.}
\item{grad}{Function returning the gradient of the model. \code{function}.}
\item{logl}{Log-likelihood function. \code{function}.}
\item{initial}{Function returning initial parameter values for the optimizer. \code{function}.}
\item{params}{Estimated parameter values. \code{FLPar}.}
\item{logLik}{Value of the log-likelihood. \code{logLik}.}
\item{vcov}{Variance-covariance matrix. \code{array}.}
\item{details}{extra information on the model fit procedure. \code{list}.}
}}
\section{Extends}{\describe{
FLComp
}}
\section{Methods}{\describe{
\item{\link[stats]{AIC}(stats) :}{Describe method}
\item{}{\code{ signature(object=FLModel,k=numeric) }}
\item{\link[stats]{AIC}(stats) :}{Describe method}
\item{}{\code{ signature(object=FLModel,k=missing) }}
\item{\link[stats4]{BIC}(stats4) :}{Describe method}
\item{}{\code{ signature(object=missing) }}
\item{\link{fmle}(FLCore) :}{Describe method}
\item{}{\code{ signature(object=missing,start=missing) }}
\item{\link[stats]{nls}(stats) :}{Describe method}
\item{}{\code{ signature(formula=FLModel,data=missing,start=missing,control=missing,algorithm=missing,trace=missing,subset=missing,weights=missing,na.action=missing,model=missing,lower=missing,upper=missing) }}
}}
\author{The FLR Team}
\seealso{ \link[stats]{AIC}, \link[stats4]{BIC}, \link{fmle}, \link[stats]{nls}, \link{FLComp} }
\examples{}
\keyword{classes}
