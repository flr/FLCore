\name{FLModel}
\docType{class}
\alias{FLModel-class}
\alias{FLModel}
\alias{FLModel-methods}
\alias{FLModel,formula-method}
\alias{FLModel,missing-method}
\alias{FLModel,character-method}
\alias{FLModel,function-method}

\title{Class FLModel for statistical models}
\description{
  The FLModel class provides a virtual class that developers of various statistical
  models can use to implement classes that allow those models to be tested, fitted
  and presented.

  Slots in this class attempt to map all the usual outputs for a modelling
  exercise, together with the standard inputs. Input data is stored in slots
  created by each of those classes based on FLModel.See, for example
  \code{\linkS4class{FLSR}} for a class used for stock-recruitment models.

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
\item{gr}{Function returning the gradient of the likelihood. \code{function}.}
\item{logl}{Log-likelihood function. \code{function}.}
\item{initial}{Function returning initial parameter values for the optimizer, as an object of class \code{FLPar}. \code{function}.}
\item{params}{Estimated parameter values. \code{FLPar}.}
\item{logLik}{Value of the log-likelihood. \code{logLik}.}
\item{vcov}{Variance-covariance matrix. \code{array}.}
\item{hessian}{Hessian matrix obtained from the parameter fitting. \code{array}.}
\item{details}{extra information on the model fit procedure. \code{list}.}
}}
\section{Extends}{\describe{
FLComp
}}
\section{Constructor}{
\describe{
\item{}{Constructor method for objects of class \code{\linkS4class{FLModel}}. This method is to be called by the constructor methods of classes extending \code{FLModel}.}
\item{}{An argument \code{class} instructs the constructor about the exact class of the returned object. Constructor methods for \code{FLModel}-based class should simply invoke this method with the appropriate class argument (See example below).}
\item{ signature(object=formula) :}{First argument is a formula describing the model, to be placed in the \code{model} slot.}
\item{ signature(object=missing) :}{Arguments, if given, are parsed and allocated by name to a iven slot. If none if provided, and empty \code{FLModel} object is returned.}
\item{ signature(object=function) :}{A function returning a list with names equal to one or more of the slots in the class is called, and elements in that list are allocated by name. See \code{\link{SRModels}} for uses of this mechanism}
\item{ signature(object=character) :}{A function with his name is called, as above.}
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
\examples{
# Normally, FLModel objects won't be created, as class lacks input slots
summary(FLModel(length~width*alpha))

# Objects of FLModel-based classes use their own constructor,
# which internally calls FLModel
fsr <- FLModel(rec~ssb*a, class='FLSR')
is(fsr)
summary(fsr)

# An example constructor method for an FLModel-based class
# create FLGrowth class with a single new slot, 'mass'
setClass('FLGrowth', representation("FLModel",
  mass='FLArray'))

# define creator method, based on FLModel()
setGeneric('FLGrowth', function(object, ...)
		standardGeneric('FLGrowth'))
setMethod('FLGrowth', signature(object='ANY'),
  function(object, ...)
    FLModel(object, class='FLGrowth', ...))
}
\keyword{classes}
