\name{FLModel}
\docType{methods}
\alias{FLModel}
\alias{FLModel-methods}
\alias{FLModel,formula-method}
\alias{FLModel,missing-method}
\alias{FLModel,character-method}
\alias{FLModel,function-method}
\title{Method FLModel}
\description{
  Constructor method for objects of class \code{\link{FLModel-class}}. This method is to
  be called by the constructor methods of classes extending \code{FLModel}.

  An argument \code{class} instructs the constructor about the exact class of the 
  returned object. Constructor methods for \code{FLModel}-based class should simply
  invoke this method with the appropriate class argument (See example below).
}
\section{Generic function}{
 FLModel(object) 
}
\section{Methods}{\describe{
\item{ signature(object=formula) :}{First argument is a formula describing the model, to be placed in the \code{model} slot.}
\item{ signature(object=missing) :}{Arguments, if given, are parsed and allocated by name to a iven slot. If none if provided, and empty \code{FLModel} object is returned.}
\item{ signature(object=function) :}{A function returning a list with names equal to one or more of the slots in the class is called, and elements in that list are allocated by name. See \code{\link{SRModels}} for uses of this mechanism}
\item{ signature(object=character) :}{A function with his name is called, as above.}
}}
\author{The FLR Team}
\seealso{ \link{FLComp} }
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
\keyword{methods}
