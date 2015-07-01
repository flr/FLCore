#' Class FLModel
#'
#' A virtual class for statistical models
#'
#' The \code{FLModel} class provides a virtual class that developers of various
#' statistical models can use to implement classes that allow those models to
#' be tested, fitted and presented.
#' 
#' Slots in this class attempt to map all the usual outputs for a modelling
#' exercise, together with the standard inputs. Input data are stored in slots
#' created by a specified class that is based on \code{FLModel}. See for example
#' \code{\linkS4class{FLSR}} for a class used for stock-recruitment models.
#' 
#' Various fitting algorithms, similar to those present in the basic R packages,
#' are currently available for \code{FLModel}, including \code{\link{fmle}},
#' \code{\link{nls-FLCore}} and \code{\link[stats]{glm}}.
#' 
#' @name FLModel
#' @aliases FLModel-class FLModel FLModel-methods FLModel,formula-method
#' FLModel,missing-method FLModel,character-method FLModel,function-method
#' @docType class
#' @section Slots: \describe{ \item{name}{Name of the object.
#' \code{character}.} \item{desc}{Description of the object. \code{character}.}
#' \item{range}{Range. \code{numeric}.} \item{fitted}{Estimated values for rec.
#' \code{FLQuant}.} \item{residuals}{Residuals obtained from the model fit.
#' \code{FLQuant}.} \item{model}{Model formula. \code{formula}.}
#' \item{gr}{Function returning the gradient of the likelihood.
#' \code{function}.} \item{logl}{Log-likelihood function. \code{function}.}
#' \item{initial}{Function returning initial parameter values for the
#' optimizer, as an object of class \code{FLPar}. \code{function}.}
#' \item{params}{Estimated parameter values. \code{FLPar}.} \item{logLik}{Value
#' of the log-likelihood. \code{logLik}.} \item{vcov}{Variance-covariance
#' matrix. \code{array}.} \item{hessian}{Hessian matrix obtained from the
#' parameter fitting. \code{array}.} \item{details}{extra information on the
#' model fit procedure. \code{list}.} }
#' @author The FLR Team
#' @seealso \link[stats]{AIC}, \link[stats4]{BIC}, \link{fmle},
#' \link[stats]{nls}, \link{FLComp}
#' @keywords classes
#' @examples
#' 
#' # Normally, FLModel objects won't be created if "class" is not set
#'   summary(FLModel(length~width*alpha))
#' 
#' # Objects of FLModel-based classes use their own constructor,
#' # which internally calls FLModel
#'   fsr <- FLModel(rec~ssb*a, class='FLSR')
#'   is(fsr)
#'   summary(fsr)
#' 
#' # An example constructor method for an FLModel-based class
#'   # Create class FLGrowth with a single new slot, 'mass'
#'     setClass('FLGrowth', representation('FLModel', mass='FLArray'))
#'
#'   # Define a creator method based on FLModel
#'     setMethod('FLGrowth', signature(object='ANY'),
#'       function(object, ...) return(FLModel(object, ..., class='FLGrowth')))
#'     setMethod('FLGrowth', signature(object='missing'),
#'       function(...) return(FLModel(formula(NULL), ..., class='FLGrowth')))
#'
#'   # Define an accessor method
#'     setMethod('mass', signature(object='FLGrowth'),
#'       function(object) return(object@mass))
#'