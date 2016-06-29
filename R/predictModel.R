# predictModel.R - DESC
# predictModel.R

# Copyright 2015 Iago Mosqueira. Distributed under the GPL 2.
# Maintainer: Iago Mosqueira, EC JRC G03

# class predictModel: model + data + params {{{
#' A class for model prediction
#'
#' Object of the predictModel class are used in various FLR classes to allow
#' flexible modelling of the dynamics of different biological and technological
#' processes.
#'
#' The dependency of life history processes, such as maturity and fecundity, to biological
#' and environmental factors, can be represented in objects of this class via a simple model
#' (represented by a `formula`) and the corresponding paramaters (`FLPar`) and inputs
#' (`FLQuants`).
#'
#' @name predictModel
#' @rdname predictModel
#' @docType class
#' @aliases predictModel predictModel-methods predictModel-class
#'
#' @section Slots:
#'     \describe{
#'     \item{.Data}{Inputs to the model not found in enclosing class (\code{FLQuants}).}
#'     \item{model}{Model representation (\code{formula}).}
#'     \item{params}{Model paramaters (\code{FLPar}).}
#' }
#'
#' @section Validity:
#'
#'   \describe{
#'     \item{VALIDITY}{Neque porro quisquam est qui dolorem ipsum.}
#' }
#'
#' You can inspect the class validity function by using
#'    \code{getValidity(getClassDef('predictModel'))}
#'
#' @section Accessors:
#' All slots in the class have accessor and replacement methods defined that
#' allow retrieving and substituting individual slots.
#'
#' The values passed for replacement need to be of the class of that slot.
#' A numeric vector can also be used when replacing FLQuant slots, and the
#' vector will be used to substitute the values in the slot, but not its other
#' attributes.
#'
#' @section Constructor:
#' A construction method exists for this class that can take named arguments for
#' any of its slots. All slots are then created to match the requirements of the
#' class validity.
#'
#' @section Methods:
#' Methods exist for various calculations based on values stored in the class:
#'
#' \describe{
#'     \item{METHOD}{Neque porro quisquam est qui dolorem ipsum.}
#' }
#'
#' @author The FLR Team
#' @seealso \code{\link{FLQuants}} \code{\link{FLPar}} \code{\link{FLBiol}}
#' @keywords classes
#' @examples
#'
setClass('predictModel',
	contains='FLQuants',
	representation(params='FLPar', model='formula'),
    prototype(FLQuants(), params=FLPar(), model=~NA)) # }}}

# predictModel() {{{
setMethod('predictModel', signature(object='FLQuants'),
	function(object, model=~NA, params=FLPar()) {

	# CREATE object
	res <- new("predictModel", object, model=model, params=params)
	
	return(res)
	}
)

setMethod('predictModel', signature(object='missing'),
	function(object, ...) {

	args <- list(...)

	# PARSE for named FLQuant(s)
	if(length(args) > 0) {
		cls <- unlist(lapply(args, is, 'FLQuant'))
		fqs <- do.call('FLQuants', args[cls])
		args <- c(list(object=fqs), args[!cls])
	} else {
		args <- list(object=FLQuants())
	}

	# CREATE object
	res <- do.call('predictModel', args)
	
	return(res)
	}
) # }}}

# model, model<- {{{
setMethod('model', signature(object='predictModel'),
	function(object) {
		return(object@model)
	}
)
setReplaceMethod('model', signature(object='predictModel', value='formula'),
	function(object, value) {
		object@model  <- value
		return(object)
	}
) # }}}

# params, params<- {{{
setMethod('params', signature(object='predictModel'),
	function(object) {
		return(object@params)
	}
)
setReplaceMethod('params', signature(object='predictModel', value='FLPar'),
	function(object, value) {
		object@params  <- value
		return(object)
	}
) # }}}

# predict(predictModel) {{{
setMethod('predict', signature(object='predictModel'),
	function(object) {
		return(eval(object@model[[length(object@model)]], c(object, as(object@params, 'list'))))
	}
) # }}}

# summary {{{
setMethod('summary', signature(object='predictModel'),
	function(object) {
		summary(object)
		print(names(object))
		print(object@model)
	}
) # }}}

# [, [[<- {{{
setMethod('[', signature(x="predictModel", i="ANY", j="missing"),
	function(x, i) {
		x@.Data <- x@.Data[i]
		return(x)
	}
)
# }}}

# show {{{
setMethod('show', signature(object='predictModel'),
	function(object) {
		
		callNextMethod()
		cat("model: ", "\n")
		show(object@model)
		cat("\nparams: ", "\n")
		show(object@params)
	}
) # }}}

# evalPredictModel {{{

#' Evaluates a predictModel slot inside the object cointaining it
#'
#' Models in objects of the \link{predictModel} class can make use of slots and
#' methods of the FLR class in which it is contained as a slot. This function can
#' be used by methods wishing to evaluate a single `predictModel` slot in the
#' context of the class it isd part of.
#'
#' @param object The FLR S4 object holding the `predictModel` slot.
#' @param slot The name of the slot to be evaluated, as a character.
#'
#' @return The result of evaluating the model, usually an `FLQuant`
#'
#' @name evalPredictModel
#' @rdname evalPredictModel
#' @aliases evalPredictModel
#'
#' @author The FLR Team
#' @seealso \link{predictModel}
#' @keywords utilities
#' @examples
#' 
evalPredictModel <- function(object, slot='fec') {

  # EXTRACT slot
  slot <- slot(object, slot)

  lis <- list()

  # EXTRACT expression to evaluate
  args <- all.names(slot@model[[length(slot@model)]], functions=FALSE)

  # (1) EXTRACT from FLQuants

  # MATCH names
  idx <- names(slot) %in% args
  
  # EXTRACT
  if(any(idx)) {
    lis <- slot@.Data[idx]
    names(lis) <- names(slot)[idx]
    
    # DROP extracted args
    args <- args[!args %in% names(lis)]
  }

  # (2) FLPar
  pars <- as(slot@params, 'list')
  idx <- names(pars) %in% args
  if(any(idx)) {
    lis <- c(lis, as(slot@params, 'list')[idx])
    
    # DROP extracted args
    args <- args[!args %in% names(lis)]
  }

  # (3) CALL methods on object (inc. accessors)
  if(length(args) > 0)
    for(i in args)
      lis[[i]] <- do.call(i, list(object))

  # RETURN
  return(eval(slot@model[[length(slot@model)]], lis))
} # }}}

