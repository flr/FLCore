# predictModel.R - DESC
# FLCore/R/predictModel.R

# Copyright 2015-2023 Iago Mosqueira. Distributed under the GPL 2.
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
setClass('predictModel',
	contains='FLQuants',
	representation(params='FLPar', model='formula'),
    prototype(FLQuants(), params=FLPar(), model=as.formula("~NA",
    env=emptyenv()))) # }}}

# predictModel() {{{

#' @rdname predictModel
#' @aliases predictModel,FLQuants,formula-method
#' @examples
#' fec <- FLQuants(fec=FLQuant(rlnorm(10, 20, 5),
#'   dimnames=list(year=2000:2009), units='1'))
#' predictModel(fec, model=~fec)

setMethod('predictModel', signature(object='FLQuants', model="formula"),
	function(object, model, params=FLPar()) {
	
  # CREATE object
	res <- new("predictModel", object, model=model, params=params)

  # DROP model environment
  res@model <- as.formula(format(res@model), env=emptyenv())
	
	return(res)
	}
)

#' @rdname predictModel
#' @aliases predictModel,FLQuants,missing-method
#' @examples
#' predictModel(fec)

setMethod('predictModel', signature(object='FLQuants', model="missing"),
	function(object, params=FLPar()) {

	# CREATE object
  return(predictModel(object=object, model=~NA, params=params))
	}
)

#' @rdname predictModel
#' @aliases predictModel,FLQuants,character-method
#' @examples
#' predictModel(fec, model="bevholt")

setMethod('predictModel', signature(object='FLQuants', model="character"),
	function(object, model, params=FLPar()) {

  # GET model from function
  model <- do.call(model, list())$model

	# CREATE object
  return(predictModel(object=object, model=model, params=params))
	}
)

#' @rdname predictModel
#' @aliases predictModel,FLQuants,function-method
#' @examples
#' predictModel(fec, model=bevholt)

setMethod('predictModel', signature(object='FLQuants', model="function"),
	function(object, model, params=FLPar()) {

  # GET model from function
  model <- do.call(model, list())$model

	# CREATE object
  return(predictModel(object=object, model=model, params=params))
	}
)

#' @rdname predictModel
#' @aliases predictModel,FLQuants,list-method
#' @examples
#' predictModel(fec, model=bevholt())

setMethod('predictModel', signature(object='FLQuants', model="list"),
	function(object, model, params=FLPar()) {

  model <- model$model
	# CREATE object
  return(predictModel(object=object, model=model, params=params))
	}
)

#' @rdname predictModel
#' @aliases predictModel,missing,ANY-method
#' @examples
#' predictModel(model=rec~a*ssb, params=FLPar(a=1.234))
#' predictModel(model=bevholt, params=FLPar(a=1.234))
#' predictModel(model="bevholtss3", params=FLPar(a=1.234))

setMethod('predictModel', signature(object="missing", model="ANY"),
	function(object, model, ...) {

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
	res <- do.call('predictModel', c(args, model=model))
	
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
		object@model  <- as.formula(format(value), env=emptyenv())
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
	function(object, ...) {

    # SPLIT params in FLPars  
    pars <- dimnames(params(object))$params
    if(!all(pars == character(1)))
      args <- setNames(lapply(pars, function(x) params(object)[x,]), nm=pars)
    else
      args <- pars

    # ADD FLQuant(s)
    args <- c(args, object)

    # ADD extra arguments
    extra <- list(...)

    # EXTRACT FLQuant elements
    exflq <- extra[unlist(lapply(extra, is, "FLQuant"))]

    # PARSE any FLPar to list
    expar <- extra[unlist(lapply(extra, is, "FLPar"))]
    if(length(expar))
      expar <- Reduce("c", lapply(expar, as, 'list'))

    # PARSE any numeric to FLQuant
    exnum <- extra[unlist(lapply(extra, is, "numeric"))]
    if(length(exnum))
      exnum <- lapply(exnum, as, 'FLQuant')

    args <- c(args, exflq, expar, exnum)

    # SUBSET args in formula
    fargs <- all.vars(object@model[[length(object@model)]])
    args <- args[fargs]

    # No. iters
    nits <- unlist(lapply(args, function(x) dims(x)$iter))

    if(length(unique(nits[nits > 1])) > 1)
      warning("objects have different number of iters")

    # lapply eval(model) along iters, then combine
    return(Reduce(combine, lapply(seq(max(nits)), function(i)
      eval(object@model[[length(object@model)]], lapply(args, iter, i))))
    )
	}
) # }}}

# summary {{{
#' @rdname summary-methods
#' @aliases summary,predictModel-method
setMethod('summary', signature(object='predictModel'),
	function(object) {

    cat('An object of class "predictModel":\n')

    # model
    cat(as.character(object@model), "\n")
    # FLQuants
    if(length(names(object)) > 0) {
      for(j in names(object)) {
        cat(substr(paste0("  ", j, "          "), start=1, stop=12),
          " : [", dim(object[[j]]),"], units = ",
          object[[j]]@units, "\n")
      }
    }
    # params
    cat("params:\n")
    par <- object@params
    cat(substr(paste0("  ", ifelse(all(sum(!is.na(par)) == 0 & dimnames(par)[[1]] == ""),
      "NA", paste(dimnames(par)[[1]], collapse=", ")),
      "           "), start=1, stop=12), " : [", dim(object@params),
      "], units = ", object@params@units, "\n")
	}
) # }}}

# [ {{{
#' @rdname Extract
#' @aliases [,predictModel,ANY,missing,ANY-method
setMethod('[', signature(x="predictModel", j="missing"),
	function(x, i, k, l, m, n, ..., drop=FALSE) {

    if(!missing(i) && missing(k) && missing(l) && missing(m) && missing(n) &&
      drop) {
      if(is.character(i)) {
        res <- FLQuants(x@.Data[match(i, names(x))])
        names(res) <- i
      } else {
        res <- FLQuants(x@.Data[i])
        names(res) <- names(x)[i]
      }
      return(res)
    }
    
    foo <- selectMethod("[", c("predictModel", "ANY", "ANY", "ANY"))

    args <- list()

		if (!missing(i))
      args <- c(args, list(i=i))
		if (!missing(k))
      args <- c(args, list(k=k))
		if (!missing(l))
      args <- c(args, list(l=l))
		if (!missing(m))
      args <- c(args, list(m=m))
		if (!missing(n))
      args <- c(args, list(n=n))

    return(do.call(foo, c(list(x=x), args)))
  })

setMethod('[', signature(x="predictModel"),
	function(x, i, j, k, l, m, n, ..., drop=FALSE) {
    
   args <- list()

		if (!missing(i))
      args <- c(args, list(i=i))
		if (!missing(j))
      args <- c(args, list(j=j))
		if (!missing(k))
      args <- c(args, list(k=k))
		if (!missing(l))
      args <- c(args, list(l=l))
		if (!missing(m))
      args <- c(args, list(m=m))
		if (!missing(n))
      args <- c(args, list(n=n))
    
    # SUBSET FLQuants
		x@.Data <- lapply(x@.Data, function(p) do.call("[", c(list(x=p), args)))

    # SUBSET FLPar
    dns <- names(x@.Data[[1]])
    pns <- names(x@params)[-c(1,length(dim(x@params)))]
    
    # IF subset dimnames in params
    idx <- letters[seq(9, 9+6)][match(pns, dns)]
    if(length(idx) > 0)
      x@params <- do.call("[", c(list(x=x@params),
        args[idx]))

		return(x)
	}
) # }}}

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
#' context of the class it is part of.
#'
#' @param object The FLR S4 over whicvh the predictModel evaluation should take place
#' @param slot The predictModel object to be evaluated
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
evalPredictModel <- function(object, slot, ...) {

  # EVALUATING list
  lis <- list()

  # EXTRACT expression to evaluate
  args <- unique(all.names(slot@model[[length(slot@model)]],
    functions=FALSE))

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
  parnms <- dimnames(slot@params)$params
  idx <- parnms %in% args

  if(any(idx)) {
    pars <- lapply(parnms, function(x) params(slot)[x,])
    names(pars) <- parnms
    
    lis <- c(lis, pars[idx])
    
    # DROP extracted args
    args <- args[!args %in% names(lis)]
  }

  # (3) CALL methods on object (inc. accessors)
  if(length(args) > 0)
    for(i in args)
      lis[[i]] <- do.call(i, list(object))
  
  # (4) SUBSTITUTE from list(...)
  extra <- list(...)
  lis[names(extra)] <- extra

  # RETURN
  return(eval(slot@model[[length(slot@model)]], lis))
} # }}}

# returnPredictModelSlot {{{
returnPredictModelSlot <- function(object, what=TRUE, slot, ...) {

    # TRUE, compute
    if(isTRUE(what))
      return(evalPredictModel(object, slot=slot(object, slot), ...))
    # character, extract
    else if(is.character(what)) {
      # @model, @param
      if(any(what %in% c("model", "params")))
        return(slot(slot(object, slot), what))
      # FLQuants elements
      else if(length(what) == 1)
        return(slot(object, slot)@.Data[[names(object@mat) == what]])
      # FLQuants element
      else {
        res <- slot(object, slot)@.Data[names(object@mat) == what]
        names(res) <- what
        return(res)
      }
    } else {
      # RETURN whole predictModel
      return(slot(object, slot))
    }
} # }}}

# dims {{{
setMethod("dims", signature(obj="predictModel"),
  function(obj) {

    dims(predict(obj))
  
  }
)# }}}

# window {{{
setMethod("window", signature(x="predictModel"),
  function(x, ...) {
    x@.Data <- window(FLQuants(x@.Data), ...)
    return(x)
  }
) # }}}

# trim {{{
setMethod("trim", signature(x="predictModel"),
  function(x, ...) {
    x@.Data <- lapply(FLQuants(x@.Data), trim, ...)
    return(x)
  }
) # }}}

# propagate {{{
setMethod("propagate", signature(object="predictModel"),
  function(object, iter, fill.iter=TRUE) {

    # FLQuants iter
    fqi <- unlist(lapply(lapply(object, dim), "[", 6), use.names=FALSE)

    # FLPar iter
    fpi <- dim(params(object))
    fpi <- fpi[length(fpi)]

    # RETURN object if all equal to iter
    if(all(c(fqi, fpi) == iter))
      return(object)

    # CHECK single iters in object
    if(any(!c(fqi, fpi) %in% c(1, iter)))
      stop("Existing slots contain iters != to 'iter'")

    # EXPAND FLQs, if not empty
    if(!is.null(fqi)) {
      object@.Data <- lapply(object@.Data, propagate, iter, fill.iter=fill.iter)
    }

    # EXPAND FLP, if not empty
    params(object) <- propagate(params(object), iter=iter, fill.iter=fill.iter)
#    params(object) <- expand(params(object), iter=seq(1, iter),
#      fill.iter=fill.iter)

    return(object)
  }
) # }}}

# iter {{{
setMethod("iter", signature(obj="predictModel"),
	  function(obj, iter) {
    
      res <- callNextMethod()
      obj@.Data <- res@.Data
      params(obj) <- iter(params(obj), iter)
      
      return(obj)
	  }
) # }}}

# iter<- {{{
setMethod("iter<-", signature(object="predictModel", value="predictModel"),
	  function(object, iter, value) {
      
      # FLQuants
      object@.Data <- mapply(function(x, y) {
          iter(x, iter) <- y
          return(x)
        }, object, value, SIMPLIFY=FALSE)

      # FLPar
      iter(object@params, iter) <- value@params

      return(object)
	  }
) # }}}

# combine {{{

setMethod('combine', signature(x='predictModel', y='predictModel'),
  function(x, y, ..., check=FALSE) {

    args <- c(list(x=x, y=y), list(...))

    x@.Data <- do.call(Map, c(f=combine, lapply(args, slot, ".Data")))
    
    x@params <- Reduce(combine, lapply(args, slot, 'params'))

    return(x)

  }
)
# }}}
