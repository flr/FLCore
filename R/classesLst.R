# classesLst.R - DESC
# classesLst.R

# Copyright 2003-2014 FLR Team. Distributed under the GPL 2 or later
# Maintainer: Iago Mosqueira, JRC
# Soundtrack:
# Notes:

# FLlst class{{{
setClass("FLlst", contains="list",
  representation(names="character", desc="character", lock="logical"),
	prototype(lock=FALSE),
	validity=function(object){

  # All elements in the list are validObjects themselves
  if(!all(unlist(lapply(object, validObject))))
	  return("Components must be valid objects themselves (validObject == TRUE)")

	# Everything is fine
	return(TRUE)
}

) # }}}

# FLQuants {{{
# validity
vFLQs <- function(object){
	# Make sure the list contains all items of the same class
    if(!all(unlist(lapply(object, is, 'FLQuant'))))
		return("Components must be FLQuant")
	# Everything is fine
	return(TRUE)
}

#' Class FLQuants
#'
#' \code{FLQuants} is a \code{list} of \code{FLQuant} objects.
#' It is very similar to the standard \code{list} class.
#' It implements a lock mechanism that, when turned on, does
#' not allow the user to increase or decrease the object length.
#' The elements of \code{FLQuants} must all be of class  \code{FLQuant}.
#'
#' @name FLQuants
#' @aliases FLQuants-class FLQuants FLQuants-methods FLQuants,ANY-method
#' FLQuants,missing-method FLQuants,list-method FLQuants,FLQuants-method
#' @docType class
#' @section Slots: \describe{
#'     \item{.Data}{The data. \code{list}.}
#'     \item{names}{Names of the list elements. \code{character}.}
#'     \item{desc}{Description of the object. \code{character}.}
#'     \item{lock}{Lock mechanism, if turned on the length of the list can not be modified by adding or removing elements. \code{logical}.}
#' }
#' @template FLlst-accessors
#' @template FLlst-constructors
#' @author The FLR Team
#' @seealso \link[base]{*}, \link[methods]{Arith}, \link[base]{as.data.frame},
#' \link{bubbles}, \link{catch<-}, \link{iter}, \link[stats]{model.frame},
#' \link[methods]{show}, \link[base]{summary}, \link[lattice]{xyplot},
#' \link{FLlst}, \link[base]{list}
#' @keywords classes
# class
setClass("FLQuants", contains="FLlst",
	validity=vFLQs
)

# constructor
setGeneric("FLQuants", function(object, ...){
	standardGeneric("FLQuants")
	}
)

setMethod("FLQuants", signature(object="ANY"), function(object, ...){
	lst1 <- list(...)
	nlst <- length(lst1)
	lst <- list()
	length(lst) <- nlst + 1
	lst[[1]] <- object
	lst[-1] <- lst1
	new("FLQuants", lst)
})


setMethod("FLQuants", signature(object="FLComp"),
	function(object, ...) {

		args <- list(...)

		# SPLIT into list if a character vector
		if(length(args) == 1 & length(args[[1]]) > 1)
			args <- as.list(args[[1]])

		# CHECK args are char or function
		chr <- unlist(lapply(args, function(x) is(x, 'character')))
		fun <- unlist(lapply(args, function(x) is(x, 'function')))

		if(sum(chr + fun) != length(args))
			stop("Arguments in ... must be of class 'character' or 'function'")

		# CHECK function elements have names
		if(any(names(args[fun]) == ""))
			stop("Function calls must be named, e.g. catch=catch")

		# GET names
		nms <- names(args)
		nms[chr] <- unlist(args[chr])

		# DO.CALL list elements
		res <- lapply(args, function(x) do.call(x, args=list(object)))

		# ASSIGN names
		names(res) <- nms

		return(new("FLQuants", res))
})

setMethod("FLQuants", "missing", function(...){
	if(missing(...)){
		new("FLQuants")
	} else {
		lst <- list(...)
		new("FLQuants", lst)
	}
})

setMethod("FLQuants", "list", function(object){
	new("FLQuants", object)
})

setMethod("FLQuants", "FLQuants", function(object){
	return(object)
}) # }}}

# FLCohorts {{{

# validity
vFLQs <- function(object){
	# Make sure the list contains all items of the same class
	for(i in 1:length(object)){
		if(!is(object[[i]], "FLCohort")) stop("Components must be FLCohort")
	}
	# Everything is fine
	return(TRUE)
}

# class
setClass("FLCohorts", contains="FLlst",
	validity=vFLQs
)

# constructor
setGeneric("FLCohorts", function(object, ...){
	standardGeneric("FLCohorts")
	}
)

setMethod("FLCohorts", signature(object="ANY"), function(object, ...){
	lst1 <- list(...)
	nlst <- length(lst1)
	lst <- list()
	length(lst) <- nlst + 1
	lst[[1]] <- object
	lst[-1] <- lst1
	new("FLCohorts", lst)
})

setMethod("FLCohorts", "missing", function(...){
	if(missing(...)){
		new("FLCohorts")
	} else {
		lst <- list(...)
		new("FLCohorts", lst)
	}
})

setMethod("FLCohorts", "list", function(object){
	new("FLCohorts", object)
})

setMethod("FLCohorts", "FLCohorts", function(object){
	return(object)
}) # }}}

# FLComps {{{
vFLCs <- function(object) {

  # all elements inherit from class FLComp
  if(!all(unlist(lapply(object, is, 'FLComp'))))
    return("All elements must be of a class that inherits from FLComp")

  return(TRUE)
}

setClass("FLComps", contains=c("FLlst"), validity=vFLCs)
# }}}

# FLStocks {{{
vFLSs <- function(object){

  # All items are FLStock
  if(!all(unlist(lapply(object, is, 'FLStock'))))
      return("Components must be FLStock")

	return(TRUE)
}

# class
setClass("FLStocks", contains="FLComps",
	validity=vFLSs
)

# constructor
setMethod("FLStocks", signature(object="FLStock"), function(object, ...) {
    args <- list(...)
    lst <- c(list(object), args)
    FLStocks(lst)
})

setMethod("FLStocks", signature(object="missing"),
  function(...) {
    # empty
  	if(missing(...)){
	  	new("FLStocks")
    # or not
  	} else {
      args <- list(...)
      object <- args[!names(args)%in%c('names', 'desc', 'lock')]
      args <- args[!names(args)%in%names(object)]
      do.call('FLStocks',  c(list(object=object), args))
	  }
  }
)

setMethod("FLStocks", signature(object="list"),
  function(object, ...) {

    args <- list(...)

    # names in args, ...
    if("names" %in% names(args)) {
      names <- args[['names']]
    } else {
    # ... or in object,
      if(!is.null(names(object))) {
        names <- names(object)
    # ... or in elements, ...
      } else {
        names <- unlist(lapply(object, name))
        # ... or 1:n
        idx <- names == "NA" | names == ""
        if(any(idx))
          names[idx] <- as.character(length(names))[idx]
      }
    }

    # desc & lock
    args <- c(list(Class="FLStocks", .Data=object, names=names),
      args[!names(args)%in%'names'])

    return(
      do.call('new', args)
      )

}) # }}}

# FLIndices {{{
# class
setClass("FLIndices", contains="FLComps",
	validity = function(object) {

  # All items are FLIndex
  if(!all(unlist(lapply(object, inherits, 'FLI'))))
      return("Components must be FLIndex")

	return(TRUE)
	}
)

# constructor
setMethod("FLIndices", signature(object="FLI"), function(object, ...) {
    lst <- c(object, list(...))
    FLIndices(lst)
})

setMethod("FLIndices", signature(object="missing"),
  function(...) {
    # empty
  	if(missing(...)){
	  	new("FLIndices")
    # or not
  	} else {
      args <- list(...)
      object <- args[!names(args)%in%c('names', 'desc', 'lock')]
      args <- args[!names(args)%in%names(object)]
      do.call('FLIndices',  c(list(object=object), args))
	  }
  }
)

setMethod("FLIndices", signature(object="list"),
  function(object, ...) {

    args <- list(...)

    # names in args, ...
    if("names" %in% names(args)) {
      names <- args[['names']]
    } else {
    # ... or in object,
      if(!is.null(names(object))) {
        names <- names(object)
    # ... or in elements, ...
      } else {
        names <- unlist(lapply(object, name))
        # ... or 1:n
        idx <- names == "NA" | names == ""
        if(any(idx))
          names[idx] <- as.character(seq(length(names)))[idx]
      }
    }

    # desc & lock
    args <- c(list(Class="FLIndices", .Data=object, names=names),
      args[!names(args)%in%'names'])

    return(
      do.call('new', args)
      )

}) # }}}

# FLBiols {{{
vFLSs <- function(object){

  # All items are FLBiol
  if(!all(unlist(lapply(object, is, 'FLBiol'))))
      return("Components must be FLBiol")

	return(TRUE)
}

# class
setClass("FLBiols", contains="FLComps",
	validity=vFLSs
)

# constructor
setMethod("FLBiols", signature(object="FLBiol"), function(object, ...) {
    lst <- c(object, list(...))
    FLBiols(lst)
})

setMethod("FLBiols", signature(object="missing"),
  function(...) {
    # empty
  	if(missing(...)){
	  	new("FLBiols")
    # or not
  	} else {
      args <- list(...)
      object <- args[!names(args)%in%c('names', 'desc', 'lock')]
      args <- args[!names(args)%in%names(object)]
      do.call('FLBiols',  c(list(object=object), args))
	  }
  }
)

setMethod("FLBiols", signature(object="list"),
  function(object, ...) {

    args <- list(...)

    # names in args, ...
    if("names" %in% names(args)) {
      names <- args[['names']]
    } else {
    # ... or in object,
      if(!is.null(names(object))) {
        names <- names(object)
    # ... or in elements, ...
      } else {
        names <- unlist(lapply(object, name))
        # ... or 1:n
        idx <- names == "NA" | names == ""
        if(any(idx))
          names[idx] <- as.character(length(names))[idx]
      }
    }

    # desc & lock
    args <- c(list(Class="FLBiols", .Data=object, names=names),
      args[!names(args)%in%'names'])

    return(
      do.call('new', args)
      )

}) # }}}

# FLPars {{{
# validity
vFLPs <- function(object){
	# Make sure the list contains all items of the same class
	for(i in 1:length(object)){
		if(!is(object[[i]], "FLPar")) stop("Components must be FLPar")
	}
	# Everything is fine
	return(TRUE)
}

# class
setClass("FLPars", contains="FLlst",
	validity=vFLPs
)

# constructor
setGeneric("FLPars", function(object, ...){
	standardGeneric("FLPars")
	}
)

setMethod("FLPars", signature(object="ANY"), function(object, ...){
	lst1 <- list(...)
	nlst <- length(lst1)
	lst <- list()
	length(lst) <- nlst + 1
	lst[[1]] <- object
	lst[-1] <- lst1
	new("FLPars", lst)
})

setMethod("FLPars", "missing", function(...){
	if(missing(...)){
		new("FLPars")
	} else {
		lst <- list(...)
		new("FLPars", lst)
	}
})

setMethod("FLPars", "list", function(object){
	new("FLPars", object)
})

setMethod("FLPars", "FLPars", function(object){
	return(object)
}) # }}}

# FLModelSims {{{
# validity
vFLMs <- function(object){
	# Make sure the list contains all items of the same class
	for(i in 1:length(object)){
		if(!is(object[[i]], "FLModelSim")) stop("Components must be FLModelSim")
	}
	# Everything is fine
	return(TRUE)
}

# class
setClass("FLModelSims", contains="FLlst",
	validity=vFLMs
)

# constructor
setGeneric("FLModelSims", function(object, ...){
	standardGeneric("FLModelSims")
	}
)

setMethod("FLModelSims", signature(object="ANY"), function(object, ...){
	lst1 <- list(...)
	nlst <- length(lst1)
	lst <- list()
	length(lst) <- nlst + 1
	lst[[1]] <- object
	lst[-1] <- lst1
	new("FLModelSims", lst)
})

setMethod("FLModelSims", "missing", function(...){
	if(missing(...)){
		new("FLModelSims")
	} else {
		lst <- list(...)
		new("FLModelSims", lst)
	}
})

setMethod("FLModelSims", "list", function(object){
	new("FLModelSims", object)
})

setMethod("FLModelSims", "FLModelSims", function(object){
	return(object)
}) # }}}
