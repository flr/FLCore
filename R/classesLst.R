# classesLst.R - DESC
# classesLst.R

# Copyright 2003-2016 FLR Team. Distributed under the GPL 2 or later
# Maintainer: Iago Mosqueira, EC JRC

# FLlst {{{

#' Class FLlst
#' 
#' \code{FLlst} is a class that extends \code{list} but implements a set of
#' features that give a little more structure to list objects. First the
#' elements of \code{FLlst} must all be of the same class. Second it implements
#' a lock mechanism that, when turned on, does not allow the user to increase
#' or decrease the object length.
#' 
#' @name FLlst
#' @aliases FLlst-class FLlst FLlst-methods FLlst,ANY-method
#' FLlst,missing-method FLlst,list-method
#' @docType class
#' @section Slots: \describe{
#'   \item{.Data}{The data. \code{list}.}
#'   \item{names}{Names of the list elements. \code{character}.}
#'   \item{desc}{Description of the object. \code{character}.}
#'   \item{lock}{Lock mechanism, if turned on the length of the list can not be
#'     modified by adding or removing elements. \code{logical}.} }
#' @template FLlst-constructors
#' @author The FLR Team
#' @seealso \link[base]{[}, \link[base]{[<-}, \link[base]{[[<-},
#' \link[base]{$<-}, \link[methods]{coerce}, \link[base]{lapply},
#' \link[stats]{window}, \link[base]{list}
#' @keywords classes
#' @examples
#' 
#' fll01 <- new("FLlst", list(a=1:10, b=10:20))
#' fll02 <- new("FLlst", list(1:10, 10:20), names=c("a","b"))
#' fll03 <- FLlst(a=1:10, b=10:20)
#' fll04 <- FLlst(list(a=1:10, b=10:20))
#' fll05 <- FLlst(c(1:10), c(10:20))
#' names(fll05) <- names(fll01)
#' names(fll01)
#' 
setClass("FLlst", contains="list",
  representation(names="character", desc="character", lock="logical"),
	prototype(lock=FALSE), validity=function(object){

    # ALL elements are of the same class
    if(length(unique(lapply(object, is))) > 1)
      return("Elements must be of the same class")

    # ALL elements in the list are validObjects themselves
    if(!all(unlist(lapply(object, validObject))))
	    return("Components must be valid objects themselves (validObject == TRUE)")

	  # Everything is fine
	  return(TRUE)
  }
) # }}}

# FLQuants {{{

#' Class FLQuants
#' 
#' \code{FLQuants} is a \code{list} of \code{FLQuant} objects. It is very
#' similar to the standard \code{list} class. It implements a lock mechanism
#' that, when turned on, does not allow the user to increase or decrease the
#' object length. The elements of \code{FLQuants} must all be of class
#' \code{FLQuant}.
#'
#' @name FLQuants
#' @docType class
#' @aliases FLQuants-class
#' @section Slots: \describe{ \item{.Data}{The data. \code{list}.}
#' \item{names}{Names of the list elements. \code{character}.}
#' \item{desc}{Description of the object. \code{character}.} \item{lock}{Lock
#' mechanism, if turned on the length of the list can not be modified by adding
#' or removing elements. \code{logical}.} }
#' @template FLlst-constructors
#' @author The FLR Team
#' @seealso \link[base]{*}, \link[methods]{Arith}, \link[base]{as.data.frame},
#' \link{bubbles}, \link{catch<-}, \link{iter}, \link[stats]{model.frame},
#' \link[methods]{show}, \link[base]{summary}, \link[lattice]{xyplot},
#' \link{FLlst}, \link[base]{list}
#' @keywords classes
#' @examples
#'
#' # Compute various FLStock indicators
#'   data(ple4)
#'   fqs <- FLQuants(ssb=ssb(ple4), catch=catch(ple4), rec=rec(ple4),
#'     f=fbar(ple4))
#'   summary(fqs)
#'   xyplot(data~year|qname, fqs, type='b', scales=list(relation='free'))
#'
setClass("FLQuants", contains="FLlst",
	validity = function(object){
	# Make sure the list contains all items of the same class
    if(!all(unlist(lapply(object, is, 'FLQuant'))))
	  	return("Components must be FLQuant")
	# Everything is fine
	return(TRUE)
  }
)

#' @rdname FLQuants
#' @aliases FLQuants,ANY-method
setMethod("FLQuants", signature(object="ANY"), function(object, ...){
	lst1 <- list(...)
	nlst <- length(lst1)
	lst <- list()
	length(lst) <- nlst + 1
	lst[[1]] <- object
	lst[-1] <- lst1
	new("FLQuants", lst)
})

#' @rdname FLQuants
#' @aliases FLQuants,FLComp-method
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

#' @rdname FLQuants
#' @aliases FLQuants,missing-method
setMethod("FLQuants", "missing", function(...){
	if(missing(...)){
		new("FLQuants")
	} else {
		lst <- list(...)
		new("FLQuants", lst)
	}
})

#' @rdname FLQuants
#' @aliases FLQuants,list-method
setMethod("FLQuants", "list", function(object){
	new("FLQuants", object)
})

#' @rdname FLQuants
#' @aliases FLQuants,FLQuants-method
setMethod("FLQuants", "FLQuants", function(object){
	return(object)
}) # }}}

# FLCohorts {{{

#' Class FLCohorts
#' 
#' \code{FLCohorts} is a class that extends \code{list} through \code{FLlst}
#' but implements a set of features that give a little more structure to
#' list objects. The elements of \code{FLCohorts} must all be of class
#' \code{FLCohort}. It implements a lock mechanism that, when turned on, does
#' not allow the user to increase or decrease the object length.
#'
#' @name FLCohorts
#' @aliases FLCohorts-class FLCohorts FLCohorts-methods FLCohorts,ANY-method
#' FLCohorts,missing-method FLCohorts,list-method FLCohorts,FLCohorts-method
#' @docType class
#' @section Slots: \describe{ \item{.Data}{The data. \code{list}}
#' \item{names}{Names of the list elements. \code{character}}
#' \item{desc}{Description of the object. \code{character}} \item{lock}{Lock
#' mechanism, if turned on the length of the list can not be modified by adding
#' or removing elements. \code{logical}} }
#' @template FLlst-constructors
#' @author The FLR Team
#' @seealso \link[base]{*}, \link[methods]{Arith}, \link[base]{as.data.frame},
#' \link{bubbles}, \link{catch<-}, \link{iter}, \link[stats]{model.frame},
#' \link[methods]{show}, \link[base]{summary}, \link[lattice]{xyplot},
#' \link{FLlst}, \link[base]{list}
#' @keywords classes
#'

setClass("FLCohorts", contains="FLlst",
	validity=function(object){
	# Make sure the list contains all items of the same class
	for(i in 1:length(object)){
		if(!is(object[[i]], "FLCohort")) stop("Components must be FLCohort")
	}
	# Everything is fine
	return(TRUE)
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

#' Class FLComps
#'
#' A virtual class that forms the basis for many FLR list classes. No objects
#' of this class can be constructed.
#'
#' @name FLComps
#' @aliases FLComps FLComps-class
#' @docType class
#' @section Validity: \describe{
#'     \item{Elements}{All elements must be of a class that inherits from FLComp}
#' }
#' @section Slots: \describe{
#'   \item{.Data}{The data. \code{list}.}
#'   \item{names}{Names of the list elements. \code{character}.}
#'   \item{desc}{Description of the object. \code{character}.}
#'   \item{lock}{Lock mechanism, if turned on the length of the list can not be
#'     modified by adding or removing elements. \code{logical}.} }
#' @template FLlst-constructors
#' @author The FLR Team
#' @seealso \link{FLlst}, \link{FLComp}
#' @keywords classes

setClass("FLComps", contains=c("FLlst"),
  validity=function(object) {

  # all elements inherit from class FLComp
  if(!all(unlist(lapply(object, is, 'FLComp'))))
    return("All elements must be of a class that inherits from FLComp")

  return(TRUE)
  }
) # }}}

# FLStocks {{{

#' Class FLStocks
#' 
#' \code{FLStocks} is a class that extends \code{list} through \code{FLlst} but
#' implements a set of features that give a little bit more structure to list
#' objects. The elements of \code{FLStocks} must all be of class
#' \code{FLStock}. It implements a lock mechanism that, when turned on, does
#' not allow the user to increase or decrease the object length.
#'
#' @name FLStocks
#' @aliases FLStocks-class FLStocks FLStocks-methods FLStocks,ANY-method
#' FLStocks,missing-method FLStocks,list-method
#' @docType class
#' @section Slots: \describe{ \item{.Data}{The data. \code{list}.}
#' \item{names}{Names of the list elements. \code{character}.}
#' \item{desc}{Description of the object. \code{character}.} \item{lock}{Lock
#' mechanism, if turned on the length of the list can not be modified by adding
#' or removing elements. \code{logical}.} }
#' @template FLlst-constructors
#' @author The FLR Team
#' @seealso \link[graphics]{plot}, \link{FLlst}, \link[base]{list}
#' @keywords classes
#' @examples
#' 
#' data(ple4)
#' fls <- FLStocks(sa=ple4, sb=window(ple4, end=1980))
#' summary(fls)
#' 

setClass("FLStocks", contains="FLComps",
	validity=function(object){

  # All items are FLStock
  if(!all(unlist(lapply(object, is, 'FLStock'))))
      return("Components must be FLStock")

	return(TRUE)
  }
)

#' @rdname FLStocks
#' @aliases FLStocks,FLStock-method
setMethod("FLStocks", signature(object="FLStock"), function(object, ...) {
    args <- list(...)
    lst <- c(list(object), args)
    FLStocks(lst)
})

#' @rdname FLStocks
#' @aliases FLStocks,missing-method
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

#' @rdname FLStocks
#' @aliases FLStocks,list-method
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

#' Class FLIndices
#' 
#' \code{FLIndices} is a class that extends \code{list} through \code{FLlst}
#' but implements a set of features that give a little more structure to
#' list objects. The elements of \code{FLIndices} must all be of class
#' \code{FLIndex}. It implements a lock mechanism that, when turned on, does
#' not allow the user to increase or decrease the object length.
#'
#' @name FLIndices
#' @aliases FLIndices-class
#' @docType class
#' @section Slots: \describe{ \item{.Data}{The data. \code{list}.}
#' \item{names}{Names of the list elements. \code{character}.}
#' \item{desc}{Description of the object. \code{character}.} \item{lock}{Lock
#' mechanism, if turned on the length of the list can not be modified by adding
#' or removing elements. \code{logical}.} }
#' @template FLlst-constructors
#' @author The FLR Team
#' @seealso \linkS4class{FLlst}, \link[base]{list}
#' @keywords classes
#' @examples
#' 
#' data(ple4.index)
#' flis <- FLIndices(INDa=ple4.index, INDb=window(ple4.index, end=2000))
#'
setClass("FLIndices", contains="FLComps",
	validity = function(object) {

  # All items are FLIndex
  if(!all(unlist(lapply(object, inherits, 'FLI'))))
      return("Components must be FLIndex")

	return(TRUE)
	}
)

#' @rdname FLIndices
#' @aliases FLIndices,FLI-method
setMethod("FLIndices", signature(object="FLI"), function(object, ...) {
    lst <- c(object, list(...))
    FLIndices(lst)
})

#' @rdname FLIndices
#' @aliases FLIndices,missing-method
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

#' @rdname FLIndices
#' @aliases FLIndices,list-method
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

# FLPars {{{

#' Class FLPars
#' 
#' A list of \code{FLPar} objects.
#'
#' @name FLPars
#' @aliases FLPars-class FLPars FLPars-methods
#' @docType class
#' @section Slots: \describe{
#'   \item{.Data}{Internal S4 data representation, of class \code{list}.}
#'   \item{desc}{As textual description of the object contents}
#'   \item{lock}{Can the object be extended/trimmed? \code{TRUE} or \code{FALSE}.}
#'   \item{names}{A character vector for the element names} }
#' @template FLlst-constructors
#' @author The FLR Team
#' @seealso \link{FLlst}, \link[base]{list}, \link[base]{vector}
#' @keywords classes
#'
setClass("FLPars", contains="FLlst",
	validity=function(object){
  	# Make sure the list contains all items of the same class
	  for(i in 1:length(object)){
		  if(!is(object[[i]], "FLPar")) stop("Components must be FLPar")
  	}
	  # Everything is fine
  	return(TRUE)
  }
)

#' @rdname FLPars
#' @aliases FLPars,ANY-method
setMethod("FLPars", signature(object="ANY"), function(object, ...){
	lst1 <- list(...)
	nlst <- length(lst1)
	lst <- list()
	length(lst) <- nlst + 1
	lst[[1]] <- object
	lst[-1] <- lst1
	new("FLPars", lst)
})

#' @rdname FLPars
#' @aliases FLPars,missing-method
setMethod("FLPars", "missing", function(...){
	if(missing(...)){
		new("FLPars")
	} else {
		lst <- list(...)
		new("FLPars", lst)
	}
})

#' @rdname FLPars
#' @aliases FLPars,list-method
setMethod("FLPars", "list", function(object){
	new("FLPars", object)
})

#' @rdname FLPars
#' @aliases FLPars,FLPars-method
setMethod("FLPars", "FLPars", function(object){
	return(object)
}) # }}}

# FLModelSims {{{

#' Class FLModelSims
#' 
#' A list of \code{\linkS4class{FLModelSim}} objects.
#'
#' @name FLModelSims
#' @aliases FLModelSims-class FLModelSims FLModelSims-methods
#' @docType class
#' @section Slots: \describe{
#'   \item{.Data}{The data. \code{list}.}
#'   \item{names}{Names of the list elements. \code{character}.}
#'   \item{desc}{Description of the object. \code{character}.}
#'   \item{lock}{Lock mechanism, if turned on the length of the list can not be
#'     modified by adding or removing elements. \code{logical}.} }
#' @template FLlst-constructors
#' @author The FLR Team
#' @seealso \link{FLlst}, \link[base]{list}, \link[base]{vector}
#' @keywords classes
#'

setClass("FLModelSims", contains="FLlst",
	validity=function(object){
  	# Make sure the list contains all items of the same class
	  for(i in 1:length(object)){
		  if(!is(object[[i]], "FLModelSim")) stop("Components must be FLModelSim")
  	}
	  # Everything is fine
	  return(TRUE)
  }
)

#' @rdname FLModelSims
#' @aliases FLModelSims
setGeneric("FLModelSims", function(object, ...){
	standardGeneric("FLModelSims")
	}
)

#' @rdname FLModelSims
#' @aliases FLModelSims,ANY-method
setMethod("FLModelSims", signature(object="ANY"), function(object, ...){
	lst1 <- list(...)
	nlst <- length(lst1)
	lst <- list()
	length(lst) <- nlst + 1
	lst[[1]] <- object
	lst[-1] <- lst1
	new("FLModelSims", lst)
})

#' @rdname FLModelSims
#' @aliases FLModelSims,missing-method
setMethod("FLModelSims", "missing", function(...){
	if(missing(...)){
		new("FLModelSims")
	} else {
		lst <- list(...)
		new("FLModelSims", lst)
	}
})

#' @rdname FLModelSims
#' @aliases FLModelSims,list-method
setMethod("FLModelSims", "list", function(object){
	new("FLModelSims", object)
})

#' @rdname FLModelSims
#' @aliases FLModelSims,FLModelSims-method
setMethod("FLModelSims", "FLModelSims", function(object){
	return(object)
}) # }}}
