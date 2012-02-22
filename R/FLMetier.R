# FLMetier - «Short one line description»
# FLCore/R/FLMetier.R

# Copyright 2003-2007 FLR Team. Distributed under the GPL 2 or later
# Maintainer:
# $Id$


## FLMetier()	{{{
# FLMetier(FLCatch)
setMethod('FLMetier', signature(catches='FLCatch'),
	function(catches, gear='NA', ...)
    FLMetier(catches=FLCatches(catches), gear=gear, ...)
)
# FLMetier(FLCatches)
setMethod('FLMetier', signature(catches='FLCatches'),
	function(catches, gear='NA', ...)
    {
		args <- list(...)
    if(length(args) > 0)
    {
      classes <- lapply(args, class)
      # if any in ... is FLQuant
      if(any('FLQuant' %in% classes))
        # take dimnames of first one
        ## BUG FIX: dimn <- dimnames(args[[names(classes['FLQuant' %in% classes])[1]]])
        dimn <- dimnames(args[[names(classes[classes %in% 'FLQuant'])[1]]])
    }
    if(!exists('dimn'))
    {
      # generate from FLCatch
      dimn <- dimnames(landings.n(catches[[1]]))
      years <- apply(as.data.frame(lapply(catches, function(x) unlist(dims(x)[c(
        'minyear','maxyear')]))), 1, max)
      dimn$year <- as.character(seq(years[1], years[2]))
      dimn[[1]] <- 'all'
    }
    
    # new object
		res <- new('FLMetier', catches=catches, gear=gear, effshare=FLQuant(1, dimnames=dimn),
      vcost=FLQuant(NA, dimnames=dimn), range=range(catches))
    # load extra arguments
		if(length(args) > 0)
			for (i in seq(length(args)))
				slot(res, names(args[i])) <- args[[i]]
		return(res)
    }
)
# FLMetier(FLQuant)
setMethod('FLMetier', signature(catches='FLQuant'),
	function(catches, gear='NA', ...)
      return(FLMetier(FLCatch(catches), gear=gear, ...))
)
# FLMetier(missing)
setMethod('FLMetier', signature(catches='missing'),
	function(catches, gear='NA', ...)
    FLMetier(FLCatches(FLCatch(name='NA')), ...)
)	# }}}

# summary	{{{
setMethod('summary', signature(object='FLMetier'),
	function(object, ...)
	{
		callNextMethod(object)
		cat("\n")
		cat("Catches: ", "\n")
		for (j in names(object@catches))
			cat("\t", j, ": [", dim(object@catches[[j]]@landings.n),"]\n")
	}
)
# }}}

# trim {{{
setMethod('trim', signature(x='FLMetier'),
  function(x, ...)
  {
    x <- callNextMethod()
    x@catches <- lapply(x@catches, trim, ...)
    return(x)
  }
) # }}}

# propagate {{{
setMethod('propagate', signature(object='FLMetier'),
  function(object, ...)
  {
    object <- qapply(object, propagate, ...)
    object@catches <- lapply(object@catches, propagate, ...)
    return(object)
  }
) # }}}

## iter {{{
setMethod("iter", signature(obj="FLMetier"),
	  function(obj, iter)
	  {
		# FLQuant slots
		names <- names(getSlots(class(obj))[getSlots(class(obj))=="FLQuant"])
		for(s in names) 
		{
			if(dims(slot(obj, s))$iter == 1)
				slot(obj, s) <- iter(slot(obj, s), 1)
			else
				slot(obj, s) <- iter(slot(obj, s), iter)
		}
		# FLCatches
		names <- names(obj@catches)
		for (s in names)
			catches(obj, s) <- iter(catches(obj, s), iter)
		
		return(obj)
	  }
) # }}}

# "[" and "[["             {{{
setMethod("[", signature(x="FLMetier", i="ANY", j="missing"),
  function(x, i, drop=FALSE)
  {
	  if (missing(i))
      return(x)
    x@catches <- x@catches[i]
    return(x)
	}
)

setMethod("[[", signature(x="FLMetier", i="ANY", j="missing"),
  function(x, i, drop=FALSE)
  {
	  if (missing(i))
      stop("invalid subscript type")
    return(x@catches[[i]])
	}
)  # }}}
