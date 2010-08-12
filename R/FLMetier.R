# FLMetier - «Short one line description»
# FLCore/R/FLMetier.R

# Copyright 2003-2007 FLR Team. Distributed under the GPL 2 or later
# Maintainer:
# $Id$


## FLMetier		{{{
validFLMetier <- function(object) {
	# FLQuant slots share dims 1:5 ...
  dnames <- qapply(object, function(x) dimnames(x)[3:5])
	for(i in names(dnames))
		if(!identical(dnames[[i]], dnames[[1]]))
			return('All FLQuant slots must have the same dimensions')

  # ... and are consistent with catches
  catdnames <- lapply(object@catches, function(x)
    qapply(object, function(x) dimnames(x)[3:5]))
  for(i in seq(length=length(catdnames)))
    for(j in names(catdnames[[1]]))
	    if(!identical(catdnames[[i]][[j]], dnames[[1]]))
			  return('All FLQuant slots must have the same dimensions')
  
  # Year range of FLMetier covers all catches
  catyears <- matrix(unlist(lapply(object@catches, function(x) 
    unlist(dims(x)[c('minyear', 'maxyear')]))), byrow=TRUE, ncol=2)
  if(any(dims(object)$minyear < catyears [,1]) |
    any(dims(object)$maxyear > catyears [,2]))
    return('Year range of metier should encompass those of catch(es)')

  # iter is consistent between metier and catches
  if(any(dims(object)$iter != unlist(lapply(object@catches, function(x) dims(x)$iter))))
    return('iter must be 1 or N across all slots and levels')

	return(TRUE)
}

setClass('FLMetier',
	representation('FLComp',
		gear='character',
		effshare='FLQuant',
		vcost='FLQuant',
		catches='FLCatches'),
	prototype(name=character(0), desc=character(0),
		range= unlist(list(min=0, max=0, plusgroup=NA, minyear=1, maxyear=1)),
		gear=character(0), catches=new('FLCatches'), effshare=FLQuant(1), vcost=FLQuant(NA)),
	validity=validFLMetier)

remove(validFLMetier)
# Accesors
createFLAccesors('FLMetier', exclude=c('range', 'catches', 'name', 'desc'))
# }}}

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
setMethod("iter", signature(object="FLMetier"),
	  function(object, iter)
	  {
		# FLQuant slots
		names <- names(getSlots(class(object))[getSlots(class(object))=="FLQuant"])
		for(s in names) 
		{
			if(dims(slot(object, s))$iter == 1)
				slot(object, s) <- iter(slot(object, s), 1)
			else
				slot(object, s) <- iter(slot(object, s), iter)
		}
		# FLCatches
		names <- names(object@catches)
		for (s in names)
			catches(object, s) <- iter(catches(object, s), iter)
		
		return(object)
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
