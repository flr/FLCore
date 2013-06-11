# classesComp.R - 
# FLCore/R/classesComp.R

# Copyright 2003-2012 FLR Team. Distributed under the GPL 2 or later
# Maintainer: Iago Mosqueira, JRC
# $Id:  $


# FLComp   {{{
validFLComp <- function(object){

	# range must be named ...
	nms <- names(range(object))
	# with non/empty strings
	if(any(nchar(nms) == 0))
		return("names in range cannot be empty")

	# Any FLArray?
	slots <- getSlots(class(object))
	
	if(any("FLArray" %in% slots) | any("FLQuant" %in% slots)) {

		# FLQuant slots must have either 1 or n iter
  	dims <- unlist(qapply(object, function(x) dims(x)$iter))
	  test <- dims != max(dims) & dims != 1
		if (any(test))
			stop(paste("All slots must have iters equal to 1 or 'n': error in",
				paste(names(test[!test]), collapse=', ')))
	
	  # and dimname for iter[1] should be '1'
  	dimnms <- qapply(object, function(x) dimnames(x)$iter)
		test <- unlist(dimnms[dims == 1])
		if(!all(test==test))
			stop(paste("Incorrect names on the iter dimension in ",
				paste(names(test[!test]), collapse=', ')))

	  # all 'quant' should be equal
  	quants <- unlist(qapply(object, quant))
	  if(any(quants != quants[1]))
  	  stop("Not all 'quant' names are the same. Check using qapply(x, quant)")

	}
	
	return(TRUE)
}

setClass("FLComp", representation(name="character", desc="character",
	range="numeric", "VIRTUAL"), prototype(name=character(0), desc=character(0),
  range	= unlist(list(min=0, max=0, plusgroup=NA, minyear=1, maxyear=1))), 
  validity=validFLComp)

#invisible(createFLAccesors('FLComp', include=c('name', 'desc')))
#  }}}

# FLS			{{{
validFLS <- function(object) {

	# TODO
	return(TRUE)

	names <- names(getSlots('FLS')[getSlots('FLS')=="FLQuant"])
	for(i in names){
		# all dimnames 2:5 are the same
		if(!identical(unlist(dimnames(object@catch.n)[2:5]),
			unlist(dimnames(slot(object, i))[2:5])))
			return(paste('All elements must share dimensions 2 to 5: Error in object@', i))
		# no. iter are equal or one
	}
	for (i in names[!names%in%c('catch', 'landings', 'discards', 'stock')])
	{
		# quant is n
		if(!identical(unlist(dimnames(object@catch.n)[1]),
			unlist(dimnames(slot(object, i))[1])))
			return(paste('All elements must share quant names: Error in object', i))
	}
	for (i in c('catch', 'landings', 'discards'))
	{
		# quant is 1
		if(dim(slot(object, i))[1] != 1)
			return(paste('Wrong dimensions for slot ', i, 'in object'))
	}
	# check range
	dim <- dim(object@catch.n)
	dimnm <- dimnames(object@catch.n)
	if(all(as.numeric(object@range[4:5]) != c(as.numeric(dimnm$year[1]),
		as.numeric(dimnm$year[dim[2]]))))
		return('Range does not match object dimensions')
	
	return(TRUE)}

setClass("FLS",
	representation(
	"FLComp",
	catch	    	="FLQuant",
	catch.n	    ="FLQuant",
	catch.wt		="FLQuant",
	discards		="FLQuant",
	discards.n  ="FLQuant",
	discards.wt ="FLQuant",
	landings		="FLQuant",
	landings.n  ="FLQuant",
	landings.wt ="FLQuant",
	stock	    	="FLQuant",
	stock.n	    ="FLQuant",
	stock.wt		="FLQuant",
	m						="FLQuant",
	mat		    	="FLQuant",
	harvest	    ="FLQuant",
	harvest.spwn="FLQuant",
	m.spwn	    ="FLQuant",
	"VIRTUAL"
	),
	prototype=prototype(
		name	= character(0),
		desc	= character(0),
		range	= unlist(list(min=0, max=0, plusgroup=NA, minyear=1, maxyear=1, minfbar=0, maxfbar=0)),
		catch	= FLQuant(),
		catch.n	= FLQuant(),
		catch.wt= FLQuant(),
		discards= FLQuant(),
		discards.n = FLQuant(),
		discards.wt= FLQuant(),
		landings   = FLQuant(),
		landings.n = FLQuant(),
		landings.wt= FLQuant(),
		stock	   = FLQuant(),
		stock.n	 = FLQuant(),
		stock.wt = FLQuant(),
		m		 = FLQuant(),
		mat		 = FLQuant(),
		harvest	 = FLQuant(units="f"),
		harvest.spwn = FLQuant(),
		m.spwn	 = FLQuant()
	),
  validity=validFLS
)
remove(validFLS)

invisible(createFLAccesors("FLS", exclude=c('name', 'desc', 'range', 'harvest')))	# }}}

# FLStock			{{{
validFLStock <- function(object) {
	
	names <- names(getSlots('FLStock')[getSlots('FLStock')=="FLQuant"])
	for(i in names){
		# all dimnames but iter are the same
		if(!identical(unlist(dimnames(object@catch.n)[2:5]),
			unlist(dimnames(slot(object, i))[2:5])))
			return(paste('All elements must share dimensions 2 to 5: Error in FLStock@', i))
		# no. iter are equal or one
	}
	for (i in names[!names%in%c('catch', 'landings', 'discards', 'stock')])
	{
		# quant is n
		if(!identical(unlist(dimnames(object@catch.n)[1]),
			unlist(dimnames(slot(object, i))[1])))
			return(paste('All elements must share quant names: Error in FLStock', i))
	}
	for (i in c('catch', 'landings', 'discards'))
	{
		# quant is 1
		if(dim(slot(object, i))[1] != 1)
			return(paste('Wrong dimensions for slot ', i, 'in FLStock'))
	}
	# check range
	dim <- dim(object@catch.n)
	dimnm <- dimnames(object@catch.n)
	if(all(as.numeric(object@range[4:5]) != c(as.numeric(dimnm$year[1]),
		as.numeric(dimnm$year[dim[2]]))))
		return('Range does not match object dimensions')
	
	return(TRUE)}

setClass("FLStock",
	representation(
	"FLS"
	),
	prototype=prototype(
		name	= character(0),
		desc	= character(0),
		range	= unlist(list(min=0, max=0, plusgroup=NA, minyear=1, maxyear=1, minfbar=0, maxfbar=0)),
		catch	= FLQuant(),
		catch.n	= FLQuant(),
		catch.wt= FLQuant(),
		discards= FLQuant(),
		discards.n = FLQuant(),
		discards.wt= FLQuant(),
		landings   = FLQuant(),
		landings.n = FLQuant(),
		landings.wt= FLQuant(),
		stock	   = FLQuant(),
		stock.n	 = FLQuant(),
		stock.wt = FLQuant(),
		m		 = FLQuant(),
		mat		 = FLQuant(),
		harvest	 = FLQuant(units="f"),
		harvest.spwn = FLQuant(),
		m.spwn	 = FLQuant()
	),
  validity=validFLStock
)
remove(validFLStock)

#invisible(createFLAccesors("FLStock", exclude=c('name', 'desc', 'range', 'harvest')))	# }}}

# FLStockLen			{{{
validFLStockLen <- function(object) {

	# TODO
	return(TRUE)
	
	names <- names(getSlots('FLStock')[getSlots('FLStock')=="FLQuant"])
	for(i in names){
		# all dimnames but iter are the same
		if(!identical(unlist(dimnames(object@catch.n)[2:5]),
			unlist(dimnames(slot(object, i))[2:5])))
			return(paste('All elements must share dimensions 2 to 5: Error in FLStock@', i))
		# no. iter are equal or one
	}
	for (i in names[!names%in%c('catch', 'landings', 'discards', 'stock')])
	{
		# quant is n
		if(!identical(unlist(dimnames(object@catch.n)[1]),
			unlist(dimnames(slot(object, i))[1])))
			return(paste('All elements must share quant names: Error in FLStock', i))
	}
	for (i in c('catch', 'landings', 'discards'))
	{
		# quant is 1
		if(dim(slot(object, i))[1] != 1)
			return(paste('Wrong dimensions for slot ', i, 'in FLStock'))
	}
	# check range
	dim <- dim(object@catch.n)
	dimnm <- dimnames(object@catch.n)
	if(all(as.numeric(object@range[4:5]) != c(as.numeric(dimnm$year[1]),
		as.numeric(dimnm$year[dim[2]]))))
		return('Range does not match object dimensions')
	
	return(TRUE)}

setClass("FLStockLen",
	representation(
	"FLS",
	midpoint = "numeric"
	),
	prototype=prototype(
		name	= character(0),
		desc	= character(0),
		range	= unlist(list(min=0, max=0, plusgroup=NA, minyear=1, maxyear=1, minfbar=0, maxfbar=0)),
		midpoint = as.numeric(NA),
		catch	= FLQuant(),
		catch.n	= FLQuant(),
		catch.wt= FLQuant(),
		discards= FLQuant(),
		discards.n = FLQuant(),
		discards.wt= FLQuant(),
		landings   = FLQuant(),
		landings.n = FLQuant(),
		landings.wt= FLQuant(),
		stock	   = FLQuant(),
		stock.n	 = FLQuant(),
		stock.wt = FLQuant(),
		m		 = FLQuant(),
		mat		 = FLQuant(),
		harvest	 = FLQuant(units="f"),
		harvest.spwn = FLQuant(),
		m.spwn	 = FLQuant()
	),
  validity=validFLStockLen
)
remove(validFLStockLen)

invisible(createFLAccesors("FLStockLen", include=c('midpoint')))	# }}}

# FLBiol {{{
validFLBiol <- function(object){

   ## All FLQuant objects must have same dimensions
   Dim <- dim(object@n)[-6]

   s.  <-list("n","wt","fec","spwn","m")

   for (i. in s.)	{
	   t. <- slot(object,i.)
	   if (is.FLQuant(t.) & !all(dim(t.)[-6] == Dim))
	      return(paste("FLQuant dimensions wrong for ", i.))
	   }

   # Verify that bounds are correct and correspond to first slot
	.t  <-getSlots(class(object))
	.t  <-.t[.t=="FLQuant"]

   if (length(.t)> 0) {

      Par <- dims(.s<-slot(object,names(.t[1])))

	   min <- object@range["min"]
      if (!is.na(min) && (min < Par$min || min > Par$max))
   		return("min quant is outside range in FLQuant slots")
	   max <- object@range["max"]
	   if (!is.na(max) && (max < Par$min || max > Par$max))
	   	return("max quant is outside range in FLQuant slots")
	   if (!is.na(min) && !is.na(max) && max < min)
		   return("max quant is lower than min")
	   plusgroup <- object@range["plusgroup"]
	   if (!is.na(plusgroup) && (plusgroup < Par$min || plusgroup > Par$max))
		   return("plusgroup is outside [min, max] quant range in FLQuant slots")
	   minyear <- object@range["minyear"]
	   if (!is.na(minyear) && (minyear < Par$minyear || minyear > Par$maxyear))
		   return("minyear is outside years range in FLQuant slots")
	   maxyear <- object@range["maxyear"]
	   if (!is.na(maxyear) && (maxyear < Par$minyear || maxyear > Par$maxyear))
		   return("maxyear is outside years range in FLQuant slots")
	   if (!is.na(minyear) && !is.na(maxyear) && maxyear < minyear)
		   return("maxyear is lower than minyear")
	   }

   # Everything is fine
   return(TRUE)
   }

setClass("FLBiol",
	representation(
		"FLComp",
        n        ="FLQuant",
		m        ="FLQuant",
		wt       ="FLQuant",
		fec      ="FLQuant",
		spwn     ="FLQuant"
      ),
	prototype=prototype(
		name     =character(0),
		desc     =character(0),
		range    =unlist(list(min=NA, max=NA, plusgroup=NA, minyear=1, maxyear=1)),
    n        = FLQuant(),
		m        = FLQuant(),
		wt       = FLQuant(),
		fec      = FLQuant(),
		spwn     = FLQuant()),
	validity=validFLBiol
)

setValidity("FLBiol", validFLBiol)
remove(validFLBiol)	# We do not need this function any more
invisible(createFLAccesors("FLBiol", exclude=c('name', 'desc', 'range'))) # }}}

# FLIndex    {{{
validFLIndex <- function(object) {

  dimnms <- qapply(object, function(x) dimnames(x))

  # iters are 1 or N
  if (length(unique(unlist(qapply(object,function(x) dims(x)$iter))))>2)
     stop("Iters in FLIndex can only be of length 1 or n")

  # quant is 1 or N
  if (length(unique(unlist(qapply(object,function(x) dims(x)$max))))>2)
     stop("quant dimension in FLIndex can only be 'all' or n")

  # iter is 1 or N
  if (length(unique(unlist(qapply(object,function(x) dims(x)$iter))))>2)
     stop("iter dimension in FLIndex can only be '1' or n")

  # dims[2:5] match
  for(i in names(dimnms)[-1])
    if(!all.equal(dimnms[[i]][c(-1,-6)], dimnms[[1]][c(-1,-6)]))
      stop(cat("Mismatch in dims for", i))
      
  # first dim equal for all index.* slots
  #for(i in grep('index', names(dimnms), value=TRUE))
  #  if(!all.equal(dimnms[[i]][1], dimnms[[1]][1]))
  #    stop(cat("Mismatch in dims for", i))

  # effort should have quant='all'
  if (!(dims(slot(object,"effort"))[1] == 1))
     stop("Effort can only have quant = 'all'")

  # min / max
  dims <- dims(object@catch.n)
  min <- object@range["min"]

  if (!is.na(min) && (min < dims(object@catch.n)$min || min > dims(object@catch.n)$max))
     stop(paste("min is outside quant range in FLQuant slot", i))

  max <- object@range["max"]
  if(!is.na(max) && (max < dims(object@catch.n)$min || max > dims(object@catch.n)$max))
    stop(paste("max is outside quant range in FLQuant slot", i))

  if (!is.na(min) && !is.na(max) && max < min)
    stop(paste("max quant is lower than min quant in FLQuant slot", i))

  # plusgroup
  plusgroup <- object@range["plusgroup"]
  if (!is.na(plusgroup) && (plusgroup < dims$min || plusgroup > dims$max))
     stop("plusgroup is outside [min, max] range in FLQuant slots")

  # minyear / maxyear
  dims <- dims(object@index)
  minyear <- object@range["minyear"]
  if (!is.na(minyear) && (minyear < dims$minyear || minyear > dims$maxyear))
     stop(paste("minyear is outside years range in FLQuant slot", i))
  maxyear <- object@range["maxyear"]
  if (!is.na(maxyear) && (maxyear < dims$minyear || maxyear > dims$maxyear))
     stop(paste("maxyear is outside years range in FLQuant slot", i))
  if (!is.na(minyear) && !is.na(maxyear) && maxyear < minyear)
     stop(paste("maxyear is lower than minyear in FLQuant slot", i))

  # Everything is fine
  return(TRUE)
  }

setClass("FLIndex",
    representation(
		"FLComp",
        type         = "character",
        distribution = "character",
        index        = "FLQuant",
        index.var    = "FLQuant",
        catch.n      = "FLQuant",
		catch.wt     = "FLQuant",
		effort       = "FLQuant",
		sel.pattern  = "FLQuant",
		index.q      = "FLQuant"),
    prototype=prototype(
        name         = character(0),
        desc         = character(0),
        type         = character(0),
        range        = unlist(list(min=0, max=0, plusgroup=NA,
			minyear=1, maxyear=1, startf=NA, endf=NA)),
        distribution = character(0),
        index        = new("FLQuant"),
        index.var    = new("FLQuant"),
		catch.n      = new("FLQuant"),
		catch.wt     = new("FLQuant"),
		effort       = new("FLQuant"),
		sel.pattern  = new("FLQuant"),
		index.q      = new("FLQuant")),
    validity=validFLIndex
)

setValidity("FLIndex", validFLIndex)
remove(validFLIndex)    #   }}}

# FLModel  {{{
validFLModel <- function(object)
{
  # All FLArray slots are of the same exact class
  flarr <- getSlotNamesClass(object, 'FLArray')
  class <- class(slot(object, flarr[1]))
  for(i in flarr[-1])
    if(class(slot(object, i)) != class)
      return(paste('FLQuant/FLCohort slots in object should all be of the same class: ',
        i))
  
  # initial returns an FLPar
  init <- do.call(initial(object), lapply(formals(initial(object)), function(x) x<-0.1))
  if(!is.null(init) & !is(init, 'FLPar'))
    return("initial function must return an 'FLPar'")

  return(TRUE)
}
setClass('FLModel',
  representation('FLComp',
    model='formula',
    logl='function',
    gr='function',
    distribution='factor',
    initial='function',
    params='FLPar',
    logLik='logLik',
    vcov='array',
    hessian='array',
    details='list',
    residuals='FLArray',
    fitted='FLArray'),
  prototype(name=character(0),
    desc=character(0),
    range=unlist(list(min=NA, max=NA, minyear=1, maxyear=1)),
    model=formula(NULL),
    distribution=factor(levels=c('beta', 'dbinom', 'cauchy', 'chisq', 'exp',
        'f', 'gamma', 'geom', 'hyper', 'lnorm', 'multinom', 'nbinom', 'norm',
        'pois', 't', 'unif', 'weibull')),
    fitted=FLQuant(),
    residuals=FLQuant())
)
invisible(createFLAccesors("FLModel", exclude=c('name', 'desc', 'range', 'params', 'distribution')))  # }}}

# FLlst class{{{
vFLl <- function(object){

	# Make sure the list contains all items of the same class
	cls <- unlist(lapply(object, class))
  if(any(cls != cls[1]))
	  stop("Components must be of the same class!")	

  # All elements in the list are validObjects themselves
  if(!all(unlist(lapply(object, validObject))))
	  stop("Components must be valid objects themselves (validObject == TRUE)")	

	# Everything is fine
	return(TRUE)
}

# class
setClass("FLlst", contains="list",
  representation(names="character", desc="character", lock="logical"),
	prototype(lock=FALSE),
	validity=vFLl
) # }}}

# FLQuants {{{
# validity
vFLQs <- function(object){
	# Make sure the list contains all items of the same class
	for(i in 1:length(object)){
		if(!is(object[[i]], "FLQuant")) stop("Components must be FLQuant")	
	}
	# Everything is fine
	return(TRUE)
}

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

# FLSR  {{{
validFLSR <- function(object)
{
	return(TRUE)
}
setClass('FLSR',
  representation(
	  'FLModel',
  	rec='FLQuant',
	  ssb='FLQuant',
  	covar='FLQuants',
    logerror='logical'),
  prototype(residuals=FLQuant(), fitted=FLQuant(), logerror=TRUE, covar=new('FLQuants')),
	validity=validFLSR)
remove(validFLSR)

invisible(createFLAccesors("FLSR", include=c('rec', 'ssb', 'covar'))) # }}}

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
    lst <- c(object, list(...))
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
vFLSs <- function(object){
	
  # All items are FLIndex
  if(!all(unlist(lapply(object, is, 'FLIndex'))))
      return("Components must be FLIndex")	
	
	return(TRUE)
}

# class
setClass("FLIndices", contains="FLComps",
	validity=vFLSs
)

# constructor
setMethod("FLIndices", signature(object="FLIndex"), function(object, ...) {
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
          names[idx] <- as.character(length(names))[idx]
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

# FLSRs {{{
vFLSRs <- setClass("FLSRs", contains="FLComps",
	validity=function(object) {
    # All items are FLSR
    if(!all(unlist(lapply(object, is, 'FLSR'))))
      return("Components must be FLSR")	
	
	  return(TRUE)
  }
)

# constructor
setMethod("FLSRs", signature(object="FLSR"), function(object, ...) {
    lst <- c(object, list(...))
    FLSRs(lst)
})

setMethod("FLSRs", signature(object="missing"),
  function(...) {
    # empty
  	if(missing(...)){
	  	new("FLSRs")
    # or not
  	} else {
      args <- list(...)
      object <- args[!names(args)%in%c('names', 'desc', 'lock')]
      args <- args[!names(args)%in%names(object)]
      do.call('FLSRs',  c(list(object=object), args))
	  }
  }
)

setMethod("FLSRs", signature(object="list"),
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
    args <- c(list(Class="FLSRs", .Data=object, names=names),
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
