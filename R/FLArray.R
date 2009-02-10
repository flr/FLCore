# FLArray-class - Base class for FLQuant and FLCohort

# Copyright 2003-2008 FLR Team. Distributed under the GPL 2 or later
# Maintainer: Iago Mosqueira, Cefas
# $Id$

## Class {{{
validFLArray  <-  function(object){
	# Make sure there are at least 6 dimensions in the array
	Dim  <-  dim(object)
	if (length(Dim) != 6) 
		return("the array must have 6 dimensions")

	if (!is.numeric(object) && !is.na(object)) 
		return("array is not numeric")

	# check "units" slot
	if(!is.character(object@units))
		return("units must be a string")
	# Everything is fine
	return(TRUE)
}

setClass("FLArray",	representation("array", units="character"),
	prototype(array(as.numeric(NA), dim=c(1,1,1,1,1,1)), units="NA"),
	validity=validFLArray
) # }}}

# units {{{
if (!isGeneric("units"))
	setGeneric("units", useAsDefault=units)
setMethod("units", signature(x="FLArray"),
	function(x)
		return(x@units)
) # }}}

# units<- {{{
if (!isGeneric("units<-"))
	setGeneric("units<-", function(x, value)
		standardGeneric("units<-"))

setMethod("units<-", signature(x="FLArray", value="character"),
	function(x, value) {
		x@units <- value
		return(x)
	}
) # }}}

# quant        {{{
if (!isGeneric("quant"))
	setGeneric("quant", function(object, ...)
		standardGeneric("quant"))

setMethod("quant", signature(object="FLArray"),
	function(object)
  {
		return(names(dimnames(object))[1])
	}
) # }}}

# quant<-      {{{
if (!isGeneric("quant<-"))
	setGeneric("quant<-", function(object, value)
		standardGeneric("quant<-"))

setMethod("quant<-", signature(object="FLArray", value='character'),
	function(object, value)
  {
    if(length(value) > 1)
      stop('quant must be a single character string')
		names(attributes(object)$dimnames) <- c(value, names(dimnames(object))[-1])
		return(object)
	}
) # }}}

## "["             {{{
setMethod("[", signature(x="FLArray"),
    function(x, i, j, k, l, m, n, ..., drop=FALSE)
    {
	  	dx <- dim(x)
		  if (missing(i))
        i  <-  seq(1, dx[1])
      if (missing(j))
        j  <-  seq(1, dx[2])
      if (missing(k))
        k  <-  seq(1, dx[3])
      if (missing(l))
        l  <-  seq(1, dx[4])
      if (missing(m))
        m  <-  seq(1, dx[5])
      if (missing(n))
        n  <-  seq(1, dx[6])

      if(drop == TRUE)
        return(x@.Data[i, j, k, l, m, n, drop=TRUE])
      else
        x@.Data <- x@.Data[i, j, k, l, m, n, drop=FALSE]
      return(x)
	}
)

setMethod("[", signature(x="FLArray", i="array"),
  function(x, i, j=missing, ..., drop=missing)
  {
    dimn <- dimnames(i)
    for(d in 1:6)
      dimn[[d]] <- dimn[[d]][apply(i, d, any, FALSE)==TRUE]

    if(length(x@.Data[i]) != prod(unlist(lapply(dimn, length))))
      stop("Selected elements do not form a coherent 6D array")
    return(new(class(x), array(x@.Data[i], dimnames=dimn,
      dim=unlist(lapply(dimn, length)))))
  }
)   # }}}

## "[<-"            {{{
setMethod("[<-", signature(x="FLArray"),
	function(x, i, j, k, l, m, n, ..., value="missing")
  {
		if(!missing(i) && is.array(i))
    {
			x@.Data[i] <- value
			return(x)
		}

		dx <- dim(x)
		if (missing(i))
            i  <-  seq(1, dx[1])
        if (missing(j))
            j  <-  seq(1, dx[2])
        if (missing(k))
            k  <-  seq(1, dx[3])
        if (missing(l))
            l  <-  seq(1, dx[4])
        if (missing(m))
            m  <-  seq(1, dx[5])
        if (missing(n))
            n  <-  seq(1, dx[6])

		x@.Data[i,j,k,l,m,n] <- value

   	return(x)
	}
)   # }}}

## names         {{{
if (!isGeneric("names"))
	setGeneric("names")
setMethod("names", signature(x="FLArray"),
	function(x)
    names(dimnames(x))
)
# }}}

# iter     {{{
setGeneric("iter", function(object, ...)
	standardGeneric("iter"))

setMethod("iter", signature(object="FLArray"),
	function(object, iter) {
    if(dims(object)$iter == 1)
      return(object)
    else
      return(object[,,,,,iter])
	}
)   # }}}

## summary          {{{
if (!isGeneric("summary")) {
	setGeneric("summary", useAsDefault = summary)
}
setMethod("summary", signature(object="FLArray"),
	function(object, ...)
	{
		cat("An object of class \"", as.character(class(object)), "\" with:\n", sep="")
		cat("dim  : ", dim(object), "\n")
		cat("quant: ", quant(object), "\n")
		cat("units: ", units(object), "\n\n")
		if(all(is.na(object)))
		{
			cat("Min    :  NA\n")
			cat("1st Qu.:  NA\n")
			cat("Mean   :  NA\n")
			cat("Median :  NA\n")
			cat("3rd Qu.:  NA\n")
			cat("Max    :  NA\n")
		}
		else
		{
			cat("Min    : ", min(object, na.rm=TRUE), "\n")
			cat("1st Qu.: ", quantile(as.vector(object), 0.25, na.rm=TRUE), "\n")
			cat("Mean   : ", mean(as.vector(object), na.rm=TRUE), "\n")
			cat("Median : ", median(as.vector(object), na.rm=TRUE), "\n")
			cat("3rd Qu.: ", quantile(as.vector(object), 0.75, na.rm=TRUE), "\n")
			cat("Max    : ", max(object, na.rm=TRUE), "\n")
		}
		cat("NAs    : ", format(length(as.vector(object)
			[!complete.cases(as.vector(object))])/length(as.vector(object))*100,
			digits=2), "%\n")
	}
)   # }}}

## show     {{{
setMethod("show", signature(object="FLArray"),
	function(object){
		cat("An object of class \"", as.character(class(object)), "\"\n", sep="")
		if(dim(object)[6] != 1)
			cat("iters: ", dim(object)[6],"\n\n")
    if(dim(object)[6] > 1)
    {
		  v1 <- apply(object@.Data, 1:5, median, na.rm=TRUE)
  		v2 <- apply(object@.Data, 1:5, mad, na.rm=TRUE)	 
      v3 <- paste(format(v1,digits=5),"(", format(v2, digits=3), ")", sep="")
    }
    else
      v3 <- paste(format(apply(object@.Data, 1:5, median, na.rm=TRUE),digits=5))
		
    print(array(v3, dim=dim(object)[1:5], dimnames=dimnames(object)[1:5]), quote=FALSE)

		cat("units: ", object@units, "\n")
	}
)   # }}}

# trim {{{
setGeneric("trim", function(x, ...)
	standardGeneric("trim"))
setMethod('trim', signature(x='FLArray'),
  function(x, ...)
  {
    args <- list(...)
    nargs <- names(args)

    # dimension names
    qnames <- names(dimnames(x))
    
    # check input names match dimnames
    if(!all(nargs%in%qnames))
      stop(paste("Wrong dimension name provided: ", nargs[!nargs%in%qnames]))

    # Create list with given standard elements in right position ...
    select <- args[match(qnames, nargs)]

    # change names to those for '['
    names(select) <- c('i', 'j', 'k', 'l', 'm', 'n')
    
    # get rid of unspecified dimensions
    select <- select[!unlist(lapply(select, is.null))]
    
    # turn into characters
    select <- lapply(select, as.character)
    
    do.call('[', c(list(x=x), select))
  }
) # }}}

# expand {{{
setGeneric("expand", function(x, ...)
	standardGeneric("expand"))
setMethod('expand', signature(x='FLArray'),
  function(x, ...)
  {
    args <- list(...)
    nargs <- names(args)
    
    # dimension names
    qnames <- names(dimnames(x))
    
    # check input names match dimnames
    if(!all(nargs%in%qnames))
      stop(paste("Wrong dimension name provided: ", nargs[!nargs%in%qnames]))

    # Create list with given standard elements in right position ...
    select <- args[match(qnames, nargs)]
    
    # get rid of unspecified dimensions
    select <- select[!unlist(lapply(select, is.null))]
    
    # turn into characters
    select <- lapply(select, as.character)
    
    # match specified dimensions and dimnames
    dimnames <- dimnames(x)
    dimnames[names(select)] <- select

    # output object
    res <- new(class(x), array(as.numeric(NA), dimnames=dimnames, dim= unlist(lapply(dimnames, length))), units=units(x))
    
    # list for assignment of x data
    dimnames <- dimnames(x)
    names(dimnames) <- c('i', 'j', 'k', 'l', 'm', 'n')

    do.call('[<-', c(list(x=res, value=x), dimnames))
  }
) # }}}

## Arith    {{{
setMethod("Arith", ##  "+", "-", "*", "^", "%%", "%/%", "/"
	signature(e1 = "numeric", e2 = "FLArray"),
	function(e1, e2)
	{
		return(new(class(e2), callGeneric(e1, e2@.Data), units=units(e2)))
	}
)
setMethod("Arith",
	signature(e1 = "FLArray", e2 = "numeric"),
	function(e1, e2)
	{
		return(new(class(e1), callGeneric(e1@.Data, e2), units=units(e1)))
	}
)
setMethod("Arith",
	signature(e1 = "FLArray", e2 = "FLArray"),
	function(e1, e2)
	{
    if(!all(dim(e1)[-6] == dim(e2)[-6]))
      stop("non-conformable arrays")
		if(dim(e1)[6] == 1 & dim(e2)[6] > 1) {
			e <- e2
			e[,,,,,] <- e1
			e <- array(callGeneric(unclass(e), unclass(e2)),
        dimnames=dimnames(e2), dim=dim(e2))
		}
		else if(dim(e2)[6] == 1 & dim(e1)[6] > 1) {
			e <- e1
			e[,,,,,] <- e2
			e <- array(callGeneric(unclass(e1), unclass(e)),
        dimnames=dimnames(e1), dim=dim(e1))
		}
		else
			e <- array(callGeneric(drop(unclass(e1)), drop(unclass(e2))),
        dimnames=dimnames(e1), dim=dim(e1))
    return(new(class(e1), e, units=paste(units(e1), units(e2))))
	}
)   # }}}

## as.data.frame        {{{
if (!isGeneric("as.data.frame")) {
	setGeneric("as.data.frame", useAsDefault = as.data.frame)
}
setMethod("as.data.frame", signature(x="FLArray", row.names="missing",
  optional="missing"),
	function(x, row.names="missing", optional="missing")
	{
		# to avoid warnings when NA have to be added
		options(warn=-1)
        if(any(is.na(suppressWarnings(as.numeric(dimnames(x)[[1]])))))
            quant <- as.character(dimnames(x)[[1]])
        else
            quant <- as.numeric(dimnames(x)[[1]])
		df <- data.frame(expand.grid(quant=quant,
			year=as.numeric(dimnames(x)[[2]]),
			unit=factor(dimnames(x)[[3]], levels=dimnames(x)[[3]]),
			season=factor(dimnames(x)[[4]], levels=dimnames(x)[[4]]),
			area=factor(dimnames(x)[[5]], levels=dimnames(x)[[5]]),
			iter=factor(dimnames(x)[[6]], levels=dimnames(x)[[6]])),
			data=as.vector(x))
		names(df)[1:2] <- names(dimnames(x))[1:2]
		attributes(df)$units <- units(x)
		options(warn=0)
		return(df)
	}
)   # }}}

