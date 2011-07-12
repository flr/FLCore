# FLArray-class - Base class for FLQuant and FLCohort

# Copyright 2003-2008 FLR Team. Distributed under the GPL 2 or later
# Maintainer: Iago Mosqueira, JRC
# $Id$

# units {{{
setMethod("units", signature(x="FLArray"),
	function(x)
		return(x@units)
) # }}}

# units<- {{{
setMethod("units<-", signature(x="FLArray", value="character"),
	function(x, value) {
		x@units <- value
		return(x)
	}
) # }}}

# quant        {{{
setMethod("quant", signature(object="FLArray"),
	function(object)
  {
		return(names(dimnames(object))[1])
	}
) # }}}

# quant<-      {{{
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
      if(length(list(...)) > 0)
        stop(paste(class(x), 'objects only have 6 dimensions'))
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

      if(drop)
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
      dimn[[d]] <- dimn[[d]][apply(i@.Data, d, any, FALSE)==TRUE]

    if(length(x@.Data[i]) != prod(unlist(lapply(dimn, length))))
      return(x@.Data[i])
    #      stop("Selected elements do not form a coherent 6D array")
    else
      return(new(class(x), array(x@.Data[i], dimnames=dimn,
        dim=unlist(lapply(dimn, length)))))
  }
)   # }}}

## "[<-"            {{{
setMethod("[<-", signature(x="FLArray"),
  function(x, i, j, k, l, m, n, ..., value)
  {
    if(length(list(...)) > 0)
      stop(paste(class(x), 'objects only have 6 dimensions'))

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

    #
		x@.Data[i,j,k,l,m,n] <- value

   	return(x)
	}
)   # }}}

## names         {{{
setMethod("names", signature(x="FLArray"),
	function(x)
    names(dimnames(x))
)
# }}}

# iter     {{{
setMethod("iter", signature(object="FLArray"),
	function(object, iter) {
    if(dims(object)$iter == 1)
      return(object)
    else
      return(object[,,,,,iter])
	}
)   # }}}

## summary          {{{
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
setMethod('expand', signature(x='FLArray'),
  function(x, ...) {

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
    
    # check new dimnames contain old ones
    for(i in names(select))
      if(!dimnames[[i]] %in% select[[i]])
        stop("New dimnames do not contain existing ones")
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
setMethod("as.data.frame", signature(x="FLArray", row.names="missing",
  optional="missing"),
	function(x) {
    as(x, 'data.frame')
  }
)
setMethod("as.data.frame", signature(x="FLArray", row.names="ANY",
  optional="missing"),
	function(x, row.names=NULL) {
    df <- as(x, 'data.frame')
    row.names(df) <- row.names
    return(df)
  }
) # }}}

# scale {{{
setMethod("scale", signature(x="FLArray", center="ANY", scale="ANY"),
  function(x, center, scale)
  {
    new(class(x), array(scale(x@.Data, center=center, scale=scale), dim=dim(x),
      dimnames=dimnames(x)), units=units(x))
  }
)

setMethod("scale", signature(x="FLArray", center="ANY", scale="missing"),
  function(x, center)
  {
    new(class(x), array(scale(x@.Data, center=center, scale=TRUE), dim=dim(x),
      dimnames=dimnames(x)), units=units(x))
  }
)

setMethod("scale", signature(x="FLArray", center="missing", scale="ANY"),
  function(x, scale)
  {
    new(class(x), array(scale(x@.Data, center=TRUE, scale=scale), dim=dim(x),
      dimnames=dimnames(x)), units=units(x))
  }
)

setMethod("scale", signature(x="FLArray", center="missing", scale="missing"),
  function(x)
  {
    new(class(x), array(scale(x@.Data, center=TRUE, scale=TRUE), dim=dim(x),
      dimnames=dimnames(x)), units=units(x))
  }
) # }}}

# sweep {{{
setMethod('sweep', signature(x='FLArray'),
  function(x, MARGIN, STATS, FUN, check.margin=TRUE, ...)
  {
    res <- callNextMethod()
    do.call(class(x), list(res, units=units(x)))
  }
) # }}}

# sigma {{{
setMethod('sigma', signature(object='FLArray'),
  function(object, hat=rep(0, length(object)))
  {
    ## calculates sigma squared for use in concentrated likelihood
    if(all(is.na(hat)))
      return(Inf)

    SS <- sum((object - hat) ^ 2, na.rm=TRUE)

    return((SS/length(hat[!is.na(hat)])) ^ 0.5)
   }
) # }}}

# qmax, qmin {{{
setMethod("qmax", signature(x="FLArray"),
  function(x, ..., na.rm=TRUE)
  {
    args <- c(list(x), list(...))
    args <- lapply(args, function(x) x@.Data)
    res <- do.call(pmax, args)
    do.call(class(x), list(object=res, units=units(x)))
  }
) 
setMethod("qmin", signature(x="FLArray"),
  function(x, ..., na.rm=TRUE) 
  {
    args <- c(list(x), list(...))
    args <- lapply(args, function(x) x@.Data)
    res <- do.call(pmin, args)
    do.call(class(x), list(object=res, units=units(x)))
  }
)

# }}}

# apply {{{
setMethod("apply", signature(X="FLArray", MARGIN="numeric", FUN="function"),
	function(X, MARGIN, FUN, ...)
  {
	data <- apply(X@.Data, MARGIN, FUN, ...)
	if(length(dim(data))<=length(MARGIN)){
		# set dim
		dim <- c(1,1,1,1,1,1)
		# if apply generated a new dimension
		if (is.null(dim(data)))
			dim[MARGIN] <- length(data)
		else
			dim[MARGIN] <- dim(data)
		# new object
		flq <- array(NA, dim=dim)
		# inject data
		flq[1:dim[1],1:dim[2],1:dim[3],1:dim[4],1:dim[5],1:dim[6]] <- data
		# set dimnames
		MRG <- dim(X) == dim(flq)
    if(all(MRG))
      dimnames(flq) <- dimnames(X)
    else
    {
		  dimnames(flq)[MRG] <- dimnames(X)[MRG]
  		dimnames(flq)[!MRG] <- dimnames(new(class(X)))[!MRG]
	  	names(dimnames(flq)) <- names(dimnames(X))
    } 
		# new FLobject
		flq <- new(class(X),flq)
		# set quant
		if(is(flq, 'FLQuant')) quant(flq) <- quant(X)
		return(flq)
	} else {
		return(data)
	}
})   # }}}

# survprob {{{
# estimate survival probabilities by year or cohort
setMethod("survprob", signature(object="FLArray"),
	function(object, ...) {
		
		ps <- mm <- object

		# estimate by year
			ps[1,,,,,] <- 1	
			for(a in 2:dim(ps)[1])
				ps[a,,,,,] <- ps[a-1,,,,,]*exp(-mm[a-1,,,,,])

		return(ps)
	}
) # }}}

## window           {{{
setMethod("window", signature="FLArray",
	function(x, start=dims(x)$minyear, end=dims(x)$maxyear, extend=TRUE, frequency=1)
  {
		# get original min and max
		min <- dims(x)$minyear
		max <- dims(x)$maxyear

    # if extend=FALSE and end/start ask for it, error
		if(!extend && (start < min | end > max))
			stop("FLQuant to be extended but extend=FALSE")

    # if extend is a number, added to end
    if(is.numeric(extend))
        if (missing(end))
          end <- dims(x)$maxyear + extend
        else
          stop("'extend' is numeric and 'end' provided, don't know what to do")
		
    # construct new FLQuant
		years <- seq(start, end, by=frequency)
    dnames <- dimnames(x)
    dnames[[2]] <- years
    flq <- do.call(class(x), list(NA, units=units(x), dimnames=dnames))

		# add data for matching years
		flq[,dimnames(x)$year[dimnames(x)$year%in%as.character(years)],,,]  <-
			x[,dimnames(x)$year[dimnames(x)$year%in%as.character(years)],,,]

		return(flq)
	}
)   # }}}
