# FLPar - common structure for parameter matrices of various types.
# FLCore/R/FLPar.R

# Copyright 2003-2007 FLR Team. Distributed under the GPL 2 or later
# Maintainer: Iago Mosqueira, Cefas
# $Id$

# Reference:
# Notes:

# Validity  {{{
validFLPar <- function(object) {

	# Last dimension is called iter
  if(names(dimnames(object))[length(dim(object))] != "iter")
    return("last dimension must be named 'iter'")

	return(TRUE)
}   # }}}

# FLPar {{{
setClass('FLPar', representation('array', units='character'),
	prototype=prototype(array(as.numeric(NA), dim=c(1,1),
	dimnames=list(param="", iter=1)), units='NA'), validity=validFLPar)
remove(validFLPar)
# }}}

# Constructors  {{{
if (!isGeneric("FLPar"))
	setGeneric("FLPar", function(object, ...)
		standardGeneric("FLPar"))

# FLPar(array)
setMethod('FLPar', signature(object="array"),
	function(object, params=letters[1:dim(object)[1]],
    iter=seq(dim(object)[length(dim(object))]), units=rep('NA', dim(object)[1]),
    dimnames= c(list(params=params), lapply(as.list(dim(object)[-c(1,
      length(dim(object)))]), seq), list(iter=iter)))
	{
    # if no dimnames, 1st is params, last is iter
    if(!is.null(dimnames(object)))
    {
      dimnames <- dimnames(object)
      pnames <- match(c('params', 'iter'), names(dimnames))
      object <- aperm(object, c(pnames[1], seq(1, length(dimnames))[!seq(1,
        length(dimnames)) %in% pnames], pnames[2]))
      dimnames <- dimnames(object)
    }
		
    res <- array(object, dim=dim(object), dimnames=dimnames)
		return(new('FLPar', res, units=units))
	}
)
	
# FLPar(missing, iter, param)
setMethod('FLPar', signature(object="missing"),
	function(params='a', iter=1, dimnames=list(params=params, iter=seq(iter)), units='NA')
	{
		res <- array(as.numeric(NA), dim=unlist(lapply(dimnames, length)),
      dimnames=dimnames)
		return(FLPar(res, units=units, dimnames=dimnames(res)))
	}
)

# FLPar(vector)
setMethod('FLPar', signature('vector'),
	function(object, params=letters[seq(length(object)/length(iter))], iter=1, 
    dimnames=list(params=params, iter=seq(iter)), byrow=FALSE, units='NA')
  {
    # if length(iter) == 1, then expand
    if(length(iter) == 1 && as.character(iter) != '1')
      iter <- seq(iter)

		res <- array(object,dim=unlist(lapply(dimnames, length)))
		return(FLPar(res, units=units, dimnames=dimnames))
	}
)

# FLPar(FLPar)
setMethod('FLPar', signature('FLPar'),
	function(object, dimnames=attr(object, 'dimnames'), params=dimnames$params,
    iter=dimnames$iter, units=object@units)
  {
    # get iter as vector if single number given
    if(!missing(iter) && length(iter) == 1 && ac(iter) != '1')
      iter <- ac(seq(as.numeric(iter)))

    dimnames$params <- params
    dimnames$iter <- ac(iter)
    res <- FLPar(NA, dimnames=dimnames, units=units)
    
    # select target dimnames and change names for '[<-'
    dimnames <- dimnames(object)
    names(dimnames) <- letters[seq(9,length=length(dimnames))]
    return(do.call('[<-', c(list(res), dimnames, list(value=object))))
	}
) # }}}

# '['   {{{
setMethod('[', signature(x='FLPar'),
    function(x, i='missing', j='missing', ..., drop=FALSE)
    {
		  dx <- dim(x)
  		if (missing(i))
        i  <-  seq(1, dx[1])
      if (missing(j))
        j  <-  seq(1, dx[2])
      
      if(drop)
        return(x@.Data[i, j, ..., drop=TRUE])

      if(length(dx) == 2)
        return(new(class(x), as.array(x@.Data[i, j, drop=FALSE])))
      else
      {
        # if ... is missing, list(...) fails
		  	args <- try(list(...), silent=TRUE)
        # so create a list using dx
        if(class(args) == 'try-error')
          k <- lapply(as.list(dx[-c(1,2)]), function(x) seq(1:x))
        else
        # and use only the section required when list(...) exists
          k <- c(args, lapply(as.list(dx[-seq(1:(2+length(args)))]),
            function(x) seq(1:x)))

        return(new(class(x), as.array(do.call('[',
          c(list(x@.Data, i, j), k, list(drop=FALSE))))))
      }
    }
)   # }}}

# "[<-"     {{{
setMethod("[<-", signature(x="FLPar"),
	function(x, i="missing", j="missing", ..., value="missing")
  {
    if(!missing(i) && is.array(i))
    {
			x@.Data[i] <- value
			return(x)
		}
    
    dx <- dim(x)
    if(missing(i))
			i  <-  seq(1, length(dimnames(x@.Data)[1][[1]]))
		if (missing(j))
			j  <-  dimnames(x@.Data)[2][[1]]

    if(length(dim(x)) == 2)
      x@.Data[i, j] <- value
		else {
			args <- list(...)
			if(length(args) == 0)
				k <- seq(1, dx[3])
			else
				k <- args[[1]]
			x@.Data[i, j, k] <- value
    }
    return(x)
	}
)   # }}}

# iter, iter<-     {{{
setMethod("iter", signature(object="FLPar"),
	function(object, iter) {
    if(dim(object)[length(dim(object))] == 1)
      return(object)
    else {
    	lst <- list(x=object, pos=iter)
    	names(lst) <- c('x', letters[8+length(dim(object))])
    	return(do.call('[', lst))
	  }
	}
)
setMethod("iter<-", signature(object="FLPar", value="FLPar"),
	function(object, iter, value)
  {
    lst <- list(x=object, pos=iter, value=value)
    names(lst) <- c('x', letters[8+length(dim(object))], 'value')
    object <- do.call('[<-', lst)
		return(object)
	}
) 

setMethod("iter<-", signature(object="FLPar", value="numeric"),
	function(object, iter, value)
  {
    lst <- list(x=object, pos=iter, value=value)
    names(lst) <- c('x', letters[8+length(dim(object))], 'value')
    object <- do.call('[<-', lst)
		return(object)
	}
)

# }}}

# summary   {{{
setMethod('summary', signature(object='FLPar'),
	function(object, ...) {
		cat("An object of class \"", class(object), "\"\n\n", sep="")
    if(dim(object)[length(dim(object))] == 1)
	  	return(object)
    else
  		return(apply(object@.Data, seq(dim(object))[-length(dim(object))], summary))
  }
)   # }}}

# plots {{{

# plot
setMethod("plot", signature(x="FLPar", y="missing"),
	function(x, y="missing", ...) {
	  # get dimensions to condition on (skip iter)
		condnames <- names(dimnames(x))[names(dimnames(x)) != 'iter']
		cond <- paste(condnames, collapse="+")
		if(cond != "") cond <- paste("|", cond)
			formula <- formula(paste("~data", cond))
		# set strip to show conditioning dimensions names
		strip <- strip.custom(var.name=condnames, strip.names=c(TRUE,TRUE))

    do.call('densityplot', list(x=formula, data=as.data.frame(x, row.names='row'),
      ylab="", xlab="", scales=list(y=list(draw=FALSE), relation='free'), col='black'))
	}
)

# densityplot
if (!isGeneric("densityplot")) {
	setGeneric("densityplot", useAsDefault = densityplot)
}
setMethod("densityplot", signature("formula", "FLPar"), function(x, data, ...){
	lst <- substitute(list(...))
	lst <- as.list(lst)[-1]
	lst$data <- as.data.frame(data, row.names='row')
	lst$x <- x
	do.call("densityplot", lst)
})

# histogram
setMethod("histogram", signature("formula", "FLPar"), function(x, data, ...){
	lst <- substitute(list(...))
	lst <- as.list(lst)[-1]
	data <- as.data.frame(data)
	lst$data <- data.frame(param=rep(names(data), each=nrow(data)),
		data=as.vector(unlist(c(data))))
	lst$x <- x
	do.call("histogram", lst)
})

# splom
if (!isGeneric("splom")) {
	setGeneric("splom", useAsDefault = splom)
}

setMethod("splom", signature("FLPar", "missing"),
	function(x, data, ...){
		splom(as.data.frame(x))
	}
)   # }}}

# units        {{{
setMethod("units", signature(x="FLPar"),
	function(x)
		return(x@units)
) # }}}

# units<-      {{{
setMethod("units<-", signature(x="FLPar", value="character"),
	function(x, value) {
		x@units <- value
		return(x)
	}
) # }}}

# as.data.frame     {{{
setMethod("as.data.frame", signature(x="FLPar"),
	function(x, row.names='col', optional=FALSE)
	  return(data.frame(expand.grid(dimnames(x)), data=as.vector(x@.Data)))
)   # }}}

# mean, median, var, quantile   {{{
# TODO review for 3D param objects
setMethod("mean", signature(x='FLPar'),
	function(x, ...)
  	return(FLPar(apply(x, seq(1, length(dim(x)))[!names(dimnames(x))=='iter'],
      mean, ...)))
)

setMethod("median", signature(x='FLPar'),
	function(x, na.rm=FALSE)
  	return(FLPar(apply(x, seq(1, length(dim(x)))[!names(dimnames(x))=='iter'],
      median, na.rm=na.rm)))
)

setMethod("var", signature(x='FLPar'),
	function(x, y=NULL, na.rm=FALSE, use='all.obs')
  	return(FLPar(apply(x, seq(1, length(dim(x)))[!names(dimnames(x))=='iter'],
      var, na.rm=na.rm, use='all.obs')))
)   # }}}

# coerce  {{{
setAs('FLPar', 'numeric',
  function(from)
  {
    res <- as.vector(from[1,])
    names(res) <- dimnames(from)$param
    
    return(res)
  }
)
setAs('FLPar', 'list',
  function(from)
  {
    params <- dimnames(from)$params
    res <- vector("list", length = length(params))
    names(res) <- params
    for(i in params)
      res[[i]] <- as.vector(from[i,])

    return(res)
  }
)

setAs('FLQuant', 'FLPar',
  function(from)
  {
    # check quant(from) == 'params'
    if(quant(from) != 'params')
      stop("'quant' in FLQuant must be 'params'")

    # extract array with dims of length < 1 collapsed
    res <- from@.Data[,,,,,,drop=TRUE]

    res <- FLPar(res)

    if(validObject(res))
      return(res)
    else
      stop("created object is not valid, please check input")
  }
)

setAs('FLPar', 'FLQuant',
  function(from)
  {
    # extract array
    data <- from@.Data
    # and names
    names <- names(dimnames(data))

    # output FLQuant
    res <- FLQuant(quant='params', units=ifelse(all(units(from) == 'NA'),
      'NA', paste(units(from), collapse='_')))

    # reshape data for FLQuant dimnames
    idx <- match(names(res), names)
    idx <- idx[!is.na(idx)]
    aperm(data, idx)


    # get dim and dimnames for FLQuant
    idx <- names(res) %in% names(dimnames(data))
    dim <- rep(1,6)
    dim[idx] <- dim(data)
    dnames <- dimnames(res)
    dnames[idx] <- dimnames(data)
  
    res@.Data <- array(data, dim=dim, dimnames=dnames)

    return(res)

  }
)


# }}}

# propagate {{{
setMethod("propagate", signature(object="FLPar"),
  function(object, iter, fill.iter=FALSE)
  {
    # dimnames of input object
    dnames <- dimnames(object)
    dnames$iter <- seq(iter)
    # new object
    res <- FLPar(object, dimnames=dnames)
    # fill.iter
    if(fill.iter == TRUE)
    {
      args <- list(x=res, iter=seq(iter)[-1], value=object)
      names(args)[2] <- letters[seq(9,
        length=length(dim(res)))][names(dimnames(res))=='iter']
      res <- do.call('[<-', args)
    }
    return(res)
  }
) # }}}

## dims       {{{
setMethod("dims", signature(obj="FLPar"),
	# Return a list with different parameters
	function(obj, ...){
		iter <- as.numeric(dimnames(obj)$iter)
    params <- dimnames(obj)$params
		return(list(iter=iter, params=params))
	}
)   # }}}

## names         {{{
setMethod("names", signature(x="FLPar"),
	function(x)
    names(dimnames(x))
)
# }}}

## names<-         {{{
if (!isGeneric("names<-"))
	setGeneric("names<-")

setMethod("names<-", signature(x="FLPar", value="character"),
	function(x, value)
  {
    # last dim must be 'iter', as in validFLPar
    if(value[length(value)] != 'iter')
      stop("last dimension must be named 'iter'")
    
    names(dimnames(x)) <- value
    return(x)
  }
)
# }}}
