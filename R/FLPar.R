# FLPar - common structure for parameter matrices of various types.
# FLCore/R/FLPar.R

# Copyright 2003-2007 FLR Team. Distributed under the GPL 2 or later
# Maintainer: Iago Mosqueira, JRC
# $Id$


# Constructors  {{{

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
      
      # if iter missing, add it
      if(!any(names(dimnames) == "") & !'iter' %in% names(dimnames))
      {
        dimnames <- c(dimnames, list(iter=1))
        object <- array(object, dimnames=dimnames, dim=c(unlist(lapply(dimnames, length))))
      }
      
      # dimnames with no names, last one is iter ...
      if(names(dimnames)[length(dimnames)] == "")
        names(dimnames)[length(dimnames)] <- 'iter'
      # ... others are dim*
      if(any(names(dimnames) == ""))
        names(dimnames)[names(dimnames) == ""] <-
          paste('dim', seq(sum(names(dimnames) == "")))

      # forcing iter to be last dim, all others as given
      iterpos <- match(c('iter'), names(dimnames))
      object <- aperm(object, c(seq(1, length(dimnames))[!seq(1,length(dimnames)) %in%
        iterpos], iterpos))
    }
		
    res <- array(object, dim=dim(object), dimnames=dimnames)
		return(new('FLPar', res, units=units))
	}
)
	
# FLPar(missing, iter, param)
setMethod('FLPar', signature(object="missing"),
	function(..., params='a', iter=1, dimnames=list(params=params, iter=seq(iter)),
      units=rep('NA', length(params)))
	{
    args <- list(...)
    if(length(args) > 0)
    {
      len <- length(args[[1]])
      res <- array(NA, dim=c(length(args),len), 
        dimnames=list(params=names(args), iter=seq(len)))
      for (i in seq(length(args)))
        res[i,] <- args[[i]]
    }
    else
      res <- array(as.numeric(NA), dim=unlist(lapply(dimnames, length)),
        dimnames=dimnames)
		return(FLPar(res, units=units, dimnames=dimnames(res)))
	}
)

# FLPar(vector)
setMethod('FLPar', signature(object='vector'),
	function(object, params= if(length(names(object))==length(object)) names(object) else
    letters[seq(length(object)/length(iter))], iter=1,
    dimnames=list(params=params, iter=seq(iter)), byrow=FALSE, units=rep('NA', length(params)))
  {
    # if length(iter) == 1, then expand
    if(length(iter) == 1 && as.character(iter) != '1')
      iter <- seq(iter)

		res <- array(object,dim=unlist(lapply(dimnames, length)))
		return(FLPar(res, units=units, dimnames=dimnames))})

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
      
      if(length(dx) == 2)
        if(!drop)
          return(new(class(x), as.array(x@.Data[i, j, drop=FALSE])))
        else
          return(x@.Data[i, j, drop=FALSE])
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
        if(!drop)
          return(new(class(x), as.array(do.call('[',
            c(list(x@.Data, i, j), k, list(drop=FALSE))))))
        else
          return(do.call('[', c(list(x@.Data, i, j), k, list(drop=TRUE))))
      }
    }
)   # }}}

# "[<-"     {{{
setMethod("[<-", signature(x="FLPar"),
	function(x, i, j, ..., value)
  {
    if(!missing(i) && is.array(i))
    {
			x@.Data[i] <- value
			return(x)
		}
    
    dx <- dimnames(x)
    names(dx) <- letters[seq(9, length=length(dx))]

    # set up for dims 1 and 2
    if(!missing(i))
      dx$i <- i
		if (!missing(j))
      dx$j <- j

    # extra args, if given
    args <- try(list(...))
    if(is.list(try))
    {
      if (length(args) > 0)
      {
        names(args) <- letters[seq(11, length=length(args))]
        dx[names(args)] <- args
      }
    }
    x <- new(class(x), do.call('[<-', c(list(x=x@.Data), dx, list(value=value))))

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
	function(x, row.names='col', optional=FALSE, drop=FALSE) {
	  res <- as(x, 'data.frame')
    if(drop) {
      idx <- names(x)[dim(x) > 1]
      res <- res[, c(idx, 'data')]
    }
    return(res)
  }
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
  function(object, iter, fill.iter=TRUE)
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
	function(obj, ...) {
    dimnames(obj)
    names(obj)
		iter <- length(dimnames(obj)$iter)
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

## show     {{{
setMethod("show", signature(object="FLPar"),
	function(object) {
    ndim <- length(dim(object))
		cat("An object of class \"", as.character(class(object)), "\"\n", sep="")
		if(dim(object)[ndim] != 1)
			cat("iters: ", dim(object)[ndim],"\n\n")
    if(dim(object)[ndim] > 1)
    {
		  v1 <- apply(object@.Data, 1:(ndim-1), median, na.rm=TRUE)
  		v2 <- apply(object@.Data, 1:(ndim-1), mad, na.rm=TRUE)	 
      v3 <- paste(format(v1,digits=5),"(", format(v2, digits=3), ")", sep="")
    }
    else
			v3 <- format(object@.Data, digits=5)
		
    print(array(v3, dim=dim(object)[1:(ndim-1)], dimnames=dimnames(object)[1:(ndim-1)]),
      quote=FALSE)

		cat("units: ", object@units, "\n")
	}
)   # }}}

## Arith    {{{
setMethod("Arith", ##  "+", "-", "*", "^", "%%", "%/%", "/"
  signature(e1 = "FLPar", e2 = "FLPar"),
  function(e1, e2)
  {
    return(new('FLPar', callGeneric(e1@.Data, e2@.Data)))
  }
)

setMethod("Arith", signature(e1 = "FLArray", e2 = "FLPar"),
  function(e1, e2) {

    # objects dims
    d1 <- dim(e1)
    d2 <- dim(e2)
    l2 <- length(d2)
    n1 <- names(dimnames(e1))
    n2 <- names(dimnames(e2))

    # dims of length > 1 (except iter), must be in FLArray
    if(any(!(n2[-l2][d2[-l2] > 1]) %in% (n1[-6][d1[-6] > 1])))
      stop(paste("FLPar object cannot have dimensions of length > 1 not in",
        ac(class(e1))))

    # reshape FLPar
    m2 <- match(n1, n2)
    m2 <- c(1, m2[!is.na(m2)])
    e2 <- aperm(e2, m2)

    # iter of output
    it <- max(d1[6], d2[l2])

    # iters from FLQ
    if(d1[6] >= d2[l2]) {
      return(new(class(e1), array(callGeneric(e1@.Data, array(e2, dim=c(d1[-6], it))),
        dim=d1, dimnames=dimnames(e1)), units=units(e1)))
    }
    else {
      return(new(class(e1), array(callGeneric(array(e1@.Data, dim=c(c(d1[-6], it))),
        array(e2, dim=c(d1[-6], it))), dim=c(d1[-6], it),
        dimnames=c(dimnames(e1)[-6], list(iter=seq(it)))), units=units(e1))
      )
    }
  }
)

setMethod("Arith", signature(e1 = "FLPar", e2 = "FLArray"),
  function(e1, e2) {

    # objects dims
    d1 <- dim(e1)
    d2 <- dim(e2)
    l1 <- length(d1)
    n1 <- names(dimnames(e1))
    n2 <- names(dimnames(e2))

    # dims of length > 1 (except iter), must be in FLArray
    if(any(!(n1[-l1][d1[-l1] > 1]) %in% (n2[-6][d2[-6] > 1])))
      stop(paste("FLPar object cannot have dimensions of length > 1 not in",
        ac(class(e2))))

    # reshape FLPar
    m1 <- match(n2, n1)
    m1 <- c(1, m1[!is.na(m1)])
    e1 <- aperm(e1, m1)

    # iter of output
    it <- max(d1[l1], d2[6])

    # iters from FLQ
    if(d2[6] >= d1[l1]) {
      return(new(class(e2), array(callGeneric(array(e1, dim=c(d2[-6], it)),
        e2@.Data), dim=d2, dimnames=dimnames(e2)), units=units(e2)))
    }
    else {
      return(new(class(e2), array(callGeneric(array(e1, dim=c(d2[-6], it)),
        array(e2@.Data, dim=c(c(d2[-6], it)))), dim=c(d2[-6], it),
        dimnames=c(dimnames(e2)[-6], list(iter=seq(it)))), units=units(e2))
      )
    }
  }
) # }}}

# ab {{{
setMethod('ab', signature(x='FLPar', model='character'),
  function(x, model, spr0=NULL)
  {
    # input params and default values
    param <- as(x, 'list')
    args <- list(s=NULL, v=NULL, spr0=spr0, c=NULL, d=NULL)
    args[names(param)] <- param
    args['model'] <- model

    res <- do.call('abPars', args)
    
    # get back c and d
    cd <- args[c('c', 'd', 'spr0')]
    res <- c(res, unlist(cd[!unlist(lapply(cd, is.null))]))

    return(FLPar(res, params=names(res)))
  })

setMethod('ab', signature(x='FLPar', model='formula'),
  function(x, model, spr0=NULL)
  {
    model <- SRModelName(model)
    if(is.null(model))
      stop("model provided has not been identified")
    else
      return(ab(x, model))
  })# }}}

# sv {{{
setMethod('sv', signature(x='FLPar', model='character'),
  function(x, model, spr0)
  {
    # input params and default values
    param <- as(x, 'list')
    args <- list(spr0=spr0, a=NULL, b=NULL, c=NULL, d=NULL)
    args[names(param)] <- param
    args['model'] <- model

    res <- do.call('svPars', args)
    # get back c and d
    cd <- args[c('c', 'd')]
    res <- c(res, unlist(cd[!unlist(lapply(cd, is.null))]))

    return(FLPar(res, params=names(res)))
  })

setMethod('ab', signature(x='FLPar', model='formula'),
  function(x, model, spr0=NULL)
  {
    model <- SRModelName(model)
    if(is.null(model))
      stop("model provided has not been identified")
    else
      return(ab(x, model))
  })# }}}

# sweep {{{
setMethod('sweep', signature(x='FLPar'),
  function(x, MARGIN, STATS, FUN, check.margin=TRUE, ...)
  {
    res <- callNextMethod()
    do.call(class(x), list(res, units=units(x)))
  }
) # }}}

# apply {{{
setMethod('apply', signature(X='FLPar'),
  function(X, MARGIN, FUN, ...)
  {
    res <- callNextMethod()
    do.call(class(X), list(res, units=units(X)))
  }
) # }}}

# jackSummary {{{
setMethod("jackSummary", signature(object="FLPar"),
  function(object, ...) {

   nms <-names(dimnames(object))
   idx <-seq(length(nms))[nms != 'iter']
   n <-dims(object)$iter - 1
   
   mn <-iter(object,  1)
   u <-iter(object, -1)
   mnU <-apply(u, idx, mean)   

   SS <-apply(sweep(u, idx, mnU,"-")^2, idx, sum)

   bias <- (n - 1) * (mnU - mn)
   se <- sqrt(((n-1)/n)*SS)

   return(list(hat=mn, mean=mnU, se=se, bias=bias))
  }
) # }}}

# rbind {{{
setMethod('rbind', signature('FLPar'),
  function(..., deparse.level=1) {
    
    args <- list(...)
    
    idx <- unlist(lapply(args, is, 'FLPar'))
    if(!all(idx))
      stop("input objects must all be of class 'FLPar'")

    res <- args[[1]]@.Data
    if(length(args) > 1)
      for (i in seq(length(args))[-1])
        res <- rbind(res, args[[i]]@.Data)
    
    # dimnames
    names(dimnames(res)) <- names(dimnames(args[[1]]))
    if(any(unlist(lapply(dimnames(res), function(x) any((x==x[1])[-1])))))
      warning("Repeated dimnames in output FLPar")
   
    return(FLPar(res, units=units(args[[1]])))
  }
) # }}}

# cbind {{{
setMethod('cbind', signature('FLPar'),
  function(..., deparse.level=1) {
    
    args <- list(...)
    
    idx <- unlist(lapply(args, is, 'FLPar'))
    if(!all(idx))
      stop("input objects must all be of class 'FLPar'")

    res <- args[[1]]@.Data
    if(length(args) > 1)
      for (i in seq(length(args))[-1])
        res <- cbind(res, args[[i]]@.Data)
    
    # dimnames
    names(dimnames(res)) <- names(dimnames(args[[1]]))
    # correct for iter dimnames
    dimnames(res)$iter <- seq(length(dimnames(res)$iter))

    if(any(unlist(lapply(dimnames(res), function(x) any((x==x[1])[-1])))))
      warning("Repeated dimnames in output FLPar")
   
    return(FLPar(res, units=units(args[[1]])))
  }
) # }}}
