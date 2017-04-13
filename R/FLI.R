# FLI.R - DESC
# FLI.R

# Copyright 2003-2015 FLR Team. Distributed under the GPL 2 or later
# Maintainer: Iago Mosqueira, EC JRC G03
# Soundtrack:
# Notes:

## computeCatch  {{{
setMethod(computeCatch, signature("FLI"), function(object){
	catch <- object@catch.n*object@catch.wt
	catch <- quantSums(catch)
	catch
})  # }}}

# '['       {{{
#' @rdname Extract
#' @aliases [,FLI,ANY,ANY,ANY-method
setMethod('[', signature(x='FLI', i='ANY', j='ANY', drop='ANY'),
	function(x, i, j, k, l, m, n, ..., drop=FALSE)
  {
		qnames <- names(getSlots(class(x))[getSlots(class(x))=="FLQuant"])
    dims <- unlist(lapply(qapply(x, dim)[qnames], function(x) max(x[1])))
    slot <- names(dims[dims == max(dims)][1])
		dx <- dim(slot(x, slot))
    args <- list(drop=FALSE)

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

    for(q in qnames)
    {
      if(dims[[q]][1] == 1)
        slot(x, q) <- do.call('[', c(list(x=slot(x,q), i=1), args[names(args) != 'i']))
      else
        slot(x, q) <- do.call('[', c(list(x=slot(x,q)), args))
    }

		dmns <- dimnames(slot(x, slot))
		if (!missing(i))
    {
    	x@range['min'] <- as.numeric(dmns[[1]][1])
	    x@range['max'] <- as.numeric(rev(dmns[[1]])[1])
      x@range['plusgroup'] <- min(x@range['min'], x@range['plusgroup'])
    }
		if (!missing(j)) {
			x@range['minyear'] <- as.numeric(dmns[[2]][1])
    	x@range['maxyear'] <- as.numeric(rev(dmns[[2]])[1])
		}
    return(x)
    }
)   # }}}

## dims {{{
setMethod("dims", signature(obj="FLI"),
    # Returns a list with different parameters
    function(obj, ...)
	{
    res <- callNextMethod()
    res[['startf']] <- obj@range[["startf"]]
    res[['endf']] <- obj@range[["endf"]]
    return(res)
    }
)    # }}}

# dim {{{
setMethod("dim", signature(x="FLI"),
  function(x) {
    return(dim(x@sel.pattern))
  }
) # }}}

## trim     {{{
setMethod("trim", signature("FLI"), function(x, ...){

	args <- list(...)
  rng<-range(x)

  names <- getSlotNamesClass(x, 'FLArray')
	quant <- quant(slot(x, names[1]))
  c1 <- args[[quant]]
	c2 <- args[["year"]]

    # FLQuants with quant
    for (name in names)
	  {
		#if(name == 'effort')
		if(all(dimnames(slot(x,name))$age=="all"))
		{
			args <- args[names(args)!= quant]
			slot(x, name) <- do.call('trim', c(list(slot(x, name)), args))
		}
		else
			slot(x, name) <- trim(slot(x,name), ...)
	  }

  	if (length(c1) > 0) {
    	x@range["min"] <- c1[1]
	    x@range["max"] <- c1[length(c1)]
      if (rng["max"] != x@range["max"])
         x@range["plusgroup"] <- NA
	}
  	if (length(c2)>0 ) {
    	x@range["minyear"] <- as.numeric(c2[1])
	    x@range["maxyear"] <- as.numeric(c2[length(c2)])
  	}

	return(x)
}) # }}}

## effort		{{{
setMethod("effort", signature(object="FLI", metier="missing"),
	function(object)
    return(slot(object, "effort")))
setReplaceMethod("effort", signature(object="FLI", value="FLQuant"),
	function(object, value)
  {
		slot(object, "effort") <- value
    return(object)
  })
# }}}
