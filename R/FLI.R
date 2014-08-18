# FLI.R - DESC
# FLI.R

# Copyright 2003-2014 FLR Team. Distributed under the GPL 2 or later
# Maintainer: Iago Mosqueira, JRC
# Soundtrack:
# Notes:

## computeCatch (added by EJ)   {{{
setMethod(computeCatch, signature("FLI"), function(object){
	catch <- object@catch.n*object@catch.wt
	catch <- quantSums(catch)
	catch
})  # }}}


# '['       {{{
setMethod('[', signature(x='FLI'),
	function(x, i, j, k, l, m, n, ..., drop=FALSE)
  {
    qnames <- getSlotNamesClass(x, 'FLQuant')
    dims <- unlist(lapply(qapply(x, dim)[qnames], function(x) max(x[1])))
    slot <- names(dims[dims == max(dims)][1])
		dx <- dim(slot(x, slot))
    args <- list(drop=FALSE)

		if (!missing(i))
    {
      args <- c(args, list(i=i))
      x@range['plusgroup'] <- min(i[length(i)], x@range['plusgroup'])
    }
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

    # range
    x@range['min'] <- dims(slot(x, slot))$min
    x@range['max'] <- dims(slot(x, slot))$max
    x@range['minyear'] <- dims(slot(x, slot))$minyear
    x@range['maxyear'] <- dims(slot(x, slot))$maxyear

    return(x)
    }
)   # }}}
