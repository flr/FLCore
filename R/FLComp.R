# FLComp - «Short one line description»
# FLCore/R/FLComp.R

# Copyright 2003-2007 FLR Team. Distributed under the GPL 2 or later
# Maintainer: Iago Mosqueira, Cefas
# $Id$

# FLComp   {{{
validFLComp <- function(object)
{
	# FLQuant slots must have either 1 or n iter
  names <- getSlotNamesClass(object, 'FLArray')
	dims <- vector(length=length(names))
	dimnms <- vector("list", length(names)) 
	dimnms <- list()
	for (i in seq(names))
	{
		dims[i] <- dims(slot(object, names[i]))$iter
		dimnms[[i]] <- dimnames(slot(object, names[i]))$iter
	}
	test <- dims != max(dims) & dims != 1
	if (any(test))
		stop(paste("All slots must have iters equal to 1 or 'n': error in",
			paste(names[test], collapse=', ')))
	# and dimname for iter[1] should be '1'
	test <- unlist(dimnms[dims == 1])
	if(!all(test==test))
		stop(paste("Incorrect names on the iter dimension in ",
			paste(names[test], collapse=', ')))
	return(TRUE)
}

setClass("FLComp", representation(name="character", desc="character",
	range="numeric", "VIRTUAL"), prototype(name=character(0), desc=character(0),
  range	= unlist(list(min=0, max=0, plusgroup=NA, minyear=1, maxyear=1))), 
  validity=validFLComp)

invisible(createFLAccesors('FLComp', include=c('name', 'desc')))
#  }}}

## summary		{{{
setMethod("summary", signature(object="FLComp"),
	function(object, ...){

		cat("An object of class \"", class(object), "\"\n\n", sep="")
		cat("Name:", object@name, "\n")
		cat("Description:", object@desc, "\n")
		cat("Range:\t", paste(sub('plusgroup', 'pgroup', names(object@range)),
      collapse="\t"), "\n")
		cat("", object@range, "\n", sep="\t")

    # character slots 
		cnames <- getSlotNamesClass(object, 'character')
    cnames <- cnames[!cnames%in%c('name', 'desc')]
    if(length(cnames) > 0)
    {
      for (s in cnames)
        cat(paste(toupper(substring(s, 1,1)), substring(s, 2), sep=""), ": ",
          slot(object, s), "\n")
    }

    # FLArray slots
		qnames <- getSlotNamesClass(object, 'FLArray')
		cat("Quant:", quant(slot(object, qnames[1])), "\n\n")
		
		for (s in qnames) {
			#if (sum(!complete.cases(slot(object, s))) == length(slot(object,s)))
			#	cat(substr(paste(s, "          "), start=1, stop=12), " : EMPTY\n") else
				cat(substr(paste(s, "          "), start=1, stop=12), " : [",
					dim(slot(object,s)),"], units = ", slot(object,s)@units, "\n")
		}
	}
)	# }}}

## window    {{{
setMethod("window", signature(x="FLComp"),
	  function(x, start=dims(x)$minyear, end=dims(x)$maxyear, extend=TRUE, frequency=1) {
      x <- qapply(x, window, start=start, end=end, extend=extend, frequency=frequency)
  		x@range["minyear"] <- start
	  	x@range["maxyear"] <- end

		return(x)
	}
)	# }}}

## propagate {{{
setMethod("propagate", signature(object="FLComp"),
	  function(object, iter, fill.iter=TRUE)
      qapply(object, propagate, iter=iter, fill.iter=fill.iter)
) # }}}

## iter {{{
setMethod("iter", signature(object="FLComp"),
	  function(object, iter) {
	  	
		# copy the iterate into the new slots
		names. <- getSlotNamesClass(object, 'FLArray')
		for(s. in names.)
		{
			if(dims(slot(object, s.))$iter == 1)
				slot(object, s.) <- iter(slot(object, s.), 1)
			else
				slot(object, s.) <- iter(slot(object, s.), iter)
		}
		
		return(object)
	  }
) # }}}

## iter<-  {{{
setMethod("iter<-", signature(object="FLComp", value="FLComp"),
	function(object, iter, value)
	{
		object[,,,,,iter] <- value
		return(object)
	}
)   # }}}

## transform	{{{
if (!isGeneric("transform"))
	setGeneric("transform", function(`_data`, ...) standardGeneric("transform"))

setMethod("transform", signature(`_data`="FLComp"),
	function(`_data`, ...)
  {	
    # An environment is created to avoid issues with
		#  methods sharing names with slots - IM 26.08.07
		env <- new.env(parent=parent.frame())
		for (i in slotNames(`_data`))
			assign(i, slot(`_data`, i), env=env)
    args <- eval(substitute(list(...)), env)
		for (i in 1:length(args)) {
			slot(`_data`, names(args)[i]) <- args[[i]]
		}
		if(validObject(`_data`))
			return(`_data`)
		stop('Attempt to modify object incorrectly: check input dimensions')
	}
)	# }}}

## qapply		{{{
if (!isGeneric("qapply"))
	setGeneric("qapply", function(X, FUN, ...) standardGeneric("qapply"))

setMethod('qapply', signature(X='FLComp', FUN='function'),
	function(X, FUN, ..., exclude=missing) {
		FUN <- match.fun(FUN)
		slots <- getSlotNamesClass(X, 'FLArray')
    if(!missing(exclude))
      slots <- slots[!slots%in%exclude]
		if(is(do.call(FUN, list(slot(X,slots[1]), ...)), 'FLArray')) {
			res <- X
			for (i in slots)
				slot(res, i) <- do.call(FUN, list(slot(X,i), ...))
		}
		else {
			res  <- vector('list', 0)
			for (i in slots)
				res[[i]] <- do.call(FUN, list(slot(X,i), ...))
		}
		return(res)
	}
)   # }}}

## trim     {{{
setMethod("trim", signature("FLComp"),
	function(x, ...)
	{
	  args <- list(...)
    names <- getSlotNamesClass(x, 'FLArray')

    c1 <- args[[quant(slot(x, names[1]))]]
	  c2 <- args[["year"]]
    
    # FLQuants with quant
    x <- qapply(x, trim, ...)

    # range
  	if (length(c1) > 0)
    {
    	x@range["min"] <- c1[1]
	    x@range["max"] <- c1[length(c1)]
    	x@range["plusgroup"] <- NA
	  }
  	if (length(c2)>0 )
    {
    	x@range["minyear"] <- as.numeric(c2[1])
	    x@range["maxyear"] <- as.numeric(c2[length(c2)])
  	}
	  return(x)
	}
) # }}}

## units	    {{{
setMethod("units", signature(x="FLComp"), function(x)
	qapply(x, units)
)
#}}}

## units<-      {{{
setMethod("units<-", signature(x="FLComp", value="list"),
    function(x, value) {
        for(i in seq(along=value))
            if(is.character(value[[i]]))
                units(slot(x, names(value[i]))) <- value[[i]]
        return(x)
	}
) # }}}

## '['       {{{
setMethod('[', signature(x='FLComp'),
	function(x, i, j, k, l, m, n, ..., drop=FALSE) {

		qnames <- names(getSlots(class(x))[getSlots(class(x))=="FLQuant"])
		dx <- dim(slot(x, qnames[1]))
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
      slot(x, q) <- do.call('[', c(list(x=slot(x,q)), args))

    # range
    x@range['min'] <- dims(slot(x, qnames[1]))$min
    x@range['max'] <- dims(slot(x, qnames[1]))$max
    x@range['minyear'] <- dims(slot(x, qnames[1]))$minyear
    x@range['maxyear'] <- dims(slot(x, qnames[1]))$maxyear

    return(x)
    }
)   # }}}

## "[<-"            {{{
setMethod("[<-", signature(x="FLComp"),
	function(x, i, j, k, l, m, n, ..., value="missing") {

		qnames <- names(getSlots(class(x))[getSlots(class(x))=="FLQuant"])
		dx <- dim(slot(x, qnames[1]))

		if (missing(i))
			i <- seq(1, dx[1])
		if (missing(j))
			j <- seq(1, dx[2])
   		if (missing(k))
			k <- seq(1, dx[3])
		if (missing(l))
			l <- seq(1, dx[4])
		if (missing(m))
			m <- seq(1, dx[5])
		if (missing(n))
			n <- seq(1, dx[6])

        for(q in qnames)
            slot(x, q)[i,j,k,l,m,n] <- slot(value, q)
	    
   		return(x)
	}
)   # }}}

## as.data.frame        {{{
setMethod("as.data.frame", signature(x="FLComp", row.names="missing", optional="missing"),
	function(x, row.names, optional)
	{
    qnames <- getSlotNamesClass(x, 'FLArray')
    quant <- quant(slot(x, qnames[1]))
	  df   <- data.frame()
    for(s in qnames)
		{
      sdf <- as.data.frame(slot(x, s))
      sdf[[quant]] <- as.character(sdf[[quant]])
      dfq <- cbind(slot=s, sdf)

		  #if(any(class(dfq[,quant(slot(x, s))])=="factor"))
			#	dfq[,quant(slot(x, s))] <- as.numeric(NA)

			df  <- rbind(df, dfq)
	  }
		# add attributes
		attributes(df)$desc <- x@desc
		attributes(df)$name <- x@name
		attributes(df)$range <- x@range

		return(df)
	}
)   # }}}

## mcf	{{{
setGeneric("mcf", function(object, ...)
	standardGeneric("mcf")
)
setMethod('mcf', signature(object='FLComp'),
	function(object, second) {

	qdims <- unlist(qapply(object, function(x) dim(x)[1]))
	qdims <- names(qdims[qdims==max(qdims)][1])

	dimnames <- list()
	dob <- dimnames(slot(object, qdims))
	dse <- dimnames(slot(second, qdims))

	for(i in names(dob))
		dimnames[[i]] <- unique(c(dob[[i]], dse[[i]]))

	foo <- function(x, dimnames) {
		if(all(dimnames(x)[[1]] == 'all'))
			return(FLQuant(x, dimnames=dimnames[-1]))
		else
			return(FLQuant(x))
	}

	res <- new('FLlst')
	res[[1]] <- qapply(object, foo, dimnames=dimnames)
	res[[2]] <- qapply(second, foo, dimnames=dimnames)

	return(res)
	}
)	# }}}

## dims {{{
setMethod("dims", signature(obj="FLComp"),
    # Returns a list with different parameters
    function(obj, ...)
	{
		qnames <- names(getSlots(class(obj))[getSlots(class(obj))=="FLQuant"])
    range <- as.list(range(obj))
		res <- list(
            quant = quant(slot(obj, qnames[1])),
            quants = dim(slot(obj, qnames[2]))[1],
            min = range$min,
            max = range$max,
            year = dim(slot(obj, qnames[1]))[2],
            minyear = range$minyear,
            maxyear = range$maxyear,
            plusgroup = ifelse('plusgroup' %in% names(range), range$plusgroup, NA),
            unit = dim(slot(obj, qnames[1]))[3],
            season = dim(slot(obj, qnames[1]))[4],
            area = dim(slot(obj, qnames[1]))[5],
            iter = max(unlist(qapply(obj, function(x) dims(x)$iter))))
    res <- lapply(res, function(x) if(is.null(x)) return(as.numeric(NA)) else return(x))
    names(res)[2] <- res$quant
    return(res)
    }
)    # }}}

## lattice plots	{{{
# xyplot
setMethod("xyplot", signature("formula", "FLComp"),
	function(x, data, ...){
	lst <- substitute(list(...))
	lst <- as.list(lst)[-1]
    lst$data <- as.data.frame(data)
	lst$x <- x
	do.call("xyplot", lst)
})

# bwplot
setMethod("bwplot", signature("formula", "FLComp"),

	function(x, data, ...){
	lst <- substitute(list(...))
	lst <- as.list(lst)[-1]
    lst$data <- as.data.frame(data)
    lst$data$year <- as.factor(lst$data$year)
	lst$x <- x
	do.call("bwplot", lst)

})

# dotplot
setMethod("dotplot", signature("formula", "FLComp"), function(x, data, ...){

	lst <- substitute(list(...))
	lst <- as.list(lst)[-1]
    lst$data <- as.data.frame(data)
    lst$data$year <- as.factor(lst$data$year)
	lst$x <- x
	do.call("dotplot", lst)

})

# barchart
setMethod("barchart", signature("formula", "FLComp"), function(x, data, ...){

	lst <- substitute(list(...))
	lst <- as.list(lst)[-1]
    lst$data <- as.data.frame(data)
	lst$x <- x
	do.call("barchart", lst)

})

# stripplot
setMethod("stripplot", signature("formula", "FLComp"), function(x, data, ...){

	lst <- substitute(list(...))
	lst <- as.list(lst)[-1]
    lst$data <- as.data.frame(data)
    lst$data$year <- as.factor(lst$data$year)
	lst$x <- x
	do.call("stripplot", lst)

})

# histogram
setMethod("histogram", signature("formula", "FLComp"), function(x, data, ...){

	lst <- substitute(list(...))
	lst <- as.list(lst)[-1]
    lst$data <- as.data.frame(data)
	lst$x <- x
	do.call("histogram", lst)

})  # }}}

# model.frame {{{
if (!isGeneric("model.frame"))
	setGeneric("model.frame", useAsDefault = model.frame)
setMethod('model.frame', signature(formula='FLComp'),
	function(formula, ...)
  {
    lst <- FLQuants()
    names <- getSlotNamesClass(formula, 'FLQuant')
    for(i in names)
      lst[[i]] <- slot(formula, i)
    names(lst) <- names
    return(model.frame(lst))
  }
)
# }}}

# range {{{
setMethod("range", "FLComp",
  function(x, i='missing', ..., na.rm = FALSE)
  {
    if(missing(i))
      slot(x, 'range')
    else
      slot(x, 'range')[i]
  }
) 

if (!isGeneric("range<-"))
	setGeneric("range<-", function(x, i, value) standardGeneric("range<-"))
setReplaceMethod("range", "FLComp",
  function(x, i, value)
  {
    slot(x, 'range')[i] <- value
    return(x)
  }
) # }}}

# expand  {{{
setMethod('expand', signature(x='FLComp'),
  function(x, ...)
  {
    x <- qapply(x, expand, ...)

    # range
    range <- qapply(x, function(x) dimnames(x)[[1]])
    slot <- names(which.max(lapply(range, length)))
    dnames <- dimnames(slot(x, slot))
    range(x, c('min', 'max', 'minyear', 'maxyear')) <- c(as.numeric(dnames[[1]][1]),
      as.numeric(dnames[[1]][length(dnames[[1]])]), as.numeric(dnames[[2]][1]),
      as.numeric(dnames[[2]][length(dnames[[2]])]))

    return(x)
  }
) # }}}
