# FLCohort - 
# FLCore/R/FLCohort.R

# Copyright 2003-2007 FLR Team. Distributed under the GPL 2 or later
# Maintainer: Ernesto Jardim, IPIMAR 
# $Id$

# Reference:
# Notes:

# Class {{{
validFLCohort <-  function(object) {
  # names
  if(!all.equal(names(dimnames(object)), 
      c("age", "cohort", "unit", "season", "area", "iter")))
    return("names of FLCohort object are not correct")

	# Everything is fine
  return(TRUE)
}

setClass("FLCohort",
	representation("FLArray"),
	prototype(array(as.numeric(NA), dim=c(1,1,1,1,1,1),
		dimnames=list(age="1", cohort="1", unit="unique", season="all", area="unique",
		iter="none")), units="NA"),
  validity=validFLCohort
) # }}}

# FLCohort(FLQuant)  {{{
setMethod("FLCohort", signature("FLQuant"),
  function(object, ...) {
  	# reduce with trim
	  if(!missing(...)) object <- trim(object, ...)

  	# dimensions and co
	  dnobj <- dimnames(object)
  	astart <- ifelse(!is.na(dims(object)$min), dims(object)$min,
      stop("FLQuant has no numeric 'age', cannot convert to FLCohort."))
  	ystart <- as.numeric(dnobj$year[1])
	  dobj <- dim(object)	
  	dflc <- dobj
	  dflc[2] <- sum(dobj[1:2])-1

  	# creating array flc
  	flc <- array(NA, dim=dflc)
	  coh.name <- ystart+((-dobj[1]+1):(dobj[2]-1))-astart	
  	dn.lst <- dimnames(object)
	  dn.lst[[2]] <- coh.name
  	names(dn.lst)[2] <- "cohort"
	  dimnames(flc) <- dn.lst

  	# creating the index
	  m <- matrix(flc[,,1,1,1,1], ncol=dflc[2], nrow=dflc[1], dimnames=dimnames(flc)[1:2])
  	lst <- split(1:ncol(object),1:ncol(object))
	  lst <- lapply(lst, function(count){	
		  paste("row(m)==(-col(m)+", dobj[1] + count, ")", sep="")
  	})
	  str <- paste(lst, collapse="|")
  	ind <- eval(parse(text = str))
	  flc.ind <- array(ind, dim=dflc)

  	# feeding the array with terrible hack to feed by "diagonal"
	  flc <- aperm(flc, c(2,1,3,4,5,6))
  	flc.ind <- aperm(flc.ind, c(2,1,3,4,5,6))
	  flq <- aperm(object@.Data, c(2,1,3,4,5,6))
  	flc[flc.ind] <- flq
	  flc <- aperm(flc, c(2,1,3,4,5,6))

  	# et voilá
	  new("FLCohort", flc, units=units(object))
  }
) # }}}

# FLCohort(FLCohort)  {{{
setMethod('FLCohort', signature(object='FLCohort'),
  function(object, units=units(object))
  {
    if(!missing(units))
      units(object) <- units

    return(object)
  }
) # }}}

# FLCohort(array)		{{{
setMethod("FLCohort", signature(object="array"),
	function(object, dim=rep(1,6), dimnames="missing", units="NA",
    iter=1, fill.iter=TRUE) {
		# no dim or dimnames
		if (missing(dim) && missing(dimnames)) {
			# get dim from object and complete
			dim <- c(dim(object), rep(1,5))[1:6]
			# change dim[6] if iter is set
			if(!missing(iter))
				dim[6] <- iter
			# if object has dimnames, use then
			if(!is.null(dimnames(object))) {
				dimnames <- fillFLCdimnames(dimnames(object), dim=dim)
			}
			# otherwise create from dim
			else {
				dimnames <- list(age=1:dim[1], cohort=1:dim[2], unit=1:dim[3],
					season=1:dim[4], area=1:dim[5], iter=1:dim[6])
				dimnames[which(dim==1)] <- list(age='1', cohort=1, unit='unique',
					season='all', area='unique', iter='1')[which(dim==1)]
			}
		}

		# dim missing
		else if (missing(dim)) {
      if(missing(iter) && length(dim(object)) == 6)
        iter <- dim(object)[6]
			dimnames <- fillFLCdimnames(dimnames, dim=c(dim(object), rep(1,6))[1:6], iter=iter)
			# extract dim from dimnames
			dim <- c(dim(object),
				as.numeric(sapply(dimnames, length))[length(dim(object))+1:6])[1:6]
			if(!missing(iter))
				dim[6] <- iter
		}

		# dimnames missing
		else if (missing(dimnames)) {
			dim <- c(dim, rep(1,6))[1:6]
			if(!missing(iter))
				dim[6] <- iter
			# create dimnames from dim
			dimnames <- list(age=1:dim[1], cohort=1:dim[2], unit=1:dim[3],
				season=1:dim[4], area=1:dim[5], iter=1:iter)
			dimnames[which(dim==1)] <- list(age='1', cohort=1, unit='unique', season='all',
				area='unique', iter='1')[which(dim==1)]
		}
    # TODO TEST
		flc <- new("FLCohort", array(as.double(object), dim=dim, dimnames=dimnames),
      units=units)

		# Set extra iters to NA, unless array has 6 dimensions
	    if(dims(flc)$iter > 1 && !fill.iter)
    		flc[,,,,,2:dims(flc)$iter] <- as.numeric(NA)

		return(flc)
	}
)	# }}}

# FLCohort(vector) {{{
setMethod("FLCohort", signature(object="vector"),
	function(object, dim=c(length(object), rep(1,5)), dimnames="missing",
			units="NA", iter=1) 
	{
		if(!missing(dimnames))
		{
			dim <- unlist(lapply(dimnames, length))
			return(FLCohort(array(object, dim=dim, dimnames=dimnames), units=units, iter=iter))
		}
		else
			return(FLCohort(array(object, dim=dim), dimnames=dimnames, units=units, iter=iter))
	}
)	# }}}

# FLCohort(missing)		{{{
setMethod("FLCohort", signature(object="missing"),
	function(object, dim=rep(1,6), dimnames="missing", units="NA", iter=1) {
		
		# no dim or dimnames
		if (missing(dim) && missing(dimnames)) {
			dim <- c(1,1,1,1,1,iter)
			dimnames <- list(age=1, cohort=1, unit='unique', season='all', area='unique',
				iter=1:dim[6])
		}

		# dim missing
		else if (missing(dim)) {
			dimnames <- fillFLCdimnames(dimnames, iter=iter)
			dim <- as.numeric(sapply(dimnames, length))
		}

		# dimnames missing
		else if (missing(dimnames)) {
			dim <- c(dim, rep(1,6))[1:6]
			if(!missing(iter))
				dim[6] <- iter
			dimnames <- list(
				age=1:dim[1],
				cohort=1:dim[2],
				unit=if(dim[3]==1){"unique"}else{1:dim[3]},
				season=if(dim[4]==1){"all"}else{1:dim[4]},
				area=if(dim[5]==1){"unique"}else{1:dim[5]},
				iter=1:dim[6])
		}
		# both
		else {
			dim <- c(dim, rep(1,6))[1:6]
			if(!missing(iter))
				dim[6] <- iter
			dimnames <- fillFLCdimnames(dimnames, dim=dim, iter=iter)
		}
		flc <- new("FLCohort", array(as.numeric(NA), dim=dim, dimnames=dimnames), units=units)

		return(flc)
	}
)	# }}}

# FLCohort methods   {{{
# coerce FLQuant into FLCohort
setAs("FLQuant", "FLCohort",
  function(from)
  {
  return(FLCohort(from))
  }
)

# coerce FLCohort into FLQuant
setAs("FLCohort", "FLQuant", function(from){
	# dimensions and co
	ystart <- as.numeric(dimnames(from)$cohort[1])
	dobj <- dim(from)	
	dflq <- dobj
	dflq[2] <- dobj[2]-dobj[1]+1
	dnflq <- dimnames(from)
	dnflq[[2]] <- as.character(as.numeric(dnflq[[2]][-c(1:dobj[1] - 1)])+as.numeric(dnflq[[1]][1]))
	names(dnflq)[2]<-"year"

	# the new object
	flq <- array(NA, dim=dflq, dimnames=dnflq)
		
	# loop
	for(i in 1:dflq[1]){
		start <- dobj[1]-i+1
		end <- dobj[2]-i+1
		flq[i,,,,,] <- from[i, start:end,,,,]
	}
	
	# et voilá
	new("FLQuant", flq, units=units(from))

})  # }}}

# flc2flq {{{
# this is a FLQuant creator method for FLCohorts 
setMethod("flc2flq", signature("FLCohort"), function(object, ...){

	# reduce with trim
	if(!missing(...))
		object <- trim(object, ...)

	# dimensions and co
	ystart <- as.numeric(dimnames(object)$cohort[1])
	dobj <- dim(object)	
	dflq <- dobj
	dflq[2] <- dobj[2]-dobj[1]+1
	dnflq <- dimnames(object)
	dnflq[[2]] <- as.character(as.numeric(dnflq[[2]][-c(1:dobj[1] - 1)])+as.numeric(dnflq[[1]][1]))
	names(dnflq)[2]<-"year"

	# the new object
	flq <- array(NA, dim=dflq, dimnames=dnflq)
		
	# loop
	for(i in 1:dflq[1]){
		start <- dobj[1]-i+1
		end <- dobj[2]-i+1
		flq[i,,,,,] <- object[i, start:end,,,,]
	}
	
	# et voilá
	new("FLQuant", flq, units=units(object))

})  # }}}

# plot  {{{
setMethod("plot", signature(x="FLCohort", y="missing"),
	function(x, y="missing", ...){
		dots <- list(...)
		condnames <- names(dimnames(x)[c(3:5)][dim(x)[c(3:5)]!=1])
		cond <- paste(condnames, collapse="+")
		if(cond != "") cond <- paste("*", cond)
		formula <- formula(paste("data~age|as.factor(cohort)", cond))
		dots$x <- formula
		dots$data <- x
		dots$ylab <- units(x)
		dots$xlab <- "age"
		dots$type <- c("l")	
		do.call("xyplot", dots)
	}
) # }}}

# bubbles {{{
setMethod("bubbles", signature(x="formula", data ="FLCohort"),
    function(x, data, bub.scale=2.5, ...){
	    dots <- list(...)
    	data <- as.data.frame(data)
	    dots$data <- data
    	dots$cex <- bub.scale*data$data/max(data$data, na.rm=TRUE)+0.1
	    pfun <- function(x, y, ..., cex, subscripts){
    		panel.xyplot(x, y, ..., cex = cex[subscripts])
		}
	    call.list <- c(x = x, dots, panel=pfun)
    	xyplot <- lattice::xyplot
	    ans <- do.call("xyplot", call.list)
    	ans$call <- match.call()
	    ans
    }
) # }}}

# ccplot  {{{
setMethod("ccplot", signature(x="formula", data ="FLCohort"), function(x, data, ...){

    dots <- list(...)
	# define a suitable xlim based on years
	if(all.vars(x)[2]=="year"){
		ys <- dimnames(data)$cohort[dim(data)[1]]
    	ye <- dimnames(data)$cohort[dim(data)[2]]
		xlim <- c(as.numeric(ys), as.numeric(ye)+2) 
	    dots$xlim <- xlim
	}
	# now data coerce
    data <- as.data.frame(data)
	# some options
    data$year <- data$cohort + data$age
    dots$data <- data
    dots$groups <- data$cohort
	# call & run
    call.list <- c(x = x, dots)
    xyplot <- lattice::xyplot
    ans <- do.call("xyplot", call.list)
    ans

})  # }}}

# xyplot  {{{
setMethod("xyplot", signature("formula", "FLCohort"), function(x, data, ...){
	lst <- substitute(list(...))
	lst <- as.list(lst)[-1]
    lst$data <- as.data.frame(data)
	lst$x <- x
	do.call("xyplot", lst)
})  # }}}

## dims       {{{
setMethod("dims", signature(obj="FLCohort"),
	# Return a list with different parameters
	function(obj, ...){
		quant   <-  as.numeric(dim(obj)[names(obj) == quant(obj)])
		min	 <- suppressWarnings(as.numeric(dimnames(obj)[[quant(obj)]][1]))
		max	 <- suppressWarnings(as.numeric(dimnames(obj)[[quant(obj)]][length(dimnames(obj)[[quant(obj)]])]))
		cohort	<-  as.numeric(dim(obj)[names(obj) == "cohort"])
		mincohort <-  suppressWarnings(as.numeric(dimnames(obj)$cohort[1]))
		maxcohort <-  suppressWarnings(as.numeric(dimnames(obj)$cohort[dim(obj)[names(obj) == "cohort"]]))
		unit	<-  dim(obj)[names(obj) == "unit"]
 		season  <-  dim(obj)[names(obj) == "season"]
		area	<-  dim(obj)[names(obj) == "area"]
		iter <- dim(obj)[names(obj) == "iter"]
		list <- list(quant=quant, min=min, max=max, cohort=cohort, mincohort=mincohort,
			maxcohort=maxcohort, unit=unit, season=season, area=area, iter=iter)
		names(list)[1] <- quant(obj)
		return(list)
	}
)   # }}}

# iter<-     {{{
setMethod("iter<-", signature(object="FLCohort", value="FLCohort"),
	function(object, iter, value)
	{
		object[,,,,,iter] <- value
		return(object)
	}
)   # }}}

# propagate {{{
setMethod("propagate", signature(object="FLCohort"),
  function(object, iter, fill.iter=TRUE)
  {
    return(new('FLCohort', array(object@.Data, dimnames=c(dimnames(object)[-6],
      list(iter=1:iter)), dim=c(dim(object)[-6], iter))))
  }
) # }}}

## fillFLCdimnames       {{{
fillFLCdimnames <- function(dnames, dim=rep(1,6), iter=1) {
	# generate standard names for given dimensions
  if(!missing(iter))
    dim[6] <- iter
	xnames <- dimnames(FLCohort(dim=dim))
	for(i in names(dnames))
	  xnames[[i]] <- dnames[[i]]

	return(xnames)
} # }}}

## dimnames<-       {{{
setMethod("dimnames<-", signature(x="FLCohort", value='list'),
	function(x, value) {
		if(any(!names(value) %in% c("age", "cohort", "unit", "season", "area", "iter")))
			stop("names in value do not match those in FLCohort")
		xnames <- dimnames(x)
		for(i in 1:length(value)) {
			if(any(names(value)[i]==c("cohort","unit","season","area","iter")))
				xnames[[names(value)[i]]] <- value[[i]]
			else {
				xnames[[1]] <- value[[i]]
				names(xnames)[1] <- names(value)[i]
			}
		}
		attributes(x)$dimnames <- xnames
		return(x)
	}
) # }}}
