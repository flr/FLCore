# FLCohort - 
# FLCore/R/FLCohort.R

# Copyright 2003-2007 FLR Team. Distributed under the GPL 2 or later
# Maintainer: Ernesto Jardim, IPIMAR 
# $Id$

# Reference:
# Notes:

# Class {{{
setClass("FLCohort",
	representation("FLArray"),
	prototype(array(NA, dim=c(1,1,1,1,1,1),
		dimnames=list(age="0", cohort="0", unit="unique", season="all", area="unique",
		iter="none")), units="NA")
) # }}}

# constructor  {{{
setGeneric("FLCohort", function(object, ...)
	standardGeneric("FLCohort")
)

setMethod("FLCohort", signature("FLQuant"), function(object, ...){
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

})  # }}}

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
setGeneric("flc2flq", function(object, ...){
	standardGeneric("flc2flq")
	}
)

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
setGeneric("ccplot", function(x, data, ...)
	standardGeneric("ccplot")
)
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
if (!isGeneric("iter<-"))
	setGeneric("iter<-", function(object, ..., value)
		standardGeneric("iter<-"))
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
