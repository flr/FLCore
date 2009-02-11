# FLlst-methods.R - FLlst-methods class and methods
# FLCore/R/FLlst-methods.R

# Copyright 2003-2007 FLR Team. Distributed under the GPL 2 or later
# Maintainer: Ernesto Jardim, IPIMAR
# $Id$

# coerce NULL {{{
setAs("NULL", "FLStock", function(from) FLStock())
setAs("NULL", "FLIndex", function(from) FLIndex())
setAs("NULL", "FLBiol", function(from) FLBiol())
setAs("NULL", "FLFleet", function(from) FLFleet())
setAs("NULL", "FLQuant", function(from) FLQuant())
setAs("NULL", "FLCatch", function(from) FLCatch())
# }}}

# replacement {{{

setReplaceMethod("[[", signature(x="FLlst", i="ANY", j="missing", value="ANY"),
	function(x, i, j, value)
	{
		if(isTRUE(x@lock) & (
			(is.character(i) & is.na(match(i, names(x))))
			|
			(is.numeric(i) & length(x)<i)))
				stop("The object is locked. You can not replace non-existent elements.") 
		lst <- as(x, "list")
	
		if(length(lst)==0)
		{
			cls <- is(value)[1]
			lst[[i]] <- value
			lst <- lapply(lst, as, cls)
		} else {
			cls1 <- is(lst[[1]])
			cls2 <- is(value)
			if(!identical(cls1,cls2))
				stop("Object must be of class ",cls1,"\n")
			lst[[i]] <- value
		}
		res <- FLlst(lst)
		class(res) <- class(x)
		return(res)
	}
)
	
setReplaceMethod("$", signature(x="FLlst", name="character", value="ANY"),
	function(x, name, value)
	{
		if(isTRUE(x@lock) & is.na(match(name, names(x))))
			stop("The object is locked. You can not replace non-existent elements.") 
		
		lst <- as(x, "list")
		if(length(lst)==0)
		{
			cls <- is(value)[1]
			lst <- do.call("$<-",list(x=lst, name=name, value=value))
			lst <- lapply(lst, as, cls)
		} else {
			cls1 <- is(lst[[1]])
			cls2 <- is(value)
			if(!identical(cls1,cls2))
				stop("Object must be of class ",cls1,"\n")
			lst <- do.call("$<-",list(x=lst, name=name, value=value))
		}
		res <- FLlst(lst)
		class(res) <- class(x)
		return(res)
})

setReplaceMethod("[", signature(x="FLlst", i="ANY", j="missing", value="ANY"),
	function(x, i, j, value)
	{
		li <- length(i)
		if(isTRUE(x@lock) & (
			(is.character(i) & !identical(sum(i %in% names(x)), li))
			|
			(is.numeric(i) & !identical(sum(i %in% 1:length(x)), li))))
				stop("The object is locked. You can not replace non-existent elements.") 
		cls1 <- is(x[[1]])
		cls2 <- is(value[[1]])

		if(!identical(cls1,cls2))
			stop("Object must be of class ", cls1, "\n")
		x@.Data[i] <- value
		return(x)
	}
)

setMethod("[", signature(x="FLlst", i="ANY", j="missing", drop="ANY"), function(x,i,j,drop){
	lst <- as(x, "list")
	lst <- lst[i]
	new(is(x), lst)
})  # }}}

# lapply  {{{
if (!isGeneric("lapply")) {
	setGeneric("lapply", useAsDefault = lapply)
}

setMethod("lapply", signature(X="FLlst"), function(X,FUN,...){
   lstargs <- list(...)
   lstargs$X <- X@.Data
   names(lstargs$X) <- names(X)
   lstargs$FUN <- FUN
   lst <- do.call("lapply", lstargs)
   # getclass
   cls <- getPlural(lst[[1]])
   if(cls != 'list')
   {
     lst <- new(cls, lst, lock=FALSE, names=attr(X, 'names'), desc=attr(X, 'desc'))
   }
  return(lst)
})  # }}}

# window  {{{
setMethod("window", "FLlst", function(x,...){
	args <- list(...)
	args$FUN <- "window"
	args$X <- x
	do.call("lapply", args)
})  # }}}

# lock/unlock {{{
setGeneric("lock", function(object, ...){
	standardGeneric("lock")
})

setMethod("lock", "FLlst", function(object){object@lock <- TRUE; object})

setGeneric("unlock", function(object, ...){
	standardGeneric("unlock")
})

setMethod("unlock", "FLlst", function(object){object@lock <- FALSE; object})  # }}}

## model.frame	{{{
setMethod('model.frame', signature(formula='FLlst'),
	function(formula, ...)
	{
		# initial FLQuant
		res <- as.data.frame(formula[[1]])
    ncol <- ncol(res)
		if(length(formula) > 1)
			for(i in seq(length(formula)-1)+1)
				res <- cbind(res, as.data.frame(formula[[i]])[ncol])

		# get names right
		names(res)[(ncol(res)-length(formula)+1):ncol(res)] <- unlist(names(formula))

		return(res)
	}
)	# }}}

## dims(FLFleets) {{{
setMethod("dims", signature(obj="FLFleets"),
  # Returns a list with different parameters
  function(obj, ...)
	{
		return(list(
      fleets=names(obj),
      metiers=unique(unlist(lapply(obj, function(x) names(x@metiers)))),
      catches=unique(unlist(lapply(obj, function(x) lapply(x@metiers, function(x) names(x@catches))))),
      quant = unlist(lapply(obj, function(x) quant(landings.n(x, 1, 1)))),
      min=min(unlist(lapply(obj, function(obj) min(as.numeric(unlist(lapply(obj@metiers, function(x) lapply(x@catches, function(x) dimnames(x@landings.n)[[1]][1])))))))),
      max=max(unlist(lapply(obj, function(obj) as.numeric(unlist(lapply(obj@metiers, function(x) lapply(x@catches, function(x) dimnames(x@landings.n)[[1]][dim(x@landings.n)[1]]))))))),
      minyear=min(unlist(lapply(obj, function(obj) as.numeric(unlist(lapply(obj@metiers, function(x) lapply(x@catches, function(x) dimnames(x@landings.n)[[2]][1]))))))),
      maxyear=max(unlist(lapply(obj, function(obj) as.numeric(unlist(lapply(obj@metiers, function(x) lapply(x@catches, function(x) dimnames(x@landings.n)[[2]][dim(x@landings.n)[2]]))))))),
      unit=unlist(lapply(obj, function(obj) unique(unlist(lapply(obj@metiers, function(x) lapply(x@catches, function(x) length(dimnames(x@landings.n)[[3]]))))))),
      season=unlist(lapply(obj, function(obj) unique(unlist(lapply(obj@metiers, function(x) lapply(x@catches, function(x) length(dimnames(x@landings.n)[[4]]))))))),
      area=unlist(lapply(obj, function(obj) unique(unlist(lapply(obj@metiers, function(x) lapply(x@catches, function(x) length(dimnames(x@landings.n)[[5]]))))))),
      iter=unlist(lapply(obj, function(x) max(unlist(lapply(x@metiers, function(x) lapply(x@catches, function(x) qapply(x, function(x) length(dimnames(x)[[6]]))))))))
    ))
    }
)    # }}}

# plot(FLIndices)  {{{
setMethod("plot", signature(x="FLIndices",y="missing"),
  function(x,show.scales=FALSE,log.scales=TRUE,...)
  {
    # Generate combinations of surveys to compare
		surv.comb	<-combn(1:length(x),2,simplify=FALSE)
		p.list <- lapply(surv.comb,function(survs)
    {
      # Extract surveys to compare
			surv.x <- x[[survs[1]]]
      surv.y <- x[[survs[2]]]
      # Convert to log scales if necessary
      if(log.scales)
        surv.x@index <- log10(surv.x@index);surv.y@index <- log10(surv.y@index)
			
      # Match up survey data to compare
			comb.surv <- merge(as.data.frame(surv.x@index),as.data.frame(surv.y@index),
        by=c("age","year","unit","season","area","iter"),all=FALSE)

      # Filter NAs, negative values
      comb.surv <- comb.surv[is.finite(comb.surv$data.x) & is.finite(comb.surv$data.y),]
      comb.surv <- comb.surv[comb.surv$data.x >0 & comb.surv$data.y>0,]

      # Plot comparison figure
			p <- xyplot((data.y) ~ (data.x) | paste("Age",age), data=comb.surv,
        as.table=TRUE, sub=list(label="Dotted lines are 95% confidence interval
        for the mean.",cex=0.7), scales = list(relation = "free",draw=show.scales),
        xlab=if(log.scales) {substitute(expression(paste(Log[10],group("(",survxname,
          ")"))),list(survxname=surv.x@name))} else { surv.x@name },
        ylab=if(log.scales) {substitute(expression(paste(Log[10],group("(",survyname,
          ")"))),list(survyname=surv.y@name))} else { surv.y@name },
        panel=function(x,y,...)
        {
          # Plot data points
          panel.xyplot(x,y,...)
					
          # Fit linear model and plot, along with confidence limits
					# but only if there are sufficient data points
					g <- lm(y~x)
          x.rng <- data.frame(x=seq(min(pretty(x)),max(pretty(x)),length.out=10))
          ci  <-  data.frame(x.rng,predict(g,x.rng,interval="confidence"))
          panel.xyplot(ci$x,ci$fit,lwd=2,type="l")
          panel.xyplot(ci$x,ci$upr,lwd=1,lty=2,type="l")
          panel.xyplot(ci$x,ci$lwr,lwd=1,lty=2,type="l")

          # Put Rsq on plot in top left corner
          rsq <- sprintf("%4.3f",round(summary(g)$r.squared,3))
					grid::grid.text(label = bquote(r^2 == .(rsq)),x = unit(1, "npc") - unit(0.25,
            "lines"), y = unit(1,"lines"),just="right",gp=gpar(cex=0.8))
			  }, ...)
      })  #End lapply

			#Print figures, return
			lapply(p.list,print)
			return(p.list)
  }
) # }}}

# TODO  {{{
setMethod('plot', signature(x='FLStocks', y='FLPar'),
  function(x, y, ylab="", xlab="", ...)
  {
    # check y is as present in FLBRP
    y <- y[1,c('harvest', 'yield', 'rec', 'ssb'), 'msy']

    # basic plot
    plot(x, ylab=ylab, xlab=xlab, ...)

  }
) # }}}

# summary {{{
setMethod('summary', signature(object='FLlst'),
  function(object)
  {
	  cat("An object of class \"", class(object), "\"\n\n", sep="")
		cat("Elements:", names(object), "\n")
    cat("\n")
    for(i in seq(1, length(object)))
    {
		  qnames <- getSlotNamesClass(object[[i]], 'FLArray')
      cat("Name:", name(object[[i]]), "\n")
  		cat("\tDescription:", desc(object[[i]]), "\n")
	  	cat("\tRange:\t", paste(sub('plusgroup', 'pgroup', names(range(object[[i]]))),
        collapse="\t"), "\n")
  		cat("\t", range(object[[i]]), "\n", sep="\t")
	  	cat("\tQuant:", quant(slot(object[[i]], qnames[1])), "\n")
	  	cat("\tdim:", dim(slot(object[[i]], qnames[1])), "\n")
   }
  }
) # }}}

# plot(FLStocks)  {{{
setMethod('plot', signature(x='FLStocks', y='missing'),
  function(x, ...)
  {
  # data.frame with selected slots per stock
  dfs <- lapply(x, function(y) data.frame(as.data.frame(FLQuants(catch=catch(y),
    ssb=ssb(y), rec=rec(y), harvest=if(units(harvest(y)) == 'f'){fbar(y)} else {
    quantSums(harvest(y))})), name=name(y)))

  # stock index
  dfs[[1]] <- cbind(dfs[[1]], stock=1)

  # rbind if more than one stock
  if(length(dfs) > 1)
    for(i in seq(2, length(dfs)))
      dfs[[1]] <- rbind(dfs[[1]], cbind(dfs[[i]], stock=i))
  dfs <- dfs[[1]]

  # default options
  options <- list(scales=list(relation='free'), ylab="", xlab="", main=paste(names(x),
    collapse=" - "), col=rainbow(length(x)), lwd=2, cex=0.6)
  args <- list(...)
  options[names(args)] <- args

  if(length(levels(dfs$iter)) == 1)
    do.call(xyplot, c(options, list(x=data~year|qname, data=dfs, groups=expression(stock),
      panel=function(x, y, groups, subscripts, ...)
      {
        panel.xyplot(x, y, type='l', groups=groups, subscripts=subscripts, ...)
        idx <- x==max(x)
        panel.xyplot(x[idx], y[idx], type='p', groups=groups,
          subscripts=subscripts[idx], ...)
      }, key=list(text=list(lab=names(x)), lines=list(col=options$col)))))
  else
  {
  do.call(xyplot, c(options, list(x=data~year|qname, data=dfs, groups=expression(stock),
      panel=panel.superpose, panel.groups=function(x, y, group.number, ...)
      {
        # median
        do.call(panel.xyplot, c(list(unique(x), tapply(y, list(x), median, na.rm=TRUE),
          col=options$col[group.number]), type='l', lwd=2))
        # lowq
        do.call(panel.xyplot, c(list(unique(x), tapply(y, list(x), quantile, 0.05,
          na.rm=TRUE), col=options$col[group.number]), type='l', lty=2, lwd=1, alpha=0.5))
        # uppq
        do.call(panel.xyplot, c(list(unique(x), tapply(y, list(x), quantile, 0.95,
          na.rm=TRUE), col=options$col[group.number]), type='l', lty=2, lwd=1, alpha=0.5))
      }, key=list(text=list(lab=names(x)), lines=list(col=options$col)))))

  }
     }
) # }}}

# range {{{
setMethod("range", "FLlst",
  function(x, i='missing', ..., na.rm = FALSE)
  {
    range <- matrix(unlist(lapply(x, function(x) range(x))), nrow=length(x), byrow=TRUE,
      dimnames=list(1:length(x), names(range(x[[1]]))))
    return(unlist(list(min=min(range[,'min']), max=min(range[,'max']),
      minyear=min(range[,'minyear']), maxyear=min(range[,'maxyear']))))
  }
) # }}}

## names         {{{
setMethod("names", signature(x="FLlst"),
	function(x)
    attr(x, 'names')
)
# }}}

## as.data.frame	{{{
setMethod("as.data.frame", signature(x="FLCohorts", row.names="missing", optional="missing"),
    function(x) {
	# names 
	if(!is.null(names(x))){
		flqnames <- names(x)
	} else {
		flqnames <- paste("v", 1:length(x), sep="")
	}

	# data.frames
	flqs.lst <- lapply(x, as.data.frame)
	flqs.lst <- lapply(flqs.lst, function(x){x[,1] <- as.factor(x[,1]); x})
	flqs.nlst <- lapply(flqs.lst, nrow)
	flqs.df <- do.call("rbind", flqs.lst)
	flqs.df$cname <- rep(flqnames, unlist(flqs.nlst))
	flqs.df

})  # }}}
