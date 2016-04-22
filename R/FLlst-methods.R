# FLlst-methods.R - FLlst classes' methods
# FLCore/R/FLlst-methods.R

# Copyright 2003-2015 FLR Team. Distributed under the GPL 2 or later
# Maintainer: Iago Mosqueira, EC JRC G03

# coerce NULL {{{
setAs("NULL", "FLStock", function(from) FLStock())
setAs("NULL", "FLIndex", function(from) FLIndex())
setAs("NULL", "FLBiol", function(from) FLBiol())
setAs("NULL", "FLQuant", function(from) FLQuant())
# }}}

# replacement, [[<-, $, [<- {{{
setReplaceMethod("[[", signature(x="FLlst", i="ANY", j="missing", value="ANY"),
	function(x, i, j, value)
	{
		if(isTRUE(x@lock) & (
			(is.character(i) & is.na(match(i, names(x))))
			|
			(is.numeric(i) & length(x)<i)))
				stop("The object is locked. You can not replace non-existent elements.")
    
    lst <- as(x, "list")
    names(lst) <- names(x)

		lst[[i]] <- value

		res <- FLlst(lst)
		class(res) <- class(x)

		if(validObject(res))
			return(res)
		else
			stop("Invalid object, classes do not match.")
	}
)

setReplaceMethod("$", signature(x="FLlst", value="ANY"),
	function(x, name, value)
	{
		if(isTRUE(x@lock) & is.na(match(name, names(x))))
			stop("The object is locked. You can not replace non-existent elements.")

		lst <- as(x, "list")
    names(lst) <- names(x)

#		if(length(lst)==0)
#		{
#			cls <- is(value)[1]
#			lst <- do.call("$<-",list(x=lst, name=name, value=value))
#			lst <- lapply(lst, as, cls)
#		}

		lst <- do.call("$<-",list(x=lst, name=name, value=value))

		res <- FLlst(lst)
		class(res) <- class(x)

		if(validObject(res))
			return(res)
		else
			stop("Invalid object, classes do not match.")
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

    nms <- names(x)
    idx <- seq(length(x))
    # HACK, need to check why names get dropped in here
		x@.Data[i] <- value
    names(x)[idx] <- nms

		if(validObject(x))
			return(x)
		else
			stop("Invalid object, classes do not match.")
	}
) # }}}

# subset, [ {{{


setMethod("[", signature(x="FLlst", i="ANY", j="missing", drop="ANY"), function(x,i,j,drop){
	lst <- as(x, "list")
  # names dropped!
  names(lst) <- names(x)
	lst <- lst[i]
	new(is(x), lst)
})  # }}}

# lapply  {{{
setMethod("lapply", signature(X="FLlst"),
  function(X, FUN, ...) {

    if(length(X) == 0)
	  return(new(class(X)))
		
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
	 	names(lst) <- names(X)
 return(lst)
	}
)  # }}}

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

# model.frame	{{{
setMethod('model.frame', signature(formula='FLlst'),
	function(formula, drop=FALSE, ...)
	{
		# initial FLQuant
		res <- as.data.frame(formula[[1]])
    ncol <- ncol(res)
		if(length(formula) > 1)
			for(i in seq(length(formula)-1)+1)
				res <- cbind(res, as.data.frame(formula[[i]])[ncol])

		# get names right
		names(res)[(ncol(res)-length(formula)+1):ncol(res)] <- unlist(names(formula))

    # drop
    if(drop) {
      idx <- apply(matrix(unlist(lapply(formula, dim)), nrow=6), 1, max) == 1
      idx <- c(idx, rep(FALSE, length(formula)))
      res <- res[, !idx]
    }

		return(res)
	}
)	# }}}

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

# plot(FLStocks, FLPar)  {{{
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
      qdims <- dims(object[[i]])

      cat("Name:", name(object[[i]]), "\n")
  		cat("\tDescription:", desc(object[[i]]), "\n")
	  	cat("\tRange:\t", paste(sub('plusgroup', 'pgroup', names(range(object[[i]]))),
        collapse="\t"), "\n")
  		cat("\t", range(object[[i]]), "\n", sep="\t")
	  	cat("\tQuant:", qdims$quant, "\n")
	  	cat("\tdim:", unlist(qdims[c(qdims$quant, 'year', 'unit', 'season', 'area')]
          , use.names=FALSE), "\n")
   }
  }
) # }}}

# plot(FLStocks) {{{
setMethod('plot', signature(x='FLStocks', y='missing'),
	function(x, key=list(lines=TRUE, points=FALSE), ...)
	{
		foo <- function(x) {
	  	if(units(harvest(x)) == 'f')
				har <- fbar(x)
			else
				har <- quantSums(harvest(x))

	  	return(as.data.frame(FLQuants(catch=catch(x), ssb=ssb(x),
				rec=rec(x),harvest=har)))
		}

	dfs <- lapply(x, foo)

  # element names
  names <- names(x)
	names[names == ""]  <- seq(length(dfs))[names == ""]

	for(i in seq(length(dfs)))
		dfs[[i]] <- cbind(dfs[[i]], stock=names[i])

	dfs <- Reduce(rbind, dfs)

  # default options
  options <- list(scales=list(y=list(relation='free')), ylab="",
		xlab="", par.settings=list(superpose.line=list(col=rainbow(length(x)), lwd=2),
		superpose.symbol=list(col=rainbow(length(x)), pch=19, cex=0.6),
		strip.background=list(col="gray85")), cex=0.6)

  args <- list(...)
  options[names(args)] <- args

  # key
  if(key == TRUE)
    options$auto.key <- list(points=FALSE, lines=TRUE, space="right")
  else if (!missing(key) && is(key, 'list'))
    options$key <- key

  if(length(levels(dfs$iter)) == 1)
    do.call(xyplot, c(options, list(x=data~year|qname, data=dfs, groups=expression(stock),
      panel=function(x, y, groups, subscripts, ...)
      {
        panel.xyplot(x, y, type=c('g','l'), groups=groups, subscripts=subscripts, ...)
        idx <- x==max(x)
        panel.xyplot(x[idx], y[idx], type='p', groups=groups,
          subscripts=subscripts[idx], ...)
      })))
  else
  {
  do.call(xyplot, c(options, list(x=data~year|qname, data=dfs, groups=expression(stock),
      panel=panel.superpose, panel.groups=function(x, y, group.number, ...)
      {
        # median
        do.call(panel.xyplot, c(list(unique(x), tapply(y, list(x), median, na.rm=TRUE),
          col=options$col[group.number]), type=c('g','l'), lwd=2))
        # lowq
        do.call(panel.xyplot, c(list(unique(x), tapply(y, list(x), quantile, 0.05,
          na.rm=TRUE), col=options$col[group.number]), type='l', lty=2, lwd=1, alpha=0.5))
        # uppq
        do.call(panel.xyplot, c(list(unique(x), tapply(y, list(x), quantile, 0.95,
          na.rm=TRUE), col=options$col[group.number]), type='l', lty=2, lwd=1, alpha=0.5))
      })))

  }
     }
) # }}}

# range {{{
setMethod("range", "FLlst",
  function(x, i='missing', ..., na.rm = FALSE)
  {
    range <- matrix(unlist(lapply(x, function(x) range(x))), nrow=length(x), byrow=TRUE,
      dimnames=list(1:length(x), names(range(x[[1]]))))
    return(unlist(list(min=min(range[,'min']), max=max(range[,'max']),
      minyear=min(range[,'minyear']), maxyear=min(range[,'maxyear']))))
  }
) # }}}

# names         {{{
setMethod("names", signature(x="FLlst"),
	function(x)
    attr(x, 'names')
)
# }}}

# as.data.frame	{{{
setMethod("as.data.frame", signature(x="FLCohorts", row.names="missing",
	optional="missing"),
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

})

setMethod("as.data.frame", signature(x="FLComps", row.names="missing",
	optional="missing"),
	function(x) {

		nms <- as.list(names(x))
		names(nms) <- names(x)

		res <- lapply(nms, function(y) cbind(as.data.frame(x[[y]]),
			data.frame(cname=nms[[y]])))

		names(res) <- NULL
		res <- do.call(rbind, c(list(deparse.level=0), res))

		return(res)

	}
) # }}}

# collapse {{{
setGeneric("collapse", function(x, ...) standardGeneric("collapse"))
setMethod("collapse", signature(x='FLlst'),
  function(x) {

    # iters per object
	  its <- unlist(lapply(x, function(x) dims(x)$iter))
    # propagate first iter
  	res <- propagate(x[[1]][,,,,,1], sum(its))
	  res[,,,,,seq(1, its[1])] <- x[[1]]

  	if(length(its) > 1) {
	  	for(i in seq(2, length(its))) {
			  pre <- sum(its[1:i-1])
  			idx <- seq(pre + 1, pre + its[i])
	  		res[,,,,,idx]  <- x[[i]]
		  }
	  }

	  return(res)
  }
) # }}}
