# FLQuants - A list of FLQuant objects
# FLCore/R/FLQuants.R

# Copyright 2003-2012 FLR Team. Distributed under the GPL 2 or later
# Maintainer: Iago Mosqueira, JRC
# $Id: FLQuants-methods.R 933 2011-05-03 12:32:03Z imosqueira $


## FLQuants     {{{
#
#setClass("FLQuants", contains="list", representation(
#	names="character"))
# }}}

## FLQuants()   {{{
#
#FLQuants <- function(...) {
#	args <- list(...)
#	if(length(args)==1 & is.list(args[[1]])) lst <- args[[1]]
#	if(length(args)>1) lst <- args
#	lst0 <- lapply(lst, is.FLQuant)
#	if(!identical(length(lst0),sum(unlist(lst0)))) stop("All elements must be \"FLQuant\" objects.\n")
#	new("FLQuants", lst)
#} # }}}

# summary {{{
setMethod('summary', signature(object='FLQuants'),
  function(object)
  {
	  cat("An object of class \"FLQuants\"\n\n", sep="")
		cat("Elements:", names(object), "\n")
    cat("\n")
    for(i in seq(length(object)))
    {
      cat("Name:", names(object)[i], "\n")
      cat("\tdim  : ", dim(object[[i]]), "\n")
  		cat("\tquant: ", quant(object[[i]]), "\n")
	  	cat("\tunits: ", units(object[[i]]), "\n\n")
		  if(all(is.na(object[[i]])))
  		{
	  		cat("\tMin    :  NA\n")
		  	cat("\t1st Qu.:  NA\n")
  			cat("\tMean   :  NA\n")
	  		cat("\tMedian :  NA\n")
		  	cat("\t3rd Qu.:  NA\n")
			  cat("\tMax    :  NA\n")
  		}
	  	else
		  {
  			cat("\tMin    : ", min(object[[i]], na.rm=TRUE), "\n")
	  		cat("\t1st Qu.: ", quantile(as.vector(object[[i]]), 0.25, na.rm=TRUE), "\n")
		  	cat("\tMean   : ", mean(as.vector(object[[i]]), na.rm=TRUE), "\n")
			  cat("\tMedian : ", median(as.vector(object[[i]]), na.rm=TRUE), "\n")
  			cat("\t3rd Qu.: ", quantile(as.vector(object[[i]]), 0.75, na.rm=TRUE), "\n")
	  		cat("\tMax    : ", max(object[[i]], na.rm=TRUE), "\n")
		  }
  		cat("\tNAs    : ", format(length(as.vector(object[[i]])
	  		[!complete.cases(as.vector(object[[i]]))])/length(as.vector(object[[i]]))*100,
		  	digits=2), "%\n")
    }
  }
) # }}}

# xyplot {{{
setMethod("xyplot", signature("formula", "FLQuants"), function(x, data, ...)
	{
	lst <- substitute(list(...))
	lst <- as.list(lst)[-1]
    	lst$data <- as.data.frame(data)
	lst$x <- x
	do.call("xyplot", lst)
}) # }}}

# histogram {{{
setMethod("histogram", signature("formula", "FLQuants"), function(x, data, ...)
	{
	lst <- substitute(list(...))
	lst <- as.list(lst)[-1]
    	lst$data <- as.data.frame(data)
	lst$x <- x
	do.call("histogram", lst)
}) # }}}

## iter {{{
setMethod("iter", signature(obj="FLQuants"),
	  function(obj, iter) {
	  	
		# simply use lapply and iter from FLQuant methods

		flqs <- FLQuants(lapply(obj,function(x,iter){x <- iter(x,iter)},iter))
		return(flqs)
	  }
) # }}}

## is.FLQuants       {{{
#is.FLQuants  <-  function(x)
#	return(is(x, "FLQuants"))
# }}}

## "["             {{{
#setMethod("[", signature(x="FLQuants"),
#	function(x, i="missing", j="missing",..., drop="missing") {
#
#		if (missing(i))
#			i  <-  seq(1, length(x))
#       res <- new('FLQuants', x@.Data[i])
#        names(res) <- names(x)[i]
#   		return(res)
#	}
#)   # }}}

# show  {{{
setMethod('show', signature('FLQuants'),
        function(object) {
			if(length(object) == 0)
				cat('An object of class "FLQuants": EMPTY\n')
			else
                for (n in seq(1:length(object))) {
                        cat(paste('$', names(object)[n], '\n'))
                        show(object[[n]])
                        cat('\n')
                }
        }
)   # }}}

# mcf: make compatible flquants     {{{
setMethod("mcf", signature(object="list"), function(object){
	# names 
	if(!is.null(names(object))){
		flqnames <- names(object)
	} else {
		flqnames <- paste("v", 1:length(object), sep="")
	}
	# how many flquants exist ?
	v <- unlist(lapply(object, is, 'FLArray'))
	nflq <- sum(v)
	lst0 <- object[v]
	# names and dim of the compatible flq
	dn <- dimnames(lst0[[1]])
	for(i in seq(2, length=nflq-1)){
		# using the first iteraction only
		dn1 <- dimnames(lst0[[i]])
		# checking how to merge quant dim
		quant.vec <- unique(c(dn[[1]],dn1[[1]]))
		if(NA %in% (suppressWarnings(as.numeric(quant.vec))))
			dn[[1]] <- sort(quant.vec)
		else 
			dn[[1]] <- as.character(sort(as.numeric(quant.vec)))

		dn[[2]] <- as.character(sort(as.numeric(unique(c(dn[[2]],dn1[[2]])))))
		dn[[3]] <- unique(c(dn[[3]],dn1[[3]]))
		dn[[4]] <- unique(c(dn[[4]],dn1[[4]]))
		dn[[5]] <- unique(c(dn[[5]],dn1[[5]]))
		dn[[6]] <- unique(c(dn[[6]],dn1[[6]]))
	}
	dflq <- unlist(lapply(dn, length))	
	# new flquant
	flq <- FLQuant(dim=dflq, dimnames=dn)
	# preparing the list
	lst <- list()
	length(lst) <- nflq
	lst <- object

	# filling up the quants	
	for(j in 1:length(lst)){
		dn2 <- dimnames(lst[[j]])
		flq0 <- flq
		flq0[dn2[[1]], dn2[[2]], dn2[[3]], dn2[[4]], dn2[[5]], dn2[[6]]] <- lst[[j]]
		lst[[j]] <- flq0
	}
	names(lst) <- flqnames	
	
	# output
	FLQuants(lst)	
})  # }}}

# as.data.frame	{{{
setMethod("as.data.frame", signature(x="FLQuants", row.names="ANY", optional="missing"),
  function(x, row.names, drop=FALSE)
	{
		# names
		if(is.null(names(x)))
			flqnames <- paste("v", 1:length(x), sep="")
		else if(any(is.na(names(x))))
		{
			names(x)[is.na(names(x))] <- paste("v", 1:length(x), sep="")[is.na(names(x))]
			flqnames <- names(x)
		}
		else
			flqnames <- names(x)

		# data.frames
		flqs.lst <- lapply(x, as.data.frame, row.names=row.names, drop=drop)

  	# test classes of quant
  	flqs.class <- unlist(lapply(flqs.lst, function(x) class(x[,1])))
  	if(any(flqs.class != flqs.class[1]))
    	flqs.lst <- lapply(flqs.lst, function(x) {x[,1] <- as.factor(x[,1]); x})

		flqs.nlst <- lapply(flqs.lst, nrow)
		flqs.df <- do.call("rbind", flqs.lst)
		flqs.df$qname <- rep(flqnames, unlist(flqs.nlst))
  	row.names(flqs.df) <- row.names
		flqs.df
}) 

setMethod("as.data.frame", signature(x="FLQuants", row.names="missing",
  optional="missing"),
    function(x, ...) {
      as.data.frame(x, row.names=NULL, ...)
    }
)

# }}}

# bkey   {{{
setGeneric("bkey", function(object, ...){
	standardGeneric("bkey")
	}
)

setMethod("bkey", signature("list"), function(object, ...){

	# test
	if(sum(names(object) %in% c("data", "cex", "bub.col", "bub.scale"))!=4){
		stop("The list passed to bkey must have components \"data\", \"cex\", \"bub.col\" and \"bub.scale\"","\n")
	}

	# get info
	dots <- list(...)
	data <- object$data
	adata <- abs(data)
	cex <- object$cex
	bub.col <- object$bub.col	
	bub.scale <- object$bub.scale
	
	# vectors with cex, col, pch and text
	v <- ceiling(max(adata, na.rm=T))
	ktext <- format(round(seq(-v,v,l=9),2))
	v <- ceiling(max(cex, na.rm=T))
	kcex <- abs(seq(-v,v,l=9))+bub.scale*0.1
	kcol <- rep(bub.col,c(4,5))
	kpch <- rep(c(1,19),c(4,5))
	
	# the key
	akey <- simpleKey(text=ktext[9:1], points=T, lines=F, columns=1, space="right")
	akey$points$col=kcol[9:1]
	akey$points$pch=kpch[9:1]
	akey$points$cex=kcex
	akey$text$cex=0.8

	# merging with other arguments	
	lst0 <- dots[names(dots) %in% names(akey)]
	lst1 <- dots[!(names(dots) %in% names(akey))]
	akey[match(names(lst0),names(akey),nomatch=0)] <- lst0
	akey <- c(akey,lst1)

	# output	
	akey
})  # }}}

# bubbles   {{{
setMethod("bubbles", signature(x="formula", data ="FLQuants"), function(x, data, bub.scale=2.5, bub.col=gray(c(0.1, 0.1)), ...){
	# data
	dots <- list(...)
	dots$data <- as.data.frame(data)

	# def col & pch to plot negative values
	col <- as.numeric(dots$data$data>=0)
	coln <- vector(mode="character", length=length(col))
	pchn <- vector(mode="integer", length=length(col))

	# for negs
	coln[col==0] <- bub.col[1]
	pchn[col==0] <- 1

	# for pos
	coln[col==1] <- bub.col[2]
	pchn[col==1] <- 19

	# for NA
	coln[coln==""] <- NA
	pchn[coln==""] <- NA
	
	# rescale cex to make it prety (I hope)
	cex0 <- abs(dots$data$data)
	cex <- bub.scale*cex0/max(cex0, na.rm=TRUE)

	# panel
	pfun <- function(x,y,subscripts,...){
		panel.xyplot(x,y,col=coln[subscripts], pch=pchn[subscripts], cex=cex[subscripts]+bub.scale*0.1)
	}

	# legend

	akey <- bkey(list(data=dots$data$data, cex=cex, bub.col=bub.col, bub.scale=bub.scale), border=F, title="Scale", cex.title=0.8, padding.text=4)
	
	# call list
	dots$cex <- cex
	dots$pch <- pchn
	dots$col <- coln
	dots$panel <- pfun
	dots$key <- akey
	call.list <- c(x = x, dots)

	# plot
	ans <- do.call("xyplot", call.list)
	ans

})  # }}}

# as(FLComp, FLQuants)  {{{
setAs('FLComp', 'FLQuants',
	function(from)
  {
    res <- FLQuants()
		datanm <- names(getSlots(class(from))[getSlots(class(from))=='FLQuant'])
    for (i in datanm)
      res[[i]] <- slot(from, i)
    return(res)
  }
) # }}}

# combine {{{
setMethod('combine', signature(x='FLQuants', y='missing'),
  function(x) {

    ln <- length(x)
    dm <- matrix(unlist(lapply(x, dim)), ncol=6, nrow=ln, byrow=TRUE)

    # dim(...)[1:5] == dim(...)[1:5]
    if(any(apply(dm[,1:5], 1, function(x) x/dm[1,1:5]) != 1))
      stop("Object dimensions [1:5] must match")

    its <- dm[,6]

    res <- FLQuant(unlist(x), dimnames=c(dimnames(x[[1]])[1:5], list(iter=seq(sum(its)))),
      units=units(x[[1]]))

    return(res)
  }
) # }}}

# Sums(FLQuants)	{{{
setMethod('Sums', signature(object='FLQuants'),
	function(object, na.rm=FALSE, ...) {
		if(length(object) == 1)
			return(object)
		eval(parse(text=paste('object[[', paste(seq(length(object)),
			collapse=']] + object[['), ']]', sep='')))
	}
)
setMethod('Products', signature(object='FLQuants'),
	function(object, na.rm=FALSE, ...)
		eval(parse(text=paste('object[[', paste(seq(length(object)),
			collapse=']] * object[['), ']]', sep='')))
)	# }}}
