# FLlst-methods.R - FLlst classes' methods
# FLCore/R/FLlst-methods.R

# Copyright 2003-2018 FLR Team. Distributed under the GPL 2 or later
# Maintainer: Iago Mosqueira, EC JRC

# coerce NULL {{{
setAs("NULL", "FLStock", function(from) FLStock())
setAs("NULL", "FLIndex", function(from) FLIndex())
setAs("NULL", "FLBiol", function(from) FLBiol())
setAs("NULL", "FLQuant", function(from) FLQuant())
# }}}

# replacement, [[<-, $, [<- {{{

#' @rdname Extract
#' @aliases [[<-,FLlst,ANY,missing,ANY-method
setReplaceMethod("[[", signature(x="FLlst", i="ANY", j="missing", value="ANY"),
	function(x, i, j, value)
	{
		if(isTRUE(x@lock) & (
			(is.character(i) & is.na(match(i, names(x))))
			|
			(is.numeric(i) & length(x)<i)))
				stop("The object is locked. You can not add non-existent elements.")
    
    # ASSIGN by name
    if(is.character(i)) {
      if(is.na(match(i, names(x)))) {
        x@.Data[[length(x) + 1]] <- value
        names(x)[length(x)] <- i
      } else {
        x@.Data[[match(i, names(x))]] <- value
      }
    }
    # or position
    else
      x@.Data[[i]] <- value

    if(validObject(x))
      return(x)
    else
			stop("Invalid object, classes do not match.")
	}
)

#' @rdname Extract
#' @aliases $<-,FLlst,ANY-method
setReplaceMethod("$", signature(x="FLlst", value="ANY"),
	function(x, name, value)
	{
		if(isTRUE(x@lock) & is.na(match(name, names(x))))
			stop("The object is locked. You can not add non-existent elements.")
		
    # ASSIGN by name
    x@.Data[[match(name, names(x))]] <- value

		if(validObject(x))
			return(x)
		else
			stop("Invalid object, classes do not match.")
})

#' @rdname Extract
#' @aliases [<-,FLlst,ANY,missing,ANY-method
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

# [ {{{
#' @rdname Extract
#' @aliases [,FLlst,ANY,missing,ANY-method
setMethod("[", signature(x="FLlst", i="ANY", j="missing", drop="ANY"),
  function(x, i, drop) {
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
		
    # GET extra args list
    lstargs <- list(...)

    # GET named list of elements
    lstargs$X <- setNames(X@.Data, nm=names(X))

    # ADD function
    lstargs$FUN <- match.fun(FUN)

    # CALL
    lst <- do.call("lapply", lstargs)

    # GET output class
    cls <- getPlural(lst[[1]])

    # CREATE output object if not list
    if(cls != 'list') {
      # IF output elements same class as input elements, return same class as X
      if(is(lst[[1]], class(X[[1]])))
        cls <- as.character(class(X))
      
      lst <- new(cls, lst, lock=FALSE, names=attr(X, 'names'),
        desc=attr(X, 'desc'))
    }

    return(lst)
	}
)  # }}}

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

# summary {{{
#' @rdname summary-methods
#' @aliases summary,FLlst-methods
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

# range {{{
setMethod("range", "FLlst",
  function(x, i = "missing", ..., na.rm = FALSE)
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

# combine {{{
setMethod("combine", signature(x="FLlst", y="missing"),
  function(x) {
    
    res <- propagate(x[[1]], length(x))
    for(i in seq(2, length(x)))
      res[,,,,,i] <- x[[i]]
    return(res)
  }
) 

setMethod("combine", signature(x="FLlst", y="FLlst"),
  function(x, y, ...) {
 
    args <- c(list(x, y), list(...))  
    
    nms <- names(x)

    for(i in nms)
      x[[i]] <- do.call(combine, lapply(args, "[[", i))
    
    return(x)
  }
) # }}}

# window {{{
setMethod("window", signature(x="FLlst"),
  function(x, ... ) {
    lapply(x, function(y) do.call("window", c(list(y), list(...))))
  }
) # }}}

# fwdWindow {{{
setMethod("fwdWindow", signature(x="FLStocks"),
  function(x, ... ) {
    lapply(x, function(y) do.call("fwdWindow", c(list(y), list(...))))
  }
) 

setMethod("fwdWindow", signature(x="FLIndices"),
  function(x, ... ) {
    lapply(x, function(y) do.call("fwdWindow", c(list(y), list(...))))
  }
) 
# }}}

# iter {{{
setMethod("iter", signature(obj="FLlst"),
	  function(obj, iter) {
    return(lapply(obj, "iter", iter=iter))
	  }
) # }}}

# FLIndices accessors {{{

# index
setMethod("index", signature(object="FLIndices"),
  function(object) {
    return(FLQuants(lapply(object, "index")))
  }
)

setMethod("index<-", signature(object="FLIndices", value="FLQuants"),
  function(object, value) {
    return(FLIndices(Map(function(x, y) {
      index(x) <- y
      return(x) },
    x=object, y=value)))
  }
)

# index.var
setMethod("index.var", signature(object="FLIndices"),
  function(object) {
    return(FLQuants(lapply(object, "index.var")))
  }
)

setMethod("index.var<-", signature(object="FLIndices", value="FLQuants"),
  function(object, value) {
    return(FLIndices(Map(function(x, y) {
      index.var(x) <- y
      return(x) },
    x=object, y=value)))
  }
)

# index.q
setMethod("index.q", signature(object="FLIndices"),
  function(object) {
    return(FLQuants(lapply(object, "index.q")))
  }
)

setMethod("index.q<-", signature(object="FLIndices", value="FLQuants"),
  function(object, value) {
    return(FLIndices(Map(function(x, y) {
      index.q(x) <- y
      return(x) },
    x=object, y=value)))
  }
)

# sel.pattern
setMethod("sel.pattern", signature(object="FLIndices"),
  function(object) {
    return(FLQuants(lapply(object, "sel.pattern")))
  }
)

setMethod("sel.pattern<-", signature(object="FLIndices", value="FLQuants"),
  function(object, value) {
    return(FLIndices(Map(function(x, y) {
      sel.pattern(x) <- y
      return(x) },
    x=object, y=value)))
  }
)

# catch.n
setMethod("catch.n", signature(object="FLIndices"),
  function(object) {
    return(FLQuants(lapply(object, "catch.n")))
  }
)

setMethod("catch.n<-", signature(object="FLIndices", value="FLQuants"),
  function(object, value) {
    return(FLIndices(Map(function(x, y) {
      catch.n(x) <- y
      return(x) },
    x=object, y=value)))
  }
)

# catch.wt
setMethod("catch.wt", signature(object="FLIndices"),
  function(object) {
    return(FLQuants(lapply(object, "catch.wt")))
  }
)

setMethod("catch.wt<-", signature(object="FLIndices", value="FLQuants"),
  function(object, value) {
    return(FLIndices(Map(function(x, y) {
      catch.wt(x) <- y
      return(x) },
    x=object, y=value)))
  }
)

# effort
setMethod("effort", signature(object="FLIndices"),
  function(object) {
    return(FLQuants(lapply(object, "effort")))
  }
)

setMethod("effort<-", signature(object="FLIndices", value="FLQuants"),
  function(object, value) {
    return(FLIndices(Map(function(x, y) {
      effort(x) <- y
      return(x) },
    x=object, y=value)))
  }
)
# }}}
