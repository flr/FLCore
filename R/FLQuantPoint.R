# FLQuantPoint - FLQuant class summarizing iters
# FLCore/R/FLQuantPoint.R

# Copyright 2003-2015 FLR Team. Distributed under the GPL 2 or later
# Maintainer: Iago Mosqueira, EC JRC G03
# $Id: FLQuantPoint.R 1779 2012-11-23 09:39:31Z imosqueira $

# FLQuantPoint()	{{{

#' @rdname FLQuantPoint
#' @aliases FLQuantPoint,missing-method
setMethod("FLQuantPoint", signature(object="missing"),
  function(..., units='NA') {

    args <- list(...)

    # empty object
    if(length(args) == 0) {
      res <- new('FLQuantPoint')
      units(res) <- units
    }
    else {
      # valid names
      if(any(!names(args) %in% c('mean', 'median', 'var', 'uppq', 'lowq')))
       stop(paste("Invalid names for FLQuantPoint elements:",
          names(args)[!names(args) %in% c('mean', 'median', 'var', 'uppq', 'lowq')]))

      # check dimnames
      dmns <- c(dimnames(args[[1]])[1:5],
        list(iter=c('mean', 'median', 'var', 'uppq', 'lowq')))

      res <- new('FLQuantPoint', FLQuant(NA, dimnames=dmns, units=units))

      for(i in names(args))
        res[,,,,,i] <- args[[i]]
    }
    return(res)
  }
)

#' @rdname FLQuantPoint
#' @aliases FLQuantPoint,FLQuant-method
setMethod("FLQuantPoint", signature(object="FLQuant"),
  function(object, ..., units='NA') {

    # new object
    res <- new('FLQuantPoint', FLQuant(NA, dimnames=c(dimnames(object)[1:5],
      iter=list(c('mean', 'median', 'var', 'uppq', 'lowq'))), units=units))

        # load values
        res[,,,,,'mean'] <- apply(object, 1:5, mean, na.rm=TRUE)
        res[,,,,,'median'] <- apply(object, 1:5, median, na.rm=TRUE)
        res[,,,,,'var'] <- apply(object, 1:5, var, NULL, na.rm=TRUE)
		    # quantile free or 0.05 & 0.95
        res[,,,,,'lowq'] <- quantile(object, 0.25, na.rm=TRUE)
        res[,,,,,'uppq'] <-quantile(object, 0.75, na.rm=TRUE)

        return(res)
    }
)	# }}}

# show     {{{
# TODO show median(var) or [lowq-uppq]
setMethod("show", signature(object="FLQuantPoint"),
	function(object){
		cat("An object of class \"FLQuantPoint\":\n")
		cat("-- median:\n")
		if(any(complete.cases(object)))
			print(unclass(median(object)@.Data), digits=3)
		else
			print(unclass(array(NA, dimnames=dimnames(object@.Data)[1:5],
				dim=dim(object@.Data)[1:5])))
		cat("units: ", object@units, "\n")
	}
)   # }}}

# random generators	{{{
# rnorm(FLQuantPoint, missing)
setMethod("rnorm", signature(n='numeric', mean="FLQuantPoint", sd="missing"),
	function(n=1, mean)
	rnorm(n, mean(mean), sqrt(var(mean)))
)
# rlnorm
setMethod("rlnorm", signature(n='numeric', meanlog="FLQuantPoint", sdlog="missing"),
	function(n=1, meanlog)
	rlnorm(n, mean(meanlog), sqrt(var(meanlog)))
)
# rgamma
setMethod("rgamma", signature(n='numeric', shape="FLQuantPoint", rate="missing",
	scale="missing"),
	function(n=1, shape) {
		return(FLQuant(rgamma(prod(dim(shape)[-6])*n, shape=mean(shape)^2/var(shape),
			scale=var(shape)/mean(shape)), dimnames=c(dimnames(shape)[-6],
			list(iter=seq(n)))))
	}
)
# pearson
# }}}

# accesors	{{{
setMethod("mean", signature(x="FLQuantPoint"),
	function(x, ...)
		return(FLQuant(x[,,,,,'mean']))
)
setMethod("mean<-", signature(x="FLQuantPoint"),
	function(x, value) {
		x[,,,,,'mean'] <- value
		return(x)
	}
)

setMethod("median", signature(x="FLQuantPoint"),
	function(x, na.rm=FALSE)
		return(FLQuant(x[,,,,,'median']))
)
setMethod("median<-", signature(x="FLQuantPoint", value="ANY"),
	function(x, value) {
		x[,,,,,'median'] <- value
		return(x)
	}
)

setMethod("var", signature(x="FLQuantPoint"),
	function(x, y=NULL, na.rm=FALSE, use)
		return(FLQuant(x[,,,,,'var']))
)

setMethod("var<-", signature(x="FLQuantPoint", value="ANY"),
	function(x, value) {
		x[,,,,,'var'] <- value
		return(x)
	}
)

setMethod("uppq", signature(x="FLQuantPoint"),
	function(x, ...)
		return(FLQuant(x[,,,,,'uppq']))
)

setMethod("uppq<-", signature(x="FLQuantPoint", value="ANY"),
	function(x, value) {
		x[,,,,,'uppq'] <- value
		return(x)
	}
)

setMethod("lowq", signature(x="FLQuantPoint"),
	function(x, ...)
		return(FLQuant(x[,,,,,'lowq']))
)

setMethod("lowq<-", signature(x="FLQuantPoint", value="ANY"),
	function(x, value) {
		x[,,,,,'lowq'] <- value
		return(x)
	}
) # }}}

# quantile   {{{
setMethod("quantile", signature(x="FLQuantPoint"),
	function(x, probs=0.25, na.rm=FALSE, dim=1:5, ...) {
		if(probs==0.25)
			return(lowq(x))
		else if (probs==0.75)
			return(uppq(x))
		else
			stop("Only the 0.25 and 0.75 quantiles are available on an FLQuantPoint object")
	}
)   # }}}

# summary          {{{
#' @rdname summary-methods
#' @aliases summary,FLQuantPoint-method
setMethod("summary", signature(object="FLQuantPoint"),
	function(object, ...){
		cat("An object of class \"FLQuantPoint\" with:\n")
		cat("dim  : ", dim(object), "\n")
		cat("quant: ", quant(object), "\n")
		cat("units: ", units(object), "\n\n")
		cat("1st Qu.: ", mean(lowq(object)), "\n")
		cat("Mean   : ", mean(mean(object)), "\n")
		cat("Median : ", mean(median(object)), "\n")
		cat("Var    : ", mean(var(object)), "\n")
		cat("3rd Qu.: ", mean(uppq(object)), "\n")
	}
)   # }}}
