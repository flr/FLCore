# FLQuantPoint - FLQuant class summarizing iters
# FLCore/R/FLQuantPoint.R

# Copyright 2003-2016 FLR Team. Distributed under the GPL 2 or later
# Maintainer: Iago Mosqueira, EC JRC

# FLQuantPoint()	{{{

# FLQuantPoint(FLQuant())
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

      for(i in length(args))
        res[,,,,,i] <- args[[i]]
    }
    return(res)
  }
)
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

#' @rdname show
#' @aliases show,FLQuantPoint-method

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

#' Method rgamma
#' 
#' Random generation for the Gamma distribution with parameters 'shape' and
#' 'scale'. 'shape' can be of class \code{\link{FLQuantPoint}} in which case
#' 'shape' and 'scale' are set as \eqn{\hat{x}^2 / \sigma^2}{mean^2 / var} and
#' \eqn{\sigma^2 / \hat{x}}{var / mean} respectively.
#'
#' @name rgamma
#' @aliases rgamma,numeric,FLQuantPoint,missing,missing-method
#' @docType methods
#' @section Generic function: rgamma(n,shape,rate,scale)
#' @author The FLR Team
#' @seealso \link[stats]{rgamma}, \linkS4class{FLQuantPoint}
#' @keywords methods
#' @examples
#' 
#' flq <- FLQuant(rnorm(1000,mean=10,sd=2),dim=c(1,10,1,1,1,100))
#' flqp <- FLQuantPoint(flq)
#' rgamma(10,shape=flqp)
#'
#' data(ple4)
#' rgamma(10,FLQuantPoint(rnorm(200, catch(ple4), 20000)))
#'

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

# plot	{{{
# TODO Fix, it is badly broken! 12.09.07 imosqueira
setMethod("plot", signature(x="FLQuantPoint", y="missing"),
	function(x, xlab="year", ylab=paste("data (", units(x), ")", sep=""),
		type='bar', ...) {

		# get dimensions to condition on (length !=1)
		condnames <- names(dimnames(x)[c(1,3:5)][dim(x)[c(1,3:5)]!=1])
		cond <- paste(condnames, collapse="+")
		if(cond != "") cond <- paste("|", cond)
		formula <- formula(paste("data~year", cond))
		# set strip to show conditioning dimensions names
		strip <- strip.custom(var.name=condnames, strip.names=c(TRUE,TRUE))

		pfun <- function(x, y, subscripts, groups, ...){
			larrows(x[groups[subscripts]=='lowq'], y[groups[subscripts]=='lowq'],
				x[groups[subscripts]=='uppq'], y[groups[subscripts]=='uppq'], angle=90,
				length=0.05, ends='both')
			lpoints(x[groups[subscripts]=='mean'], y[groups[subscripts]=='mean'], pch=16)
			lpoints(x[groups[subscripts]=='median'], y[groups[subscripts]=='median'], pch=3)
		}

	# using do.call to avoid eval of some arguments
	lst <- substitute(list(...))
	lst <- as.list(lst)[-1]
    lst$data <- as.data.frame(x)
	lst$x <- formula
	lst$xlab <- xlab
	lst$ylab <- ylab
	lst$strip <- strip
	lst$groups <- lst$data$iter
	lst$subscripts <- TRUE
	lst$panel <- pfun

	do.call("xyplot", lst)
	}
)	# }}}

# quantile   {{{
#' @rdname quantile
#' @aliases quantile,FLQuant-method
#' @examples
#' # Create an FLQuantPoint from a previous FLQuant...
#'   flp <- FLQuantPoint(flq)
#' # ...and return each of the two quantiles (025 and 0.75)...
#'   quantile(flp, 0.25)
#'   quantile(flp, 0.75)
#' # ...or alternatively use lowq and uppq
#'   lowq(flp)
#'   uppq(flp)
#' 
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
