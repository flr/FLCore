# FLQuantPoint - FLQuant class summarizing iters
# FLCore/R/FLQuantPoint.R

# Copyright 2003-2020 FLR Team. Distributed under the GPL 2 or later
# Maintainer: Iago Mosqueira (WMR) <iago.mosqueira@wur.nl>

# FLQuantPoint()	{{{

#' @rdname FLQuantPoint
#' @aliases FLQuantPoint,missing-method
setMethod("FLQuantPoint", signature(object="missing"),
  function(..., units='NA', n=1) {

    args <- list(...)

    # empty object
    if(length(args) == 0) {
      res <- new('FLQuantPoint', n=n)
      units(res) <- units
    }
    else {
      # valid names
      if(any(!names(args) %in% c('mean', 'median', 'var', 'uppq', 'lowq')))
       stop(paste("Invalid names for FLQuantPoint elements:",
          names(args)[!names(args) %in%
          c('mean', 'median', 'var', 'uppq', 'lowq')]))

      # check dimnames
      dmns <- c(dimnames(args[[1]])[1:5],
        list(iter=c('mean', 'median', 'var', 'uppq', 'lowq')))

      res <- new('FLQuantPoint', FLQuant(NA, dimnames=dmns, units=units), n=n)

      for(i in names(args))
        res[,,,,,i] <- args[[i]]
    }
    return(res)
  }
)

#' @rdname FLQuantPoint
#' @aliases FLQuantPoint,FLQuant-method
setMethod("FLQuantPoint", signature(object="FLQuant"),
  function(object, ..., probs=c(0.25, 0.75)) {

    # new object
    res <- new('FLQuantPoint', FLQuant(NA, dimnames=c(dimnames(object)[1:5],
      iter=list(c('mean', 'median', 'var', 'uppq', 'lowq'))),
      units=units(object)), n=dim(object)[6])

    # load values
    res[,,,,,'mean'] <- c(apply(object, 1:5, mean, na.rm=TRUE))
    res[,,,,,'median'] <- c(apply(object, 1:5, median, na.rm=TRUE))
    res[,,,,,'var'] <- c(apply(object, 1:5, var, NULL, na.rm=TRUE))
		# quantile free or 0.05 & 0.95
    res[,,,,,'lowq'] <- c(quantile(object, probs[1], na.rm=TRUE))
    res[,,,,,'uppq'] <- c(quantile(object, probs[2], na.rm=TRUE))

    return(res)
    }
)	# }}}

# show     {{{
# TODO show median(var) or [lowq-uppq]
setMethod("show", signature(object="FLQuantPoint"),
	function(object){
		
    cat("An object of class \"FLQuantPoint\":\n")

    # CHOOSE mean or median
    if(any(complete.cases(median(object)))) {
		  cat("-- median:\n")
			print(unclass(median(object)@.Data), digits=3)
    } else if(any(complete.cases(mean(object)))) {
		  cat("-- mean:\n")
			print(unclass(mean(object)@.Data), digits=3)
    } else {
			print(unclass(array(NA, dimnames=dimnames(object@.Data)[1:5],
				dim=dim(object@.Data)[1:5])))
    }
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
)

#' @rdname FLQuantPoint
#' @example
#' n(flqp)

setMethod("n", signature(object="FLQuantPoint"),
	function(object, ...)
		return(object@n)
)
setMethod("n<-", signature(object="FLQuantPoint"),
	function(object, value) {
		object@n <- value
		return(object)
	}
)

# }}}

# cv {{{
setMethod("cv", signature(x="FLQuantPoint"),
	function(x, ...)
		return(sqrt(var(x)) / mean(x))
) # }}}

# se {{{
setMethod("se", signature(x="FLQuantPoint"),
	function(x, ...)
		return(sqrt(var(x)) / sqrt(n(x)))
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

# as.data.frame {{{
setMethod("as.data.frame", signature(x="FLQuantPoint", row.names="missing",
  optional="missing"),
    function(x, cohort=FALSE, timestep=FALSE, date=FALSE, drop=FALSE,
      units=FALSE) {
      as.data.frame(x, row.names=NULL, cohort=cohort, timestep=timestep,
        date=date, drop=drop, units=units)
    }
)
setMethod("as.data.frame", signature(x="FLQuantPoint", row.names="ANY",
  optional="missing"),
function(x, row.names, cohort=FALSE, timestep=FALSE, date=FALSE, drop=FALSE,
  units=FALSE) {

    # COERCE as if FLQuant
    df <- callNextMethod(x, cohort=cohort, timestep=timestep,
      date=date, drop=drop, units=units)

    # FIND dimensions not dropped
    idvar <- colnames(df)[!colnames(df) %in% c("iter", "data")]

    # RESHAPE to wide
    dat <- reshape(df, idvar=idvar, timevar = "iter", direction = "wide")

    # RENAME data columns as iters
    colnames(dat) <- sub("data.", "", colnames(dat))

    return(dat)
  } 
) # }}}

se <- function(x) sqrt(var(x)) / sqrt(n(x)) # Create own function
