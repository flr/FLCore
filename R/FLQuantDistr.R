# FLQuantDistr - «Short one line description»
# FLCore/R/FLQuantDistr.R

# Copyright 2003-2012 FLR Team. Distributed under the GPL 2 or later
# Maintainer: Iago Mosqueira, JRC
# $Id: FLQuantPoint.R 1779 2012-11-23 09:39:31Z imosqueira $

## FLQuantDistr()	{{{
setMethod("FLQuantDistr", signature(object="ANY", var="ANY"),
	function(object, var, ...) {

		# object
		object <- FLQuant(object)
		# var
		var <- FLQuant(var)

		return(FLQuantDistr(object=object, var=var, ...))
	}
)

setMethod("FLQuantDistr", signature(object="FLQuant", var="FLQuant"),
  function(object, var, units=object@units, distr='norm') {
		return(new('FLQuantDistr', object, var=var, units=units, distr=distr))
	}
) # }}}

## show     {{{
# TODO show median(var) or [lowq-uppq]
setMethod("show", signature(object="FLQuantDistr"),
	function(object){
		cat("An object of class \"FLQuantDistr\":\n")

    v3 <- paste(format(object@.Data,digits=5),"(", format(object@var, digits=3), ")", sep="")
    print(array(v3, dim=dim(object)[1:5], dimnames=dimnames(object)[1:5]), quote=FALSE)

		cat("units: ", object@units, "\n")
		cat("distr: ", object@distr, "\n")
	}
)   # }}}

## random generators	{{{
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
# gamma
if (!isGeneric("rgamma"))
	setGeneric("rgamma", useAsDefault=rgamma)

setMethod("rgamma", signature(n='numeric', shape="FLQuantPoint", rate="missing",
	scale="missing"),
	function(n=1, shape)
	FLQuant(rgamma(n, shape=mean(shape)^2/var(shape), scale=var(shape)/mean(shape)),
		dim=c(dim(shape)[-6], n))
)
# pearson
# }}}

## accesors	{{{
setMethod("e", signature(x="FLQuantDistr"),
	function(x)
		return(FLQuant(x@.Data, units=units(x)))
)

setMethod("e<-", signature(x="FLQuantDistr", value="FLQuant"),
	function(x, value) {
		x@.Data <- value
		return(x)
	}
)

setMethod("var", signature(x="FLQuantDistr"),
	function(x)
		return(x@var)
)

setMethod("var<-", signature(x="FLQuantDistr", value="FLArray"),
	function(x, value) {
		x@var <- value
		return(x)
	}
)

setMethod("distr", signature(object="FLQuantDistr"),
	function(object)
		return(object@distr)
)

setMethod("distr<-", signature(object="FLQuantDistr", value="character"),
	function(object, value) {
		object@distr <- value
		return(object)
	}
) # }}}

# sd, cv {{{
setMethod("sd", signature(x="FLQuantDistr"),
	function(x, na.rm=TRUE)
		return(sqrt(var(x)))
)

setMethod("cv", signature(x="FLQuantDistr"),
	function(x)
		return(sd(x) / e(x))
) # }}}

# Arith {{{

# FLQuantDistr, FLArray
setMethod("+",
	signature(e1 = "FLQuantDistr", e2 = "FLArray"),
	function(e1, e2) {
		e1@.Data <- e1@.Data + e2
		units(e1) <- uom('+', units(e1), units(e2))
		return(e1)
	}
)

setMethod("-",
	signature(e1 = "FLQuantDistr", e2 = "FLArray"),
	function(e1, e2) {
		e1@.Data <- e1@.Data - e2
		units(e1) <- uom('-', units(e1), units(e2))
		return(e1)
	}
)
setMethod("*",
	signature(e1 = "FLQuantDistr", e2 = "FLArray"),
	function(e1, e2) {
		e1@.Data <- e1@.Data * e2@.Data
		e1@var@.Data <- e2@.Data^2 * e1@var
		units(e1) <- uom('*', units(e1), units(e2))
		return(e1)
	}
)
setMethod("/",
	signature(e1 = "FLQuantDistr", e2 = "FLArray"),
	function(e1, e2) {
		e1@.Data <- e1@.Data / e2@.Data
		e1@var@.Data <- 1/e2@.Data^2 * e1@var
		units(e1) <- uom('/', units(e1), units(e2))
		return(e1)
	}
) 

# FLQuantDistr, FLQuantDistr
setMethod("*",
	signature(e1 = "FLQuantDistr", e2 = "FLQuantDistr"),
	function(e1, e2) {

		dis <- unique(c(distr(e1), distr(e2)))

		# Both distr must be equal
		if(length(dis) > 1)
			stop ("Both objects must be of same 'distr': ", dis)

		# Both objects must be either 'norm' or 'lnorm'
		if(dis %in% c('norm', 'lnorm')) {
			var(e1)[] <- e1@.Data^2 * var(e2) + e2@.Data^2 * var(e1)
		} else {
			stop("Operation only defined for distr='norm' or 'lnorm'")
		}
		e1@.Data <- e1@.Data * e2@.Data
		units(e1) <- uom('*', units(e1), units(e2))
		
		return(e1)
	}
)

setMethod("+",
	signature(e1 = "FLQuantDistr", e2 = "FLQuantDistr"),
	function(e1, e2) {

		dis <- unique(c(distr(e1), distr(e2)))

		# Both distr must be equal
		if(length(dis) > 1)
			stop ("Both objects must be of same 'distr': ", dis)

		# Both objects must be either 'norm' or 'lnorm'
		if(dis %in% c('norm', 'lnorm')) {
			e1@var[] <- var(e1) + var(e2)
		} else {
			stop("Operation only defined for distr='norm' or 'lnorm'")
		}
		e1@.Data <- e1@.Data + e2@.Data
		units(e1) <- uom('+', units(e1), units(e2))
		
		return(e1)
	}
)

setMethod("-",
	signature(e1 = "FLQuantDistr", e2 = "FLQuantDistr"),
	function(e1, e2) {

		dis <- unique(c(distr(e1), distr(e2)))

		# Both distr must be equal
		if(length(dis) > 1)
			stop ("Both objects must be of same 'distr': ", dis)

		# Both objects must be either 'norm' or 'lnorm'
		if(dis %in% c('norm', 'lnorm')) {
			var(e1)[] <- var(e1) + var(e2)
		} else {
			stop("Operation only defined for distr='norm' or 'lnorm'")
		}
		e1@.Data <- e1@.Data - e2@.Data
		units(e1) <- uom('+', units(e1), units(e2))
		
		return(e1)
	}
) # }}}

## "["             {{{
setMethod("[", signature(x="FLQuantDistr"),
    function(x, i, j, k, l, m, n, ..., drop=FALSE) {

   		if(length(list(...)) > 0)
        stop('FLQuantDistr objects only have 6 dimensions')

	  	dx <- dim(x)
		  if (missing(i))
        i  <-  seq(1, dx[1])
      if (missing(j))
        j  <-  seq(1, dx[2])
      if (missing(k))
        k  <-  seq(1, dx[3])
      if (missing(l))
        l  <-  seq(1, dx[4])
      if (missing(m))
        m  <-  seq(1, dx[5])
      if (missing(n))
        n  <-  seq(1, dx[6])
			
			res <- x
			res@.Data <- do.call('[', list(x=x@.Data, i=i, j=j, k=k, l=l,
				m=m, n=n, drop=FALSE))
			res@var <- do.call('[', list(x=x@var, i=i, j=j, k=k, l=l,
				m=m, n=n, drop=FALSE))
      
      return(res)
	}
) 

setMethod("[", signature(x="FLQuantDistr", i="array", j="missing", drop="missing"),
  function(x, i)
  {
		res <- x
		
		res@.Data <- do.call('[', list(x=e(x), i=i))
		res@var <- do.call('[', list(x=var(x), i=i))

		return(res)
  }
)


# }}}