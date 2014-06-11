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
		var <- new('FLArray', FLQuant(var)@.Data)

		return(FLQuantDistr(object=object, var=var, ...))
	}
)

setMethod("FLQuantDistr", signature(object="FLQuant", var="FLQuant"),
  function(object, var, ...) {
	var <- new('FLArray', var@.Data)
	return(FLQuantDistr(object, var=var, ...))
	}
)

setMethod("FLQuantDistr", signature(object="FLQuant", var="FLArray"),
  function(object, var, units='NA', distr="norm") {
		
		# set units in .Data object
		if(!missing(units))
			units(object) <- units
		
		return(new("FLQuantDistr", object, var=var, distr=distr))
	}
)
	# }}}

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
)

# }}}

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

setMethod("+",
	signature(e1 = "FLQuantDistr", e2 = "FLQuantDistr"),
	function(e1, e2) {
		e1@.Data <- e1@.Data + e2
		units(e1) <- uom('+', units(e1), units(e2))
		return(e1)
	}
)



# }}}
