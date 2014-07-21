# FLModelSim.R - A class for models used in simulation
# FLCore/R/FLModelSim.R

# Copyright 2003-2013 FLR Team. Distributed under the GPL 2 or later
# Maintainer: FLR Team

# FLModelSim {{{
# validity
validFLMS <- function(object){

	pars <- object@params
	frm <- object@model
	vc <- object@vcov
	# check vcov has the same names
	dnms <- dimnames(vc)
	v1 <- all.equal(dnms[[1]], dnms[[2]])

	# check that the params and the model have the same params names
	dps <- dimnames(pars)$params
	if(any(dps != ""))
		v2 <- dimnames(pars)$params %in% all.vars(frm)
	else
		v2 <- TRUE

	# check that the params and the vcov have the same params names
	v3 <- dnms[[1]] %in% all.vars(frm)

	if(sum(!c(v1, v2, v3))>0)
		return("Object is not valid. Check that the names of the parameters in the params matrix and the vcov match the names of the formula parameters.")

	return(TRUE)
}


setClass("FLModelSim",
	representation(
		model = "formula",
		params = "FLPar",
		vcov = "array",
		distr = "character"),
	prototype = prototype(
		model = ~1,
		params = new("FLPar"),
		vcov = new("array"),
		distr = "norm"),
	validity=validFLMS
) # }}}

# FLModelSim() {{{
setGeneric("FLModelSim", function(object, ...)
	standardGeneric("FLModelSim"))

setMethod("FLModelSim", signature(object="missing"),
  function(...) {
    # empty
  	if(missing(...)){
	  	new("FLModelSim")
    # or not
  	} else {
      args <- list(...)
      if("vcov" %in% names(args))
      	if(length(dim(args$vcov))==1) dim(args$vcov) <- c(1,1) 
	  args$Class <- 'FLModelSim'
      do.call("new", args)
	  }
  }
)

invisible(createFLAccesors("FLModelSim", include=c("model", "params", "vcov", "distr")))  # }}}

# mvrnorm {{{
setMethod("mvrnorm", signature(n="numeric", mu="FLModelSim", Sigma="missing",
	tol="missing", empirical="missing", EISPACK="missing"),
	function(n, mu) {
		object <- mu
		mu <- iterMedians(params(object))
		dm <- dim(mu)
		dnm <- dimnames(mu)

		# check that params second dim is "iter"
		if(names(dnm)[2]!="iter") stop("To apply this method params must have 2 dimensions only and the second has to be \"iter\".")	

		Sigma = apply(vcov(object),c(1,2),median)
		res <- do.call("mvrnorm", list(mu=c(mu), Sigma=Sigma, n=n))
		
		if(n>1)
			res <- t(res)
		else
			res <- matrix(res, ncol=1)

		dnm$iter <- 1:n
		dimnames(res) <- dnm
		res <- FLPar(res)
		units(res) <- units(mu)
		return(FLModelSim(model=model(object), params=res))
	}
)

setMethod("mvrnorm", signature(n="FLModelSim", mu="missing", Sigma="missing",
	tol="missing", empirical="missing", EISPACK="missing"),
	function(n) {
		
		object <- n	
		mu <- params(object)
		dm <- dim(mu)
		dnm <- dimnames(mu)

		# check that params second dim is "iter"
		if(names(dnm)[2]!="iter") stop("To apply this method params must have 2 dimensions only and the second has to be \"iter\".")	

		Sigma = vcov(object)
		
		if(dm[2]>1){
			res <- array(NA, dim=dm, dimnames=dnm)

			for(i in 1:dm[2])
				res[,i] <- do.call("mvrnorm", list(mu=mu[,i], Sigma=Sigma[,,i]))
			res <- FLPar(res)
			units(res) <- units(mu)
			res <- FLModelSim(model=model(object), params=res)
		} else {
			res <- mvrnorm(1,object)
		}
	return(res)
	}
) # }}}

# predict {{{
setMethod("predict", signature(object="FLModelSim"),
	function(object, ...) {

	args <- list(...)
	pr <- params(object)
	res <- apply(pr,2,function(x) {
		lst <- as.list(x)
		eval(as.list(model(object))[[2]], envir=c(args, lst))
	})
	res <- matrix(res, ncol=dim(pr)[2])
	dimnames(res) <- list(1:nrow(res), iter=1:dim(pr)[2])
	return(res)
	}
) # }}}
