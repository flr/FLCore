# FLModelSim.R - A class for models used in simulation
# FLCore/R/FLModelSim.R

# Copyright 2003-2013 FLR Team. Distributed under the GPL 2 or later
# Maintainer: FLR Team

# FLModelSim {{{
setClass("FLModelSim",
	representation(
		model = "formula",
		params = "FLPar",
		vcov = "array",
		distr = "character"),
	prototype = prototype(
		model = ~a,
		params = new("FLPar", array(1, dim=c(1,1), dimnames=list(params="a", iter="1"))),
		vcov = new("array"),
		distr = "norm")
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
	  args$Class <- 'FLModelSim'
      do.call("new", args)
	  }
  }
)
setGeneric("distr", function(object, ...) standardGeneric("distr"))
setGeneric("distr<-", function(object, ..., value) standardGeneric("distr<-"))
invisible(createFLAccesors("FLModelSim", include=c("model", "params", "vcov", "distr")))  # }}}

# mvrnorm {{{
setMethod("mvrnorm", signature(n="numeric", mu="FLModelSim", Sigma="missing",
	tol="missing", empirical="missing", EISPACK="missing"),
	function(n, mu) {
		
		object <- mu
		mu <- iterMedians(params(object))
		dm <- dim(mu)
		dnm <- dimnames(mu)
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
	dimnames(res) <- list(pred=1:nrow(res), iter=1:dim(pr)[2])
	return(res)
	}
) # }}}
