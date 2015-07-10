# predictModel.R - DESC
# predictModel.R

# Copyright 2015 Iago Mosqueira. Distributed under the GPL 2.
# Maintainer: Iago Mosqueira, JRC
# Soundtrack:
# Notes:

# class predictModel: model + data + params {{{
setClass('predictModel',
	contains='FLQuants',
	representation(params='FLPar', model='formula')) # }}}

# predictModel() {{{
setMethod('predictModel', signature(object='FLQuants'),
	function(object, model=~a, params=FLPar(a=as.numeric(NA))) {

	# CREATE object
	res <- new("predictModel", object, model=model, params=params)
	
	return(res)
	}
)

setMethod('predictModel', signature(object='missing'),
	function(object, ...) {

	args <- list(...)

	# PARSE for named FLQuant(s)
	if(length(args) > 0) {
		cls <- unlist(lapply(args, is, 'FLQuant'))
		fqs <- do.call('FLQuants', args[cls])
		args <- c(list(object=fqs), args[!cls])
	} else {
		args <- list(object=FLQuants())
	}

	# CREATE object
	res <- do.call('predictModel', args)
	
	return(res)
	}
) # }}}

# model, model<- {{{
setMethod('model', signature(object='predictModel'),
	function(object) {
		return(object@model)
	}
)
setReplaceMethod('model', signature(object='predictModel', value='formula'),
	function(object, value) {
		object@model  <- value
		return(object)
	}
) # }}}

# params, params<- {{{
setMethod('params', signature(object='predictModel'),
	function(object) {
		return(object@params)
	}
)
setReplaceMethod('params', signature(object='predictModel', value='FLPar'),
	function(object, value) {
		object@params  <- value
		return(object)
	}
) # }}}

# predict(predictModel) {{{
setMethod('predict', signature(object='predictModel'),
	function(object) {
		return(eval(object@model[[3]], c(object, as(object@params, 'list'))))
	}
) # }}}

# summary {{{
setMethod('summary', signature(object='predictModel'),
	function(object) {
		summary(object)
		print(names(object))
		print(object@model)
	}
) # }}}

# [, [[<- {{{
setMethod('[', signature(x="predictModel", i="ANY", j="missing"),
	function(x, i) {
		x@.Data <- x@.Data[i]
		return(x)
	}
)
# }}}

# show {{{
setMethod('show', signature(object='predictModel'),
	function(object) {
		
		callNextMethod()
		cat("model: ", "\n")
		show(object@model)
		cat("\nparams: ", "\n")
		show(object@params)
	}
) # }}}

