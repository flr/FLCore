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

