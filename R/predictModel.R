# predictModel.R - DESC
# predictModel.R

# Copyright 2015 Iago Mosqueira. Distributed under the GPL 2.
# Maintainer: Iago Mosqueira, EC JRC G03

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
    return(eval(object@model[[2]], c(object, as(object@params, 'list'))))
  }
) # }}}

# predict(FLComp, character) {{{
setMethod('predict', signature(object='FLComp'),
  function(object, slot) {

    slot <- slot(object, slot)
    
    args <- all.vars(slot@model)
    nams <- all.names(slot@model)


    res <- c(slot, as(slot@params, 'list'))

    # MISSING args?
    args <- args[!args %in% names(res)]

    if(length(args) > 0) {

     # IF $ is in the formula, assume name 2 positions after is a 'quant' name
      if("$" %in% nams) {
        idx <- grep("\\$", nams) + 2
        args <- args[!args %in% nams[idx]]
      }

      # CALL methods on object (inc. accessors)
      for(i in args) {
          res[[i]] <- do.call(i, list(object))
      }
    }
    return(eval(slot@model[[2]], res))
  }
) # }}}

# summary {{{
setMethod('summary', signature(object='predictModel'),
	function(object) {
		callNextMethod()
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
		
		cat("model: ", "\n")
		show(object@model)
		
        cat("\nparams: ", "\n")
		show(object@params)

        if(length(object) > 0)
          callNextMethod()
	}
) # }}}