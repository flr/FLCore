# genericMethods - S4 generics
# FLCore/R/genericMethods

# Copyright 2003-2016 FLR Team. Distributed under the GPL 2 or later
# Maintainer: Iago Mosqueira, EC JRC

globalVariables(c("qname"))

# -- OVERLOADED methods/functions {{{

setGeneric("AIC", useAsDefault = stats::AIC)
setGeneric("apply", useAsDefault = apply)
setGeneric("as.data.frame", useAsDefault = as.data.frame)
setGeneric("barchart", useAsDefault = lattice::barchart)
setGeneric("bwplot", useAsDefault = lattice::bwplot)
setGeneric("coef", useAsDefault = coef)
setGeneric("cor", useAsDefault = cov)
setGeneric("cov", useAsDefault = cov)
setGeneric("densityplot", useAsDefault = densityplot)
setGeneric("drop", useAsDefault = drop)
setGeneric("dotplot", useAsDefault = dotplot)
setGeneric("expand", useAsDefault=Matrix::expand)
setGeneric("histogram", useAsDefault = histogram)
setGeneric("ifelse", useAsDefault = ifelse)
setGeneric("lapply", useAsDefault = lapply)
setGeneric("lm", useAsDefault = lm)
setGeneric("lowess", useAsDefault = lowess)
setGeneric("mean", useAsDefault = mean)
setGeneric("median", useAsDefault = median)
setGeneric("model.frame", useAsDefault = model.frame)
setGeneric("mvrnorm", useAsDefault = mvrnorm)
setGeneric("names")
setGeneric("names<-")
setGeneric("nls", useAsDefault = nls)
setGeneric("plot")
setGeneric("predict", useAsDefault = predict)
setGeneric("print", useAsDefault = print)
setGeneric("profile", useAsDefault = profile)
setGeneric("quantile", useAsDefault = quantile)
setGeneric("rgamma", useAsDefault = rgamma)
setGeneric("rlnorm", useAsDefault=rlnorm)
setGeneric("rnorm", useAsDefault=rnorm)
setGeneric("rpois", useAsDefault=rpois)
setGeneric("scale", useAsDefault = scale)
setGeneric("sd", useAsDefault = sd)
setGeneric("splom", useAsDefault = splom)
setGeneric("stripplot", useAsDefault = stripplot)
setGeneric("subset", useAsDefault = subset)
setGeneric("summary", useAsDefault = summary)
setGeneric("sweep", useAsDefault = sweep)
setGeneric("tail", useAsDefault = utils::tail)
setGeneric("transform", useAsDefault = transform)
setGeneric("tsp", useAsDefault = tsp)
setGeneric("units", useAsDefault = units)
setGeneric("update", useAsDefault = update)
setGeneric("var", useAsDefault = var)
setGeneric("vcov", useAsDefault = vcov)
setGeneric("window", useAsDefault = window)
setGeneric("wireframe", useAsDefault = wireframe)
setGeneric("xyplot", useAsDefault = xyplot) # }}}

# -- CONSTRUCTORS, documented with each class {{{

# FLBiol
#' @rdname FLBiol
#' @aliases FLBiol FLBiol-methods
setGeneric('FLBiol', function(object, ...) standardGeneric('FLBiol'))

# FLBiols
#' @rdname FLBiols
#' @aliases FLBiols FLBiols-methods
setGeneric("FLBiols", function(object, ...) standardGeneric("FLBiols"))

# FLCohort
#' @rdname FLCohort
#' @aliases FLCohort FLCohort-methods
setGeneric("FLCohort", function(object, ...) standardGeneric("FLCohort"))

# FLCohorts
#' @rdname FLCohorts
#' @aliases FLCohorts FLCohorts-methods
setGeneric("FLCohorts", function(object, ...) standardGeneric("FLCohorts"))

# FLIndex
#' @rdname FLIndex
#' @aliases FLIndex FLIndex-methods
setGeneric('FLIndex', function(object, ...) standardGeneric('FLIndex'))

# FLIndexBiomass
#' @rdname FLIndexBiomass
#' @aliases FLIndexBiomass FLIndexBiomass-methods
setGeneric('FLIndexBiomass', function(object, ...)
  standardGeneric('FLIndexBiomass'))

# FLIndices
#' @rdname FLIndices
#' @aliases FLIndices FLIndices-methods
setGeneric("FLIndices", function(object, ...) standardGeneric("FLIndices"))

# FLlst
#' @rdname FLlst
#' @aliases FLlst FLlst-methods
setGeneric("FLlst", function(object, ...) standardGeneric("FLlst"))

# FLModel
#' @rdname FLModel
#' @aliases FLModel FLModel-methods
setGeneric('FLModel', function(model, ...) standardGeneric('FLModel'))

# FLModelSim
#' @rdname FLModelSim
#' @aliases FLModelSim FLModelSim-methods 
setGeneric("FLModelSim", function(object, ...)
	standardGeneric("FLModelSim"))

# FLPar
#' @rdname FLPar
#' @aliases FLPar FLPar-methods
setGeneric("FLPar", function(object, ...) standardGeneric("FLPar"))

# FLPars
#' @rdname FLPars
#' @aliases FLPars FLPars-methods
setGeneric("FLPars", function(object, ...) standardGeneric("FLPars"))

# FLQuant
#' @rdname FLQuant
#' @aliases FLQuant FLQuant-methods
setGeneric("FLQuant", function(object, ...) standardGeneric("FLQuant"))

# FLQuant
#' @rdname FLQuants
#' @aliases FLQuants FLQuants-methods
setGeneric("FLQuants", function(object, ...) standardGeneric("FLQuants"))

# FLQuantDistr
#' @rdname FLQuantDistr
#' @aliases FLQuantDistr FLQuantDistr-methods
setGeneric("FLQuantDistr", function(object, var, ...)
	standardGeneric("FLQuantDistr"))

# FLQuantPoint
#' @rdname FLQuantPoint
#' @aliases FLQuantPoint FLQuantPoint-methods
setGeneric("FLQuantPoint", function(object, ...)
  standardGeneric("FLQuantPoint"))

# FLSR
#' @rdname FLSR
#' @aliases FLSR FLSR-methods
setGeneric("FLSR", function(model, ...) standardGeneric("FLSR"))

# FLSRs
#' @rdname FLSRs
#' @aliases FLSRs FLSRs-methods
setGeneric("FLSRs", function(object, ...) standardGeneric("FLSRs"))

# FLStock
#' @rdname FLStock
#' @aliases FLStock FLStock-methods
setGeneric('FLStock', function(object, ...) standardGeneric('FLStock'))

# FLStockLen
#' @rdname FLStockLen
#' @aliases FLStockLen FLStockLen-methods
setGeneric('FLStockLen', function(object, ...) standardGeneric('FLStockLen'))

# FLStocks
#' @rdname FLStocks
#' @aliases FLStocks FLStocks-methods
setGeneric("FLStocks", function(object, ...) standardGeneric("FLStocks"))
# }}}

# -- ACCESSORS {{{

#' accessor and replacement methods for FLCore classes
#'
#' All S4 classes defined in FLCore have methods for accessing and replacing any
#' of their slots. These methods are named as the slot, and will return the
#' content of the slot, for the accessor method, or modify it with the provided
#' value.
#'
#' Accessors and replacement methods, with some exception, are created at build
#' time by calls to the \code{createFLAccessors} function. An accessor method is
#' created for each slot, with simply calls \code{slot()} on the relevant slot
#' name. For slots of class \code{\link{FLQuant}}, or \code{FLArray}-based, two
#' methods are created: one if \code{value} is of class \code{FLQuant}, and
#' another for \code{value} being a numeric vector. The later would insert the
#' vector into the slot structure, using R's recycling rules.
#'
#' Users are encouraged to use the accessor methods, rather than the '@' operator
#' or the \code{slot()} method, to isolate code from the internal structure of
#' the class. If a slot was to be altered or deleted in the future, a method
#' would be provided to return the same value, computed from other slots.
#'
#' Some of these methods might already not access directly an slot, and instead
#' carry out a calculation to return the requested value, depending on the class
#' being called with. Please refer to the particular method implementation to
#' see if this is the case.
#'
#' Accessor methods for slots of class \code{\link{predictModel}} behave
#' differently depending on the \code{compute} argument. Please refer to the
#' relevant help page for further clarification.
#'
#' @param object The object from which a slot is to be extracted or replaced
#' @param value Object to be inserted into the relevant slot
#'
#' @return The required slot, for an accessor method, or invisible modifies the
#'   object, for the replacement one.
#'
#' @name accessors
#' @rdname accessors
#'
#' @author The FLR Team
#' @seealso \code{\link{FLQuant}}, \code{\link{FLStock}}, \code{\link{FLIndex}},
#' \code{\link{FLBiol}}, \code{\link{predictModel}}
#' @keywords methods
#' @examples
#'
#' data(ple4)
#'
#' # To access the catch slot in an FLStock, use
#' catch(ple4)
#'
#' # while to modify it, do
#' catch(ple4) <- catch(ple4) * 2
#'
#' # A number can be used as input, to be recycled
#' m(ple4) <- 0.3
#' # same as a longer vector, by age
#' m(ple4) <- 0.4^(seq(1, 2, length=10))
#'
#' # To see the methods defined by createFLAccessors, run, for example
#' getMethod('catch', 'FLS')
#'

# name, name<-
#' @rdname accessors
#' @aliases name, name<- name, name<-<-
setGeneric('name', function(object, ...) standardGeneric('name'))
setGeneric('name<-', function(object, ..., value) standardGeneric('name<-'))

# desc
#' @rdname accessors
#' @aliases desc desc<-
setGeneric('desc', function(object, ...) standardGeneric('desc'))
setGeneric('desc<-', function(object, ..., value) standardGeneric('desc<-'))

# range<-
#' @rdname accessors
#' @aliases range<-
setGeneric("range<-", function(x, i, value) standardGeneric("range<-"))

# units<-
#' @rdname accessors
#' @aliases units<-
setGeneric("units<-", function(x, value) standardGeneric("units<-"))

# catch
#' @rdname accessors
#' @aliases catch catch<-
setGeneric('catch', function(object, ...) standardGeneric('catch'))
setGeneric('catch<-', function(object, ..., value) standardGeneric('catch<-'))

# catch.n
#' @rdname accessors
#' @aliases desc desc<-
setGeneric('catch.n', function(object, ...) standardGeneric('catch.n'))
setGeneric('catch.n<-', function(object, ..., value) standardGeneric('catch.n<-'))

# catch.wt
#' @rdname accessors
#' @aliases catch.wt catch.wt<-
setGeneric('catch.wt', function(object, ...) standardGeneric('catch.wt'))
setGeneric('catch.wt<-', function(object, ..., value) standardGeneric('catch.wt<-'))

# discards
#' @rdname accessors
#' @aliases discards discards<-
setGeneric('discards', function(object, ...) standardGeneric('discards'))
setGeneric('discards<-', function(object, ..., value) standardGeneric('discards<-'))

# discards.n
#' @rdname accessors
#' @aliases discards.n discards.n<-
setGeneric('discards.n', function(object, ...) standardGeneric('discards.n'))
setGeneric('discards.n<-', function(object, ..., value) standardGeneric('discards.n<-'))

# discards.wt
#' @rdname accessors
#' @aliases discards.wt discards.wt<-
setGeneric('discards.wt', function(object, ...) standardGeneric('discards.wt'))
setGeneric('discards.wt<-', function(object, ..., value) standardGeneric('discards.wt<-'))

# landings
#' @rdname accessors
#' @aliases landings landings<-
setGeneric('landings', function(object, ...) standardGeneric('landings'))
setGeneric('landings<-', function(object, ..., value) standardGeneric('landings<-'))

# landings.n
#' @rdname accessors
#' @aliases landings.n landings.n<-
setGeneric('landings.n', function(object, ...) standardGeneric('landings.n'))
setGeneric('landings.n<-', function(object, ..., value) standardGeneric('landings.n<-'))

# landings.wt
#' @rdname accessors
#' @aliases landings.wt landings.wt<-
setGeneric('landings.wt', function(object, ...) standardGeneric('landings.wt'))
setGeneric('landings.wt<-', function(object, ..., value) standardGeneric('landings.wt<-'))

# m
#' @rdname accessors
#' @aliases m m<-
setGeneric('m', function(object, ...) standardGeneric('m'))
setGeneric('m<-', function(object, ..., value) standardGeneric('m<-'))

# stock
#' @rdname accessors
#' @aliases stock stock<-
setGeneric('stock', function(object, ...) standardGeneric('stock'))
setGeneric('stock<-', function(object, ..., value) standardGeneric('stock<-'))

# stock.n
#' @rdname accessors
#' @aliases stock.n stock.n<-
setGeneric('stock.n', function(object, ...) standardGeneric('stock.n'))
setGeneric('stock.n<-', function(object, ..., value) standardGeneric('stock.n<-'))

# stock.wt
#' @rdname accessors
#' @aliases stock.wt stock.wt<-
setGeneric('stock.wt', function(object, ...) standardGeneric('stock.wt'))
setGeneric('stock.wt<-', function(object, ..., value) standardGeneric('stock.wt<-'))

# m.spwn
#' @rdname accessors
#' @aliases m.spwn m.spwn<-
setGeneric('m.spwn', function(object, ...) standardGeneric('m.spwn'))
setGeneric('m.spwn<-', function(object, ..., value) standardGeneric('m.spwn<-'))

# harvest
#' @rdname accessors
#' @aliases harvest harvest<-
setGeneric('harvest', function(object, catch, ...) standardGeneric('harvest'))
setGeneric('harvest<-', function(object, ..., value) standardGeneric('harvest<-'))

# harvest.spwn
#' @rdname accessors
#' @aliases harvest.spwn harvest.spwn<-
setGeneric('harvest.spwn', function(object, ...) standardGeneric('harvest.spwn'))
setGeneric('harvest.spwn<-', function(object, ..., value) standardGeneric('harvest.spwn<-'))

# mat
#' @rdname accessors
#' @aliases mat mat<-
setGeneric('mat', function(object, ...) standardGeneric('mat'))
setGeneric('mat<-', function(object, ..., value) standardGeneric('mat<-'))

# n
#' @rdname accessors
#' @aliases n n<-
setGeneric('n', function(object, ...) standardGeneric('n'))
setGeneric('n<-', function(object, ..., value) standardGeneric('n<-'))

# m
#' @rdname accessors
#' @aliases m m<-
setGeneric('m', function(object, ...) standardGeneric('m'))
setGeneric('m<-', function(object, ..., value) standardGeneric('m<-'))

# wt
#' @rdname accessors
#' @aliases wt wt<-
setGeneric('wt', function(object, ...) standardGeneric('wt'))
setGeneric('wt<-', function(object, ..., value) standardGeneric('wt<-'))

# fec
#' @rdname accessors
#' @aliases fec fec<-
setGeneric('fec', function(object, ...) standardGeneric('fec'))
setGeneric('fec<-', function(object, ..., value) standardGeneric('fec<-'))

# spwn
#' @rdname accessors
#' @aliases spwn spwn<-
setGeneric('spwn', function(object, ...) standardGeneric('spwn'))
setGeneric('spwn<-', function(object, ..., value) standardGeneric('spwn<-'))

# effort
#' @rdname accessors
#' @aliases effort effort<-
setGeneric("effort", function(object, metier, ...) standardGeneric("effort"))
setGeneric("effort<-", function(object, ..., value) standardGeneric("effort<-"))

# type
#' @rdname accessors
#' @aliases type type<-
setGeneric('type', function(object, ...) standardGeneric('type'))
setGeneric('type<-', function(object, ..., value) standardGeneric('type<-'))

# distr
#' @rdname accessors
#' @aliases distr distr<-
setGeneric('distr', function(object, ...) standardGeneric('distr'))
setGeneric('distr<-', function(object, ..., value) standardGeneric('distr<-'))

# distribution
#' @rdname accessors
#' @aliases distribution distribution<-
setGeneric('distribution', function(object, ...)
		standardGeneric('distribution'))
setGeneric('distribution<-', function(object, ..., value)
		standardGeneric('distribution<-'))

# index
#' @rdname accessors
#' @aliases index index<-
setGeneric('index', function(object, ...)
		standardGeneric('index'))
setGeneric('index<-', function(object, ..., value)
		standardGeneric('index<-'))

# index.var
#' @rdname accessors
#' @aliases index.var index.var<-
setGeneric('index.var', function(object, ...)
		standardGeneric('index.var'))
setGeneric('index.var<-', function(object, ..., value)
		standardGeneric('index.var<-'))

# catch.n
#' @rdname accessors
#' @aliases catch.n catch.n<-
setGeneric('catch.n', function(object, ...)
		standardGeneric('catch.n'))
setGeneric('catch.n<-', function(object, ..., value)
		standardGeneric('catch.n<-'))

# catch.wt
#' @rdname accessors
#' @aliases catch.wt catch.wt<-
setGeneric('catch.wt', function(object, ...)
		standardGeneric('catch.wt'))
setGeneric('catch.wt<-', function(object, ..., value)
		standardGeneric('catch.wt<-'))

# sel.pattern
#' @rdname accessors
#' @aliases sel.pattern sel.pattern<-
setGeneric('sel.pattern', function(object, ...)
		standardGeneric('sel.pattern'))
setGeneric('sel.pattern<-', function(object, ..., value)
		standardGeneric('sel.pattern<-'))

# index.q
#' @rdname accessors
#' @aliases index.q index.q<-
setGeneric('index.q', function(object, ...)
		standardGeneric('index.q'))
setGeneric('index.q<-', function(object, ..., value)
		standardGeneric('index.q<-'))

# model
#' @rdname accessors
#' @aliases model model<-
setGeneric('model', function(object, ...)
		standardGeneric('model'))
setGeneric('model<-', function(object, ..., value)
		standardGeneric('model<-'))

# logl
#' @rdname accessors
#' @aliases logl logl<-
setGeneric('logl', function(object, ...)
		standardGeneric('logl'))
setGeneric('logl<-', function(object, ..., value)
		standardGeneric('logl<-'))

# gr
#' @rdname accessors
#' @aliases gr gr<-
setGeneric('gr', function(object, ...)
		standardGeneric('gr'))
setGeneric('gr<-', function(object, ..., value)
		standardGeneric('gr<-'))

# initial
#' @rdname accessors
#' @aliases initial initial<-
setGeneric('initial', function(object, ...)
		standardGeneric('initial'))
setGeneric('initial<-', function(object, ..., value)
		standardGeneric('initial<-'))

# logLik
#' @rdname accessors
#' @aliases logLik logLik<-
setGeneric('logLik', function(object, ...)
		standardGeneric('logLik'))
setGeneric('logLik<-', function(object, ..., value)
		standardGeneric('logLik<-'))

# vcov
#' @rdname accessors
#' @aliases vcov vcov<-
setGeneric('vcov<-', function(object, ..., value)
		standardGeneric('vcov<-'))

# hessian
#' @rdname accessors
#' @aliases hessian hessian<-
setGeneric('hessian', function(object, ...)
		standardGeneric('hessian'))
setGeneric('hessian<-', function(object, ..., value)
		standardGeneric('hessian<-'))

# logerror
#' @rdname accessors
#' @aliases logerror logerror<-
setGeneric('logerror', function(object, ...)
		standardGeneric('logerror'))
setGeneric('logerror<-', function(object, ..., value)
		standardGeneric('logerror<-'))

# details
#' @rdname accessors
#' @aliases details details<-
setGeneric('details', function(object, ...)
		standardGeneric('details'))
setGeneric('details<-', function(object, ..., value)
		standardGeneric('details<-'))

# residuals
#' @rdname accessors
#' @aliases residuals residuals<-
setGeneric('residuals', function(object, ...)
		standardGeneric('residuals'))
setGeneric('residuals<-', function(object, ..., value)
		standardGeneric('residuals<-'))

# fitted
#' @rdname accessors
#' @aliases fitted fitted<-
setGeneric('fitted', function(object, ...)
		standardGeneric('fitted'))
setGeneric('fitted<-', function(object, ..., value)
		standardGeneric('fitted<-'))

# rec
#' @rdname accessors
#' @aliases rec rec<-
setGeneric('rec', function(object, ...)
		standardGeneric('rec'))
setGeneric('rec<-', function(object, ..., value)
		standardGeneric('rec<-'))

# rec.obs
#' @rdname accessors
#' @aliases rec.obs rec.obs<-
setGeneric('rec.obs', function(object, ...)
		standardGeneric('rec.obs'))

# catch.q
#' @rdname accessors
#' @aliases catch.q catch.q<-
setGeneric('catch.q', function(object, ...)
		standardGeneric('catch.q'))
setGeneric('catch.q<-', function(object, ..., value)
		standardGeneric('catch.q<-'))

# discards.sel
#' @rdname accessors
#' @aliases discards.sel discards.sel<-
setGeneric('discards.sel', function(object, ...)
		standardGeneric('discards.sel'))
setGeneric('discards.sel<-', function(object, ..., value)
		standardGeneric('discards.sel<-'))

# landings.sel
#' @rdname accessors
#' @aliases landings.sel landings.sel<-
setGeneric('landings.sel', function(object, ...)
		standardGeneric('landings.sel'))
setGeneric('landings.sel<-', function(object, ..., value)
		standardGeneric('landings.sel<-'))

# params, params<-
#' @rdname accessors
#' @aliases params, params<- params, params<-<-
setGeneric("params", function(object, ...)
  standardGeneric("params"))
setGeneric("params<-", function(object, value)
  standardGeneric("params<-"))

# }}}

# -- METHODS

# quant, quant<- {{{
#' Method quant
#' 
#' Function to get or set the name of the first dimension (quant) in an object
#' of any FLArray-based class, like \code{\link{FLQuant}} or \code{\link{FLCohort}}.
#'
#' @name quant
#' @rdname quant
#' @aliases quant quant-methods
#' @docType methods
#' @section Generic function: quant(object) quant<-(object,value)
#' @author The FLR Team
#' @seealso \linkS4class{FLQuant}, \linkS4class{FLCohort}
#' @keywords methods
#' @examples
#' 
#' # quant is 'quant' by default
#'   quant(FLQuant())
#'
#' flq <- FLQuant(rnorm(80), dim=c(4,20), quant='age')
#' quant(flq)
#' quant(flq) <- 'length'
#' summary(flq)
#'

setGeneric("quant", function(object, ...)
  standardGeneric("quant"))
setGeneric("quant<-", function(object, value)
  standardGeneric("quant<-")) # }}}

# iter, iter<- {{{

#' Methods iter
#'
#' Select or modify iterations of an FLR object
#' 
#' To extract or modify a subset of the iterations contained in an FLR object,
#' the \code{iter} and \code{iter<-} methods can be used.
#' 
#' In complex objects with various \code{FLQuant} slots, the \code{iter} method
#' checks whether individual slots contain more than one iteration, i.e.
#' \code{dims(object)[6] > 1}. If a particular slot contains a single
#' iteration, that is returned, otherwise the chosen iteration is selected.
#' This is in contrast with the subset operator \code{[}, which does not carry
#' out this check.
#' 
#' For objects of class \code{\link{FLModel}}, iters are extracted for slots of
#' classes \code{FLQuant}, \code{FLCohort} and \code{FLPar}.
#'
#' @name iter
#' @aliases iter iter-methods 
#' @docType methods
#' @section Generic function: iter(object) iter<-(object,value)
#' @author The FLR Team
#' @seealso \linkS4class{FLComp}, \linkS4class{FLQuant}
#' @keywords methods

#setGeneric("iter", function(obj, ...)
#	standardGeneric("iter"))
setGeneric("iter", useAsDefault = iterators::iter)
setGeneric("iter<-", function(object, ..., value)
  standardGeneric("iter<-")) # }}}

# lower & upper {{{

#' Extract and modify the *lower* and *upper* FLModel attibutes.
#'
#' Description: Lorem ipsum dolor sit amet, consectetur adipiscing elit.
#'
#' Details: Aliquam sagittis feugiat felis eget consequat.
#'
#' @param object Object to extract from or modify
#' @param value New value
#' @param ... Other arguments
#'
#' @return RETURN Lorem ipsum dolor sit amet
#'
#' @name upperlower
#' @rdname upperlower-methods
#' @aliases upper upper-methods upper<- upper<--methods
#' @aliases lower lower-methods lower<- lower<--methods
#'
#' @author The FLR Team
#' @seealso [`FLModel`]
#' @keywords methods
#' @md

setGeneric("lower", function(object, ...)
  standardGeneric("lower"))
setGeneric("lower<-", function(object, ..., value)
    standardGeneric("lower<-"))

setGeneric("upper", function(object, ...)
  standardGeneric("upper"))
setGeneric("upper<-", function(object, ..., value)
  standardGeneric("upper<-"))
# }}}

# ssb {{{

#' Calculate or return the Spawning Stock Biomass
#'
#' The calculated Spawning Stock Biomass (SSB) of a fish population is returned
#' by this method. SSB is the combined weight of all individuals in a fish stock
#' that are capable of reproducing. In some classes this is calculated from
#' information stored in different slots, while in others \code{ssb()} is simply
#' an slot accessor. When the later is the case, the corresponding replacement
#' method also exists.
#'
#' @param object Object on which \code{ssb} is calculated or extracted.
#'
#' @return An object, generally of class \code{\link{FLQuant}}.
#'
#' @name ssb
#' @rdname ssb
#' @aliases ssb ssb-methods
#'
#' @author The FLR Team
#' @seealso \link{FLComp}
#' @keywords methods
setGeneric('ssb', function(object, ...)
		standardGeneric('ssb'))
setGeneric('ssb<-', function(object, ..., value)
		standardGeneric('ssb<-')) # }}}

setGeneric('ssf', function(object, ...)
		standardGeneric('ssf')) # }}}

# dims {{{

#' Method dims
#'
#' List with information on object dimensions
#' 
#' Method \code{dims} returns a named list with information on the dimensions
#' and dimension names of a given object. The list returned could be
#' extended in the future and currently contains, depending on the class of the
#' object, some of the following:
#' \describe{
#'   \item{quant}{Length of the first dimension}
#'   \item{min}{First quant}
#'   \item{max}{Last quant}
#'   \item{year}{Number of years}
#'   \item{minyear}{First year in series}
#'   \item{maxyear}{Last year in series}
#'   \item{cohort}{Number of cohorts}
#'   \item{mincohort}{First cohort in series}
#'   \item{maxcohort}{Last cohort in series}
#'   \item{unit}{Length of the third (\code{unit}) dimension}
#'   \item{season}{Length of the fourth (\code{season}) dimension}
#'   \item{area}{Length of the fifth (\code{area}) dimension}
#'   \item{iter}{Length of the sixth (\code{iter}) dimension} }
#' Values in the returned list are of class \code{numeric}, unless dimnames are
#' strings with no numeric translation, in which case the result is \code{NA}.
#' 
#' Please note that the name of the first element in the returned list changes
#' with the name of the first dimension in the input object. Use
#' \code{\link{quant}} to obtain the name and extract the relevant element from
#' the result list.
#'
#' @name dims
#' @aliases dims dims-methods
#' @docType methods
#' @section Generic function: dims(obj)
#' @author The FLR Team
#' @seealso \code{\link[base]{dimnames}}, \code{\link{FLQuant}}
#' @keywords methods

setGeneric("dims", function(obj, ...) standardGeneric("dims")) # }}}

# compute {{{

#' Methods to compute quantities
#'
#' Methods to compute total quant-aggregated catch, landings, discards and
#' stock biomass from age or length-structured numbers and mean weights.
#' 
#' These methods compute the total catch, landings, discards and stock biomass
#' from the quant-structured values in numbers and weight per individual. The
#' calculation for landings, discards and stock involves the product of the
#' landings/discards/stock in numbers (\code{landings.n}, \code{discards.n} or
#' \code{stock.n}) by the individual weight-at-quant (\code{landings.wt},
#' \code{discards.wt} or \code{stock.wt}), as in
#' 
#' \deqn{L=L_n * L_{wt}}{landings = landings.n * landings.wt}
#' 
#' By selecting \code{slot="catch"}, \code{computeCatch} can calculate in the
#' same way the total catch from the catch-at-quant and weight in the catch.
#' Those two values (in slots \code{catch.n} and \code{catch.wt}) can also be
#' calculated (from landings and discards) by specifying \code{slot="n"} and
#' \code{slot="wt"} respectively. Calling \code{computeCatch} with option
#' \code{slot="all"} will carry out the three calculations. In this case, the
#' returned object will be of class \code{\link{FLQuants}}, with element names
#' \code{catch}, \code{catch.n} and \code{catch.wt}, which can then be passed
#' directly to the \code{\link{catch<-}} replacement method.
#' 
#' @name compute
#' @docType methods
#' @section Generic function:
#' computeCatch(object, ...)
#' 
#' computeLandings(object, ...)
#' 
#' computeDiscards(object, ...)
#' 
#' computeStock(object, ...)
#'
#' @author The FLR Team
#' @seealso \link{FLComp}
#' @keywords methods
#' @examples
#' 
#' data(ple4)
#' summary(computeLandings(ple4))
#' summary(computeCatch(ple4, slot="all"))
#' stock(ple4) <- computeStock(ple4)
#' landings(ple4) <- computeLandings(ple4)
#' catch.n(ple4) <- computeCatch(ple4, slot="n")
#' catch(ple4) <- computeCatch(ple4, slot="all")
#'


#' @rdname compute
#' @aliases computeLandings computeLandings-methods
setGeneric("computeLandings", function(object, ...)
		standardGeneric("computeLandings"))

#' @rdname compute
#' @aliases computeDiscards computeDiscards-methods
	setGeneric("computeDiscards", function(object, ...)
		standardGeneric("computeDiscards"))

#' @rdname compute
#' @aliases computeLandings computeCatch-methods
	setGeneric("computeCatch", function(object, ...)
		standardGeneric("computeCatch"))

#' @rdname compute
#' @aliases computeStock computeStock-methods
setGeneric("computeStock", function(object, ...)
		standardGeneric("computeStock"))

# }}}

# tsb
	setGeneric("tsb", function(object, ...)
		standardGeneric("tsb"))

# fbar
	setGeneric("fbar", function(object, ...)
		standardGeneric("fbar"))

# ssbpurec
	setGeneric("ssbpurec", function(object, ...)
		standardGeneric("ssbpurec"))

# meanLifespan
setGeneric("meanLifespan", function(x, ...)
	standardGeneric("meanLifespan"))

# dims {{{

#' Method dims
#'
#' List with information on object dimensions
#' 
#' Method \code{dims} returns a named list with information on the dimensions
#' and dimension names of a given object. The list returned could be
#' extended in the future and currently contains, depending on the class of the
#' object, some of the following:
#' \describe{
#'   \item{quant}{Length of the first dimension}
#'   \item{min}{First quant}
#'   \item{max}{Last quant}
#'   \item{year}{Number of years}
#'   \item{minyear}{First year in series}
#'   \item{maxyear}{Last year in series}
#'   \item{cohort}{Number of cohorts}
#'   \item{mincohort}{First cohort in series}
#'   \item{maxcohort}{Last cohort in series}
#'   \item{unit}{Length of the third (\code{unit}) dimension}
#'   \item{season}{Length of the fourth (\code{season}) dimension}
#'   \item{area}{Length of the fifth (\code{area}) dimension}
#'   \item{iter}{Length of the sixth (\code{iter}) dimension} }
#' Values in the returned list are of class \code{numeric}, unless dimnames are
#' strings with no numeric translation, in which case the result is \code{NA}.
#' 
#' Please note that the name of the first element in the returned list changes
#' with the name of the first dimension in the input object. Use
#' \code{\link{quant}} to obtain the name and extract the relevant element from
#' the result list.
#'
#' @name dims
#' @aliases dims dims-methods
#' @docType methods
#' @section Generic function: dims(obj)
#' @author The FLR Team
#' @seealso \code{\link[base]{dimnames}}, \code{\link{FLQuant}}
#' @keywords methods

setGeneric("dims", function(obj, ...) standardGeneric("dims")) # }}}

# compute {{{

#' Methods to compute quantities
#'
#' Methods to compute total quant-aggregated catch, landings, discards and
#' stock biomass from age or length-structured numbers and mean weights.
#' 
#' These methods compute the total catch, landings, discards and stock biomass
#' from the quant-structured values in numbers and weight per individual. The
#' calculation for landings, discards and stock involves the product of the
#' landings/discards/stock in numbers (\code{landings.n}, \code{discards.n} or
#' \code{stock.n}) by the individual weight-at-quant (\code{landings.wt},
#' \code{discards.wt} or \code{stock.wt}), as in
#' 
#' \deqn{L=L_n * L_{wt}}{landings = landings.n * landings.wt}
#' 
#' By selecting \code{slot="catch"}, \code{computeCatch} can calculate in the
#' same way the total catch from the catch-at-quant and weight in the catch.
#' Those two values (in slots \code{catch.n} and \code{catch.wt}) can also be
#' calculated (from landings and discards) by specifying \code{slot="n"} and
#' \code{slot="wt"} respectively. Calling \code{computeCatch} with option
#' \code{slot="all"} will carry out the three calculations. In this case, the
#' returned object will be of class \code{\link{FLQuants}}, with element names
#' \code{catch}, \code{catch.n} and \code{catch.wt}, which can then be passed
#' directly to the \code{\link{catch<-}} replacement method.
#' 
#' @name compute
#' @docType methods
#' @section Generic function:
#' computeCatch(object, ...)
#' 
#' computeLandings(object, ...)
#' 
#' computeDiscards(object, ...)
#' 
#' computeStock(object, ...)
#'
#' @author The FLR Team
#' @seealso \link{FLComp}
#' @keywords methods
#' @examples
#' 
#' data(ple4)
#' summary(computeLandings(ple4))
#' summary(computeCatch(ple4, slot="all"))
#' stock(ple4) <- computeStock(ple4)
#' landings(ple4) <- computeLandings(ple4)
#' catch.n(ple4) <- computeCatch(ple4, slot="n")
#' catch(ple4) <- computeCatch(ple4, slot="all")
#'


#' @rdname compute
#' @aliases computeLandings computeLandings-methods
setGeneric("computeLandings", function(object, ...)
		standardGeneric("computeLandings"))

#' @rdname compute
#' @aliases computeDiscards computeDiscards-methods
	setGeneric("computeDiscards", function(object, ...)
		standardGeneric("computeDiscards"))

#' @rdname compute
#' @aliases computeLandings computeCatch-methods
	setGeneric("computeCatch", function(object, ...)
		standardGeneric("computeCatch"))

#' @rdname compute
#' @aliases computeStock computeStock-methods
setGeneric("computeStock", function(object, ...)
		standardGeneric("computeStock"))

# }}}

# tsb
	setGeneric("tsb", function(object, ...)
		standardGeneric("tsb"))

# fbar
	setGeneric("fbar", function(object, ...)
		standardGeneric("fbar"))

# ssbpurec
	setGeneric("ssbpurec", function(object, ...)
		standardGeneric("ssbpurec"))

# meanLifespan
setGeneric("meanLifespan", function(x, ...)
	standardGeneric("meanLifespan"))

# ssn
setGeneric("ssn", function(object, ...)
  standardGeneric("ssn"))

# leslie
setGeneric("leslie", function(object, ...)
	standardGeneric("leslie"))

# r
setGeneric("r", function(m, fec, ...)
  standardGeneric("r"))

# survprob
setGeneric("survprob", function(object, ...)
  standardGeneric("survprob"))

# covar
setGeneric('covar', function(object, ...)
		standardGeneric('covar'))
setGeneric('covar<-', function(object, ..., value)
		standardGeneric('covar<-'))

# spr0
setGeneric("spr0", function(ssb, rec, fbar, ...)
	  standardGeneric("spr0"))

# ab
setGeneric('ab', function(object, ...)
		standardGeneric('ab'))

# trim {{{

#' Method trim
#'
#' Trim FLR objects using named dimensions
#' 
#' Subsetting of FLR objects can be carried out with dimension names by using
#' \code{trim}. A number of dimension names and selected dimensions are passed
#' to the method and those are used to subset the input object.
#' 
#' Exceptions are made for those classes where certain slots might differ in
#' one or more dimensions. If trim is applied to an FLQuant object of length 1
#' in its first dimension and with dimension name equal to 'all', values to
#' \code{trim} specified for that dimension will be ignored. For example,
#' \code{\link{FLStock}} objects contain slots with length=1 in their first
#' dimension. Specifying values to trim over the first dimension will have no
#' effect on those slots (\code{catch}, \code{landings}, \code{discards}, and
#' \code{stock}). Calculations might need to be carried out to recalculate
#' those slots (e.g. using \code{computeCatch}, \code{computeLandings},
#' \code{computeDiscards} and \code{computeStock}) if their quant-structured
#' counterparts are modified along the first dimension.
#'
#' @name trim
#' @rdname trim
#' @aliases trim trim-methods
#' @docType methods
#' @section Generic function: trim(x)
#' @author The FLR Team
#' @seealso \linkS4class{FLQuant}, \linkS4class{FLStock},
#' \linkS4class{FLCohort}, \linkS4class{FLIndex}
#' @keywords methods

setGeneric("trim", function(x, ...)
	standardGeneric("trim")) # }}}

# catch.sel
setGeneric('catch.sel', function(object, ...)
		standardGeneric('catch.sel'))

# ccplot
setGeneric("ccplot", function(x, data, ...)
	standardGeneric("ccplot"))

# qapply
setGeneric("qapply", function(X, FUN, ...) standardGeneric("qapply"))

# mcf
setGeneric("mcf", function(object, ...)
	standardGeneric("mcf"))

# autoParscale
setGeneric("autoParscale", function(object, ...)
  standardGeneric("autoParscale"))

# sigma
setGeneric("sigma", function(object, ...)
  standardGeneric("sigma"))

# gradient
setGeneric("gradient", function(func, x, ...)
  standardGeneric("gradient"))

# surface
setGeneric("surface", function(fitted, ...)
  standardGeneric("surface"))

# parscale
setGeneric("parscale", function(object, ...)
  standardGeneric("parscale"))

# computeHessian
setGeneric("computeHessian", function(object, ...)
  standardGeneric("computeHessian"))

# computeD
setGeneric("computeD", function(object, ...)
  standardGeneric("computeD"))

# loglAR1
setGeneric("loglAR1", function(obs, hat, ...)
  standardGeneric("loglAR1"))

# rSq
setGeneric("rSq", function(obs, hat, ...)
  standardGeneric("rSq"))

# ab
setGeneric("ab", function(x, model, ...)
  standardGeneric("ab"))

# sv
setGeneric("sv", function(x, model, ...)
  standardGeneric("sv"))

# fapex
setGeneric("fapex", function(x, ...)
  standardGeneric("fapex"))

# qmax
setGeneric("qmax", function(x, ...)
  standardGeneric("qmax"))

# qmin
setGeneric("qmin", function(x, ...)
  standardGeneric("qmin"))

# sp
setGeneric('sp', function(stock, catch, harvest, ...)
		standardGeneric('sp'))

# propagate {{{

#' Method propagate
#' 
#' Methods to extend objects of various FLR classes along the `iter`
#' (6th FLQuant) dimension. Objects must generally have a single
#' `iter` to be extended. The new iterations can be filled with copies
#' of the existing, or remain as `NA`.
#'
#' @name propagate
#' @rdname propagate
#' @aliases propagate propagate-methods
#' @docType methods
#' @section Generic function: propagate(object, ...)
#' @author The FLR Team
#' @seealso \linkS4class{FLQuant}
#' @keywords methods

setGeneric("propagate", function(object, ...)
    standardGeneric("propagate")) # }}}

# Sums
setGeneric('Sums', function(object, ...)
		standardGeneric('Sums'))

# Products
setGeneric('Products', function(object, ...)
		standardGeneric('Products'))

# catches
setGeneric('catches', function(object, ...)
		standardGeneric('catches'))
setGeneric('catches<-', function(object, catch, ..., value)
		standardGeneric('catches<-'))

# mass
setGeneric('mass', function(object, ...)
  standardGeneric('mass'))

# readMFCL
setGeneric("readMFCL", function(file, ...)
  standardGeneric("readMFCL"))

# as.FLQuant
	setGeneric("as.FLQuant", function(x, ...)
		standardGeneric("as.FLQuant"))

# jackknife
setGeneric("jacknife", function(object, ...)
	standardGeneric("jacknife"))

# jackSummary
setGeneric("jackSummary", function(object, ...)
	standardGeneric("jackSummary"))

# catchSel
setGeneric("catchSel", function(object, ...){
	standardGeneric("catchSel")})

# %+%
#' @rdname operators
#' @aliases %+% % %+%-methods
setGeneric("%+%", function(e1, e2)
  standardGeneric("%+%"))

# %-%
#' @rdname operators
#' @aliases %-% %-%-methods
setGeneric("%-%", function(x, y)
  standardGeneric("%-%"))

# %^%
#' @rdname operators
#' @aliases %^% %^%-methods
setGeneric("%^%", function(x, y)
  standardGeneric("%^%"))

# bubbles
setGeneric("bubbles", function(x, data, ...)
    standardGeneric("bubbles"))

# readASPIC
setGeneric("readASPIC", function(x, type, scen, ...)
  standardGeneric("readASPIC"))

# distribution
setGeneric("distribution", function(object, ...)
	standardGeneric("distribution"))
setGeneric("distribution<-", function(object, ..., value)
	standardGeneric("distribution<-"))

# combine
setGeneric('combine', function(x, y, ...)
  standardGeneric('combine'))

# jackknife
setGeneric("jackknife", function(object, ...)
	standardGeneric("jackknife"))

# orig
setGeneric("orig", function(object, ...)
	standardGeneric("orig"))

# cv
setGeneric("cv", function(x, ...)
	standardGeneric("cv"))

# computeLogLik
setGeneric("computeLogLik", function(object, ...)
  standardGeneric("computeLogLik"))

# fmle
setGeneric('fmle', function(object, start, ...)
    standardGeneric('fmle'))

# quantTotals {{{

#' Methods quantTotals
#' 
#' Methods to compute totals over selected dimensions of \code{FLQuant} objects
#' These methods return an object of same dimensions as the input but with the
#' sums along the first (\code{yearTotals}) or second dimension
#' (\code{quantTotals}). Although the names might appear contradictory, it must
#' be noted that what each method really returns are the totals over the
#' selected dimension.
#' 
#' 
#' @name quantTotals
#' @aliases quantTotals quantTotals-methods yearTotals yearTotals-methods
#' @docType methods
#' @section Generic function: quantTotals(x)
#' 
#' yearTotals(x)
#' @author The FLR Team
#' @seealso \link{FLQuant}
#' @keywords methods
#' @examples
#' 
#' flq <- FLQuant(rlnorm(100), dim=c(10,10))
#' quantTotals(flq)
#' # See how the values obtained by yearSums are being replicated
#'   yearSums(flq)
#' # Get the proportions by quant
#'   flq / quantTotals(flq)
#' # or year
#'   flq / yearTotals(flq)
# quantTotals
setGeneric("quantTotals", function(x, ...)
    standardGeneric("quantTotals"))
# yearTotals
setGeneric("yearTotals", function(x, ...)
    standardGeneric("yearTotals"))

# }}}

# dim Sums, Means, Medians & CVs {{{

#' Summaries by dimension
#' 
#' Methods to compute various summary calculations (sum, mean, variance) over
#' selected dimensions of objects from any array-based classes
#' (e.g. \code{FLQuant}). These methods return an object of the
#' same dimensions as the input but with length one in the dimension chosen
#' to operate along.
#'
#' This set of methods computes three different summaries (sum, mean and
#' variance) of an \code{FLQuant} object along each of the six dimensions
#' (quant, year, unit, season, area, or iter). Medians and CVs can also be
#' computed along the sixth dimension, \code{iter}.
#' 
#' These methods encapsulate a call to \code{\link[base]{apply}} with
#' the corresponding dimensions and function: \code{\link[base]{mean}}, 
#' \code{\link[stats]{median}}, \code{\link[stats]{var}}, and
#' \code{\link[base]{sum}}, while \code{iterCVs} are computed as
#' \code{sqrt(iterVars) / iterMeans}.
#'
#' In contrast with R standard behaviour, the sum of a dimension where all
#' elements are \code{NA} will be \code{NA} and not 0. See example below.
#' 
#' Methods working along the iter dimension are also defined for objects of class
#' \code{FLPar}.
#' 
#' Methods to operate over the first dimension refer to it as the \code{quant}
#' dimension, regardless of the actual name used in the object.
#' 
#' @name dimSummaries
#' @docType methods
#' @aliases quantSums quantSums-methods quantSums,FLQuant-method
#' @section Generic methods:
#' quantSums(x), quantMeans(x), quantVars(x)
#' yearSums(x), yearMeans(x), yearVars(x)
#' unitSums(x), unitMeans(x), unitVars(x)
#' seasonSums(x), seasonMeans(x), seasonVars(x)
#' areaSums(x), areaMeans(x), areaVars(x)
#' iterMeans(x), iterVars(x), iterMedians(x), iterSums(x)
#' dimSums(x), dimMeans(x), dimVars(x)
#' @param x An object.
#' @param na.rm Should NAs be removed before calculation? Defaults to TRUE.
#' @author The FLR Team
#' @seealso \link{FLQuant}, \link[base]{sum}, \link[base]{mean},
#' \link[stats]{var}
#' @keywords methods
#' @examples
#' 
#' flq <- FLQuant(rnorm(4000), dim=c(5,10,2,2,2,10), quant='age')
#'
#' quantSums(flq)
#' quantMeans(flq)
#' yearSums(flq)
#' iterMeans(flq)
#' dim(quantSums(flq))
#'
#' # NA dims stay as NA when summed along
#' x <- FLQuant(c(NA, NA, NA, rnorm(6)), dim=c(3, 3))
#' quantSums(x)
#' # although in fact a sum of no elements (as na.rm=TRUE) is zero
#' apply(x, 2:6, sum, na.rm=TRUE)

#' @rdname dimSummaries
setGeneric("quantSums", function(x, ...) standardGeneric("quantSums"))
#' @rdname dimSummaries
setGeneric("yearSums", function(x, ...) standardGeneric("yearSums"))
#' @rdname dimSummaries
setGeneric("unitSums", function(x, ...) standardGeneric("unitSums"))
#' @rdname dimSummaries
setGeneric("seasonSums", function(x, ...) standardGeneric("seasonSums"))
#' @rdname dimSummaries
setGeneric("areaSums", function(x, ...) standardGeneric("areaSums"))
#' @rdname dimSummaries
setGeneric("iterSums", function(x, ...) standardGeneric("iterSums"))
#' @rdname dimSummaries
setGeneric("dimSums", function(x, ...) standardGeneric("dimSums"))

#' @rdname dimSummaries
setGeneric("quantMeans", function(x, ...) standardGeneric("quantMeans"))
#' @rdname dimSummaries
setGeneric("yearMeans", function(x, ...) standardGeneric("yearMeans"))
#' @rdname dimSummaries
setGeneric("unitMeans", function(x, ...) standardGeneric("unitMeans"))
#' @rdname dimSummaries
setGeneric("seasonMeans", function(x, ...) standardGeneric("seasonMeans"))
#' @rdname dimSummaries
setGeneric("areaMeans", function(x, ...) standardGeneric("areaMeans"))
#' @rdname dimSummaries
setGeneric("iterMeans", function(x, ...) standardGeneric("iterMeans"))
#' @rdname dimSummaries
setGeneric("dimMeans", function(x, ...) standardGeneric("dimMeans"))

#' @rdname dimSummaries
setGeneric("quantVars", function(x, ...) standardGeneric("quantVars"))
#' @rdname dimSummaries
setGeneric("yearVars", function(x, ...) standardGeneric("yearVars"))
#' @rdname dimSummaries
setGeneric("unitVars", function(x, ...) standardGeneric("unitVars"))
#' @rdname dimSummaries
setGeneric("seasonVars", function(x, ...) standardGeneric("seasonVars"))
#' @rdname dimSummaries
setGeneric("areaVars", function(x, ...) standardGeneric("areaVars"))
#' @rdname dimSummaries
setGeneric("iterVars", function(x, ...) standardGeneric("iterVars"))
#' @rdname dimSummaries
setGeneric("dimVars", function(x, ...) standardGeneric("dimVars"))

#' @rdname dimSummaries
setGeneric("iterMedians", function(x, ...) standardGeneric("iterMedians"))
#' @rdname dimSummaries
setGeneric("iterCVs", function(x, ...) standardGeneric("iterCVs"))

#' @rdname dimSummaries
setGeneric("iterProb", function(x, ...) standardGeneric("iterProb"))
# }}}

# z {{{

#' Total mortality z
#'
#' Returns the calculation of total mortality, *z*, usually as the sum of
#' fishing mortality, *f*, and natural mortality, *m*.
#'
#' @param object Object to calculate on.
#'
#' @return An object of the corresponding class, usually *FLQuant*.
#'
#' @name z
#' @rdname z-methods
#' @aliases z z-methods
#'
#' @author The FLR Team
#' @seealso [`FLQuant`]
#' @keywords methods
#' @md


setGeneric("z", function(object, ...)
    standardGeneric("z"))
# }}}

# breaks
setGeneric("breaks", function(object, ...) standardGeneric("breaks"))

# halfwidth
setGeneric("halfwidth", function(object, ...) standardGeneric("halfwidth"))
setGeneric("halfwidth<-", function(object, ..., value) standardGeneric("halfwidth<-"))

# leftbound
setGeneric("leftbound", function(object, ...) standardGeneric("leftbound"))

# rightbound
setGeneric("rightbound", function(object, ...) standardGeneric("rightbound"))

# mids
setGeneric("mids", function(object, ...) standardGeneric("mids"))

# vecs and rngs
setGeneric("rngyear", function(object, ...) standardGeneric("rngyear"))
setGeneric("rngyear<-", function(object,value) standardGeneric("rngyear<-"))
setGeneric("rngage", function(object, ...) standardGeneric("rngage"))
setGeneric("rngage<-", function(object,value) standardGeneric("rngage<-"))
setGeneric("vecyear", function(object, ...) standardGeneric("vecyear"))
setGeneric("vecage", function(object, ...) standardGeneric("vecage"))

# wide.frame
setGeneric("wide.frame", function(data, formula, ...) standardGeneric("wide.frame"))

# tS
setGeneric("tS", function(object, step, ...) standardGeneric("tS"))
setGeneric("tS<-", function(object, step, ..., value) standardGeneric("tS<-"))

# sr
setGeneric("sr", function(object, step, ...) standardGeneric("sr"))

# slots
setGeneric("slots", function(object, name, ...) standardGeneric("slots"))

# var<-
setGeneric("var<-", function(x, ..., value) standardGeneric("var<-"))

# mean<-
setGeneric("mean<-", function(x, value) standardGeneric("mean<-"))

# median<-
setGeneric("median<-", function(x, value) standardGeneric("median<-"))

# uppq
setGeneric("uppq", function(x, ...) standardGeneric("uppq"))
setGeneric("uppq<-", function(x, value) standardGeneric("uppq<-"))

# lowq
setGeneric("lowq", function(x, ...) standardGeneric("lowq"))
setGeneric("lowq<-", function(x, value) standardGeneric("lowq<-"))

# getPlural
setGeneric("getPlural", function(object, ...) standardGeneric("getPlural"))

# e, e<-
setGeneric("e", function(x, ...) standardGeneric("e"))
setGeneric("e<-", function(x, value) standardGeneric("e<-"))

# predictModel
setGeneric("predictModel", function(object, model, ...)
  standardGeneric("predictModel"))

# bkey
setGeneric("bkey", function(object, ...)
	standardGeneric("bkey"))

# COERCION

# as.FLStock
	setGeneric("as.FLStock", function(object, ...)
		standardGeneric("as.FLStock"))

# as.FLBiol
setGeneric("as.FLBiol", function(object, ...)
  standardGeneric("as.FLBiol"))

# as.FLindex
setGeneric("as.FLIndex", function(object, ...)
  standardGeneric("as.FLIndex"))

# as.FLSR
setGeneric("as.FLSR", function(object, ...)
  standardGeneric("as.FLSR"))

# GENERICS for methods shared by multiple packages

# vcost
setGeneric("vcost", function(object, ...)
  standardGeneric("vcost"))
setGeneric("vcost<-", function(object, ..., value)
  standardGeneric("vcost<-"))

# fcost
setGeneric("fcost", function(object, ...)
  standardGeneric("fcost"))
setGeneric("fcost<-", function(object, ..., value)
  standardGeneric("fcost<-"))

# cost
setGeneric("cost", function(object, ...)
  standardGeneric("cost"))

# ccost
setGeneric("ccost", function(object, ...)
  standardGeneric("ccost"))

# profit
setGeneric("profit", function(object, ...)
  standardGeneric("profit"))

# price
setGeneric("price", function(object, ...)
  standardGeneric("price"))
setGeneric("price<-", function(object, ..., value)
  standardGeneric("price<-"))

# revenue
setGeneric("revenue", function(object, ...)
  standardGeneric("revenue"))

# fwd
setGeneric("fwd", function(object, fishery, control, ...)
  standardGeneric("fwd"))

# metrics {{{

#' Extract simply-defined metrics from compex objects
#'
#' Time series summaries of complex objects are commonly needed, for example for
#' plotting the inputs and outputs of a class like \code{\link{FLStock}}. These
#' methods allow for simple specification of those metrics by means of function
#' calls and formulas.
#'
#' @param object A complex **FLR** object from which to extract time series metrics.
#'
#' @return An object, generally of class \code{\link{FLQuants}}.
#'
#' @name metrics
#' @rdname metrics
#' @aliases metrics metrics-methods
#'
#' @author The FLR Team
#' @seealso \link{FLComp}
#' @keywords methods
#' @md
setGeneric("metrics", function(object, metrics, ...)
  standardGeneric("metrics")) # }}}

# srparams, srmodel for FLBiolcpp
setGeneric("srmodel", function(object, ...)
  standardGeneric("srmodel"))
setGeneric("srmodel<-", function(object, ..., value)
  standardGeneric("srmodel<-"))

setGeneric("srparams", function(object, ...)
  standardGeneric("srparams"))
setGeneric("srparams<-", function(object, ..., value)
  standardGeneric("srparams<-"))

setGeneric("vb", function(x, sel, ...) standardGeneric("vb"))

setGeneric("bias", function(x) standardGeneric("bias"))

setGeneric("corrected", function(x) standardGeneric("corrected"))

setGeneric("FLQuantJK", function(object, orig) standardGeneric("FLQuantJK"))

setGeneric("FLParJK", function(object, orig) standardGeneric("FLParJK"))

setGeneric("%=%", function(object, value,...) standardGeneric("%=%"))

# msy {{{

#' msy: A series of methods to extract or compute MSY-based reference points
#'
#' Reference points based on equilibirum calculations of Maximum Sustainable
#' Yield (MSY) are computed by various FLR packages. The methods' generics are
#' defined here for convenience. Please refer to the help pages of particular
#' methods for further details
#'
#' @details The four methods provide the following parameter estimates:
#'   * `msy` Maximum Sustainable Yield (MSY)
#'   * `fmsy` Fishing mortality level expected to produce on average MSY
#'   * `bmsy` Total biomass that should produce MSY
#'   * `sbmsy` Spawning biomass that should produce MSY
#'
#' @param x An input object from which to extract or compute a reference point
#'
#' @return A value for the requested reference point, 'FLPar'
#'
#' @name msy
#' @rdname msy-methods
#' @aliases msy msy-methods
#' @md
#'
#' @author The FLR Team
#' @seealso [`FLPar`]
#' @keywords methods

setGeneric("msy", function(x, ...) standardGeneric("msy"))

#' @rdname msy-methods
#' @aliases bmsy bmsy-methods
setGeneric("bmsy", function(x, ...) standardGeneric("bmsy"))

#' @rdname msy-methods
#' @aliases sbmsy sbmsy-methods
setGeneric("sbmsy", function(x, ...) standardGeneric("sbmsy"))

#' @rdname msy-methods
#' @aliases fmsy fmsy-methods
setGeneric("fmsy", function(x, ...) standardGeneric("fmsy")) # }}}

setGeneric("rnoise", function(n, len, ...) standardGeneric("rnoise"))
setGeneric("rlnoise", function(n, len, ...) standardGeneric("rlnoise"))

# slim {{{

#' Drop unnecesary 'iters'
#'
#' Objects of FLR classes can vary in the length along the sixth dimension
#' in any slot of class [FLQuant-class]. This reduces object size and memory
#' usage. If an object has been extended fully, for example by using 
#' \code{\link[=propagate]{propagate}}, we can slim down the object by reducing
#' any slot where all iters are identical and keeping only yhe first *iter*.
#'
#' The test for whether an slot can be slimmed is based on checking if the sum
#' of the variance along the 6th dimensions is equal to zero.
#'
#' @param object A complex **FLR** object to slim down.
#'
#' @return An object of the same class as the input.
#'
#' @name slim
#' @rdname slim
#' @aliases slim slim-methods
#'
#' @author The FLR Team
#' @seealso [FLQuant-class] \code{\link[=propagate]{propagate}}
#' @keywords methods
#' @md

setGeneric("slim", function(object, ...) standardGeneric("slim")) # }}}

# simplify {{{

#' Aggregate or select along unwanted dimensions
#'
#' Objects of many FLR classes might be aggregated along the "unit", "season",
#' and/or "area" dimensions according to the type of data they contain.
#' @param object A complex **FLR** object to aggregate.
#'
#' @return An object of the same class as the input.
#'
#' @name simplify
#' @rdname simplify
#' @aliases simplify simplify-methods
#'
#' @author The FLR Team
#' @keywords methods
#' @md

setGeneric("simplify", function(object, ...) standardGeneric("simplify")) # }}}

# verify {{{

#' Verify FLR objects
#'
#' Verifies the content of FLR objects according to a set of rules
#'
#' Classes' validity functions generally check the structure and dimensions of
#' objects and their component slots. But some checks on the data content of
#' objects is often required. The various verify methods implement both a system
#' to create *rules* that an object is expected to pass, and a minimum standard
#' set of rules for each defined class
#'
#' The data.frame output by the method when `report=TRUE` contains one row per
#' rule and the following columns:
#' - *name*, the rule name
#' - *items*, number of comparisons carried out
#' - *passes*, number of *TRUE* values
#' - *fails*, number of *FALSE* values
#' - *NAs*, number of logical NAs
#' - *valid*, are all values *TRUE*?
#' - *rule*, the expression being evaluated 
#'
#' Additional rules can be specify in a call to *verify*, in one of two forms.
#' Simple rules can be defined as a formula involving methods defined for the
#' class. A rule such as `highm = ~ m < 2` will check if values in the *m* slot are
#' higher than 2 and return a logical *FLQuant*.
#'
#' Some rules cannot simply use existing methods or functions, for example those
#' operating on all slots of the object, or requiring additional computations.
#' In this case, the argument to *verify* can be a list, with an element named
#' *rule* of class *formula* and where test is defined. The test then calls for
#' a new function, defined as another element of the list, and which will be used
#' by verify when evaluating the set of rules. See below for examples.
#'
#' @name verify
#' @rdname verify
#' @param object An object of any FLR class for which the method has been defined
#' @param ... Additional rules to be tested, as a formula or list. See details
#' @param report Should the standard data.frame report be output (if TRUE) or a single logical value for all tests?
#' @return A data.frame with the results of applying those rules, or a single logical value, if report=FALSE
#' @author The FLR Team
#' @keywords methods
#' @md
#' @examples
#' # Verifying a new rule for an FLSR object
#' data(nsher)
#' # rule: are all recruitment values greater than 0?
#' verify(nsher, rec=~rec > 0)
#'
#' # Define rule calling its own function
#' data(ple4)
#' # rule: ssb is less
#' verify(ple4, ssbstock = ~ssb < stock)

setGeneric("verify", function(object, ...) standardGeneric("verify"))
# }}}

# ruleset {{{
#' Set of verify rules for an FLR class
#'
#' Returns a set of standard rules to be used by the verify method for an
#' object of any given class.
#'
#' @name ruleset
#' @rdname ruleset
#' @param object An object of any FLR class for which the method has been defined.
#' @param ... Names of positions in the standard list to subset.
#' @return A named list containing the rules defined for for the class object belongs to.
#' @author The FLR Team
#' @keywords methods
#' @md
setGeneric("ruleset", function(object, ...) standardGeneric("ruleset"))
# }}}

# fwdWindow
setGeneric("fwdWindow", function(x, y, ...)
    standardGeneric("fwdWindow"))

# refpts
setGeneric("refpts", function(object, ...)
		standardGeneric("refpts"))

# dbind

#' Methods for binding objects of array classes along a given dimension
#'
#' These methods can bind two or more objects of array-based classes
#' (e.g. FLQuant), along the specified dimension.
#'
#' The objects to bind must contain the same dimmames in all dimensions other
#' than that used to bind, while dimnames in the selected one must differ. See
#' the examples below for correct and incorrect uses.
#'
#' Object are bound in the order they are provided, with no attempt to sort
#' according to the dimnames of the chosen dimension.
#'
#' The implementation is based around a single method (*dbind*), that operates
#' along the dimension position or name indicated by the *dim* argument. A
#' series of shortcut functions call the method for specific dimensions, with
#' names related to the dimensions name they operate on (e.g. ybind for *year*).
#'
#' @param x First object to bind
#' @param y Second object to bind
#' @param ... Other objects to bind
#'
#' @return An object of the same class as the inputs
#'
#' @name dbind
#' @rdname dbind-methods
#' @aliases dbind dbind-methods
#'
#' @author Iago Mosqueira (EC JRC)
#' @seealso [`FLQuant`] [`FLarray`]
#' @keywords methods
#' @md

setGeneric("dbind", function(x, y, ...)
		standardGeneric("dbind"))

