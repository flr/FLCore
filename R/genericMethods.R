# genericMethods - S4 generics
# FLCore/R/genericMethods

# Copyright 2003-2016 FLR Team. Distributed under the GPL 2 or later
# Maintainer: Iago Mosqueira, EC JRC

globalVariables(c("qname"))

# -- OVERLOADED methods/functions

setGeneric("AIC", useAsDefault = stats::AIC)
setGeneric("apply", useAsDefault = apply)
setGeneric("as.data.frame", useAsDefault = as.data.frame)
setGeneric("barchart", useAsDefault = lattice::barchart)
setGeneric("bwplot", useAsDefault = lattice::bwplot)
setGeneric("coef", useAsDefault = coef)
setGeneric("densityplot", useAsDefault = densityplot)
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
setGeneric("mvrnorm", useAsDefault=mvrnorm)
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
setGeneric("tail", useAsDefault=tail)
setGeneric("transform", useAsDefault=transform)
setGeneric("tsp", useAsDefault=tsp)
setGeneric("units", useAsDefault=units)
setGeneric("update", useAsDefault = update)
setGeneric("var", useAsDefault = var)
setGeneric("window", useAsDefault = window)
setGeneric("wireframe", useAsDefault=wireframe)
setGeneric("xyplot", useAsDefault = xyplot)

# -- class CONSTRUCTORS, documented with each class

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

# -- ACCESSORS

#' Accesor and replacement methods for FLCore classes
#'
#' All S4 classes defined in FLCore have methods for accessing and replacing any
#' of their slots. These methods are named as the slot, and will return the
#' content of the slot, for the accessor method, or modify it with the provided
#' value.
#'
#' Accessors and replacement methods, with some exception, are created at build
#' time by calls to the \code{createFLAccessors} function. An accesor method is
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
#' @rdname accesors
#' @alias catch catch.n catch.n catch.n<- catch.n<- catch.q catch.q<- catch.wt
#' @alias catch.wt catch.wt<- catch.wt<- catch<- desc desc<- details details<-
#' @alias discards discards.n discards.n<- discards.sel discards.sel<- discards.wt
#' @alias discards.wt<- discards<- distr distr<- distribution distribution<-
#' @alias effort effort<- fec fec<- fitted fitted<- gr gr<- harvest harvest.spwn
#' @alias harvest.spwn<- harvest<- hessian hessian<- index index.q index.q<-
#' @alias index.var index.var<- index<- initial initial<- landings landings.n
#' @alias landings.n<- landings.sel landings.sel<- landings.wt landings.wt<-
#' @alias landings<- logLik logLik<- logerror logerror<- logl logl<- m m m.spwn
#' @alias m.spwn<- m<- m<- mat mat<- model model<- n n<- name name<- params
#' @alias params<- range<- rec rec.obs rec<- residuals residuals<- sel.pattern
#' @alias sel.pattern<- spwn spwn<- stock stock.n stock.n<- stock.wt stock.wt<-
#' @alias stock<- type type<- units<- vcov vcov<- wt wt<- 
#'
#' @genericMethods
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

# range<-
setGeneric("range<-", function(x, i, value) standardGeneric("range<-"))

# units<-
setGeneric("units<-", function(x, value) standardGeneric("units<-"))

# desc
setGeneric('desc', function(object, ...) standardGeneric('desc'))
setGeneric('desc<-', function(object, ..., value) standardGeneric('desc<-'))

# catch
setGeneric('catch', function(object, ...) standardGeneric('catch'))
setGeneric('catch<-', function(object, ..., value) standardGeneric('catch<-'))

# catch.n
setGeneric('catch.n', function(object, ...) standardGeneric('catch.n'))
setGeneric('catch.n<-', function(object, ..., value) standardGeneric('catch.n<-'))

# catch.wt
setGeneric('catch.wt', function(object, ...) standardGeneric('catch.wt'))
setGeneric('catch.wt<-', function(object, ..., value) standardGeneric('catch.wt<-'))

# discards
setGeneric('discards', function(object, ...) standardGeneric('discards'))
setGeneric('discards<-', function(object, ..., value) standardGeneric('discards<-'))

# discards.n
setGeneric('discards.n', function(object, ...) standardGeneric('discards.n'))
setGeneric('discards.n<-', function(object, ..., value) standardGeneric('discards.n<-'))

# discards.wt
setGeneric('discards.wt', function(object, ...) standardGeneric('discards.wt'))
setGeneric('discards.wt<-', function(object, ..., value) standardGeneric('discards.wt<-'))

# landings
setGeneric('landings', function(object, ...) standardGeneric('landings'))
setGeneric('landings<-', function(object, ..., value) standardGeneric('landings<-'))

# landings.n
setGeneric('landings.n', function(object, ...) standardGeneric('landings.n'))
setGeneric('landings.n<-', function(object, ..., value) standardGeneric('landings.n<-'))

# landings.wt
setGeneric('landings.wt', function(object, ...) standardGeneric('landings.wt'))
setGeneric('landings.wt<-', function(object, ..., value) standardGeneric('landings.wt<-'))

# m
setGeneric('m', function(object, ...) standardGeneric('m'))
setGeneric('m<-', function(object, ..., value) standardGeneric('m<-'))

# name, name<-
setGeneric('name', function(object, ...) standardGeneric('name'))
setGeneric('name<-', function(object, ..., value) standardGeneric('name<-'))

# stock
setGeneric('stock', function(object, ...) standardGeneric('stock'))
setGeneric('stock<-', function(object, ..., value) standardGeneric('stock<-'))

# stock.n
setGeneric('stock.n', function(object, ...) standardGeneric('stock.n'))
setGeneric('stock.n<-', function(object, ..., value) standardGeneric('stock.n<-'))

# stock.wt
setGeneric('stock.wt', function(object, ...) standardGeneric('stock.wt'))
setGeneric('stock.wt<-', function(object, ..., value) standardGeneric('stock.wt<-'))

# m.spwn
setGeneric('m.spwn', function(object, ...) standardGeneric('m.spwn'))
setGeneric('m.spwn<-', function(object, ..., value) standardGeneric('m.spwn<-'))

# harvest
setGeneric('harvest', function(object, catch, ...) standardGeneric('harvest'))
setGeneric('harvest<-', function(object, ..., value) standardGeneric('harvest<-'))

# harvest.spwn
setGeneric('harvest.spwn', function(object, ...) standardGeneric('harvest.spwn'))
setGeneric('harvest.spwn<-', function(object, ..., value) standardGeneric('harvest.spwn<-'))

# mat
setGeneric('mat', function(object, ...) standardGeneric('mat'))
setGeneric('mat<-', function(object, ..., value) standardGeneric('mat<-'))

# n
setGeneric('n', function(object, ...) standardGeneric('n'))
setGeneric('n<-', function(object, ..., value) standardGeneric('n<-'))

# m
setGeneric('m', function(object, ...) standardGeneric('m'))
setGeneric('m<-', function(object, ..., value) standardGeneric('m<-'))

# wt
setGeneric('wt', function(object, ...) standardGeneric('wt'))
setGeneric('wt<-', function(object, ..., value) standardGeneric('wt<-'))

# fec
setGeneric('fec', function(object, ...) standardGeneric('fec'))
setGeneric('fec<-', function(object, ..., value) standardGeneric('fec<-'))

# spwn
setGeneric('spwn', function(object, ...) standardGeneric('spwn'))
setGeneric('spwn<-', function(object, ..., value) standardGeneric('spwn<-'))

# effort
setGeneric("effort", function(object, metier, ...) standardGeneric("effort"))
setGeneric("effort<-", function(object, ..., value) standardGeneric("effort<-"))

# type
setGeneric('type', function(object, ...)
		standardGeneric('type'))
setGeneric('type<-', function(object, ..., value)
		standardGeneric('type<-'))

# distr
setGeneric('distr', function(object, ...)
		standardGeneric('distr'))
setGeneric('distr<-', function(object, ..., value)
		standardGeneric('distr<-'))

# distribution
setGeneric('distribution', function(object, ...)
		standardGeneric('distribution'))
setGeneric('distribution<-', function(object, ..., value)
		standardGeneric('distribution<-'))

# index
setGeneric('index', function(object, ...)
		standardGeneric('index'))
setGeneric('index<-', function(object, ..., value)
		standardGeneric('index<-'))

# index.var
setGeneric('index.var', function(object, ...)
		standardGeneric('index.var'))
setGeneric('index.var<-', function(object, ..., value)
		standardGeneric('index.var<-'))

# catch.n
setGeneric('catch.n', function(object, ...)
		standardGeneric('catch.n'))
setGeneric('catch.n<-', function(object, ..., value)
		standardGeneric('catch.n<-'))

# catch.wt
setGeneric('catch.wt', function(object, ...)
		standardGeneric('catch.wt'))
setGeneric('catch.wt<-', function(object, ..., value)
		standardGeneric('catch.wt<-'))

# sel.pattern
setGeneric('sel.pattern', function(object, ...)
		standardGeneric('sel.pattern'))
setGeneric('sel.pattern<-', function(object, ..., value)
		standardGeneric('sel.pattern<-'))

# index.q
setGeneric('index.q', function(object, ...)
		standardGeneric('index.q'))
setGeneric('index.q<-', function(object, ..., value)
		standardGeneric('index.q<-'))

# model
setGeneric('model', function(object, ...)
		standardGeneric('model'))
setGeneric('model<-', function(object, ..., value)
		standardGeneric('model<-'))

# logl
setGeneric('logl', function(object, ...)
		standardGeneric('logl'))
setGeneric('logl<-', function(object, ..., value)
		standardGeneric('logl<-'))

# gr
setGeneric('gr', function(object, ...)
		standardGeneric('gr'))
setGeneric('gr<-', function(object, ..., value)
		standardGeneric('gr<-'))

# initial
setGeneric('initial', function(object, ...)
		standardGeneric('initial'))
setGeneric('initial<-', function(object, ..., value)
		standardGeneric('initial<-'))

# logLik
setGeneric('logLik', function(object, ...)
		standardGeneric('logLik'))
setGeneric('logLik<-', function(object, ..., value)
		standardGeneric('logLik<-'))

# vcov
setGeneric('vcov', function(object, ...)
		standardGeneric('vcov'))
setGeneric('vcov<-', function(object, ..., value)
		standardGeneric('vcov<-'))

# hessian
setGeneric('hessian', function(object, ...)
		standardGeneric('hessian'))
setGeneric('hessian<-', function(object, ..., value)
		standardGeneric('hessian<-'))

# logerror
setGeneric('logerror', function(object, ...)
		standardGeneric('logerror'))
setGeneric('logerror<-', function(object, ..., value)
		standardGeneric('logerror<-'))

# details
setGeneric('details', function(object, ...)
		standardGeneric('details'))
setGeneric('details<-', function(object, ..., value)
		standardGeneric('details<-'))

# residuals
setGeneric('residuals', function(object, ...)
		standardGeneric('residuals'))
setGeneric('residuals<-', function(object, ..., value)
		standardGeneric('residuals<-'))

# fitted
setGeneric('fitted', function(object, ...)
		standardGeneric('fitted'))
setGeneric('fitted<-', function(object, ..., value)
		standardGeneric('fitted<-'))

# rec
setGeneric('rec', function(object, ...)
		standardGeneric('rec'))
setGeneric('rec<-', function(object, ..., value)
		standardGeneric('rec<-'))

# rec.obs
setGeneric('rec.obs', function(object, ...)
		standardGeneric('rec.obs'))

# catch.q
setGeneric('catch.q', function(object, ...)
		standardGeneric('catch.q'))
setGeneric('catch.q<-', function(object, ..., value)
		standardGeneric('catch.q<-'))

# discards.sel
setGeneric('discards.sel', function(object, ...)
		standardGeneric('discards.sel'))
setGeneric('discards.sel<-', function(object, ..., value)
		standardGeneric('discards.sel<-'))

# landings.sel
setGeneric('landings.sel', function(object, ...)
		standardGeneric('landings.sel'))
setGeneric('landings.sel<-', function(object, ..., value)
		standardGeneric('landings.sel<-'))

# params, params<-
setGeneric("params", function(object, ...)
  standardGeneric("params"))
setGeneric("params<-", function(object, value)
  standardGeneric("params<-"))


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

setGeneric("iter", function(obj, ...)
	standardGeneric("iter"))
setGeneric("iter<-", function(object, ..., value)
  standardGeneric("iter<-")) # }}}

# lower
setGeneric("lower", function(object, ...)
  standardGeneric("lower"))
setGeneric("lower<-", function(object, ..., value)
    standardGeneric("lower<-"))

# upper
setGeneric("upper", function(object, ...)
  standardGeneric("upper"))
setGeneric("upper<-", function(object, ..., value)
  standardGeneric("upper<-"))

# ssb
setGeneric('ssb', function(object, ...)
		standardGeneric('ssb'))
setGeneric('ssb<-', function(object, ..., value)
		standardGeneric('ssb<-'))

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

# catchNames
setGeneric('catchNames', function(object, ...)
		standardGeneric('catchNames'))
setGeneric('catchNames<-', function(object, ..., value)
		standardGeneric('catchNames<-'))

# catch.sel
setGeneric('catch.sel', function(object, ...)
		standardGeneric('catch.sel'))

# flc2flq
setGeneric("flc2flq", function(object, ...)
	standardGeneric("flc2flq"))

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

# propagate
setGeneric("propagate", function(object, ...)
    standardGeneric("propagate"))

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
#' @aliases %+%
setGeneric("%+%", function(x, y)
  standardGeneric("%+%"))

# %-%
#' @rdname operators
#' @aliases %-%
setGeneric("%-%", function(x, y)
  standardGeneric("%-%"))

# %^%
#' @rdname operators
#' @aliases %^%
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

# quantTotals
setGeneric("quantTotals", function(x, ...)
    standardGeneric("quantTotals"))

# yearTotals
setGeneric("yearTotals", function(x, ...)
    standardGeneric("yearTotals"))

# dim Sums, Means, Medians & CVs
setGeneric("quantSums", function(x, ...) standardGeneric("quantSums"))
setGeneric("yearSums", function(x, ...) standardGeneric("yearSums"))
setGeneric("unitSums", function(x, ...) standardGeneric("unitSums"))
setGeneric("seasonSums", function(x, ...) standardGeneric("seasonSums"))
setGeneric("areaSums", function(x, ...) standardGeneric("areaSums"))
setGeneric("iterSums", function(x, ...) standardGeneric("iterSums"))
setGeneric("dimSums", function(x, ...) standardGeneric("dimSums"))
setGeneric("quantMeans", function(x, ...) standardGeneric("quantMeans"))
setGeneric("yearMeans", function(x, ...) standardGeneric("yearMeans"))
setGeneric("unitMeans", function(x, ...) standardGeneric("unitMeans"))
setGeneric("seasonMeans", function(x, ...) standardGeneric("seasonMeans"))
setGeneric("areaMeans", function(x, ...) standardGeneric("areaMeans"))
setGeneric("iterMeans", function(x, ...) standardGeneric("iterMeans"))
setGeneric("dimMeans", function(x, ...) standardGeneric("dimMeans"))
setGeneric("quantVars", function(x, ...) standardGeneric("quantVars"))
setGeneric("yearVars", function(x, ...) standardGeneric("yearVars"))
setGeneric("unitVars", function(x, ...) standardGeneric("unitVars"))
setGeneric("seasonVars", function(x, ...) standardGeneric("seasonVars"))
setGeneric("areaVars", function(x, ...) standardGeneric("areaVars"))
setGeneric("iterVars", function(x, ...) standardGeneric("iterVars"))
setGeneric("dimVars", function(x, ...) standardGeneric("dimVars"))
setGeneric("iterMedians", function(x, ...) standardGeneric("iterMedians"))
setGeneric("iterCVs", function(x, ...) standardGeneric("iterCVs"))

# Z
setGeneric("z", function(object, ...)
    standardGeneric("z"))
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
setGeneric("predictModel", function(object, ...)
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
