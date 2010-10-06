# genericMethods - «Short one line description»
# genericMethods

# Copyright 2009 Iago Mosqueira, Cefas. Distributed under the GPL 2 or later
# $Id:  $

# Reference:
# Notes:

# TODO Wed 25 Nov 2009 05:48:37 PM CET IM:

# Existing methods/functions

setGeneric("AIC", useAsDefault = stats::AIC)
setGeneric("apply", useAsDefault = apply)
setGeneric("as.data.frame", useAsDefault = as.data.frame)
setGeneric("barchart", useAsDefault = barchart)
setGeneric("BIC", useAsDefault = stats4::BIC)
setGeneric("bwplot", useAsDefault = bwplot)
setGeneric("coef", useAsDefault = coef)
setGeneric("densityplot", useAsDefault = densityplot)
setGeneric("dotplot", useAsDefault = dotplot)
setGeneric("histogram", useAsDefault = histogram)
setGeneric("lapply", useAsDefault = lapply)
setGeneric("lm", useAsDefault = lm)
setGeneric("lowess", useAsDefault = lowess)
setGeneric("mean", useAsDefault = mean)
setGeneric("median", useAsDefault = median)
setGeneric("model.frame", useAsDefault = model.frame)
setGeneric("names")
setGeneric("nls", useAsDefault = nls)
setGeneric("plot", useAsDefault = plot)
setGeneric("predict", useAsDefault = predict)
setGeneric("print", useAsDefault = print)
setGeneric("profile", useAsDefault = profile)
setGeneric("quantile", useAsDefault = quantile)
setGeneric("rgamma", useAsDefault = rgamma)
setGeneric("scale", useAsDefault = scale)
setGeneric("sd", useAsDefault = sd)
setGeneric("splom", useAsDefault = splom)
setGeneric("stripplot", useAsDefault = stripplot)
setGeneric("summary", useAsDefault = summary)
setGeneric("sweep", useAsDefault = sweep)
setGeneric("transform", useAsDefault=transform)
setGeneric("update", useAsDefault = update)
setGeneric("units", useAsDefault=units)
setGeneric("var", useAsDefault = var)
setGeneric("window", useAsDefault = window)
setGeneric("xyplot", useAsDefault = xyplot)


# NEW methods

# range<-
setGeneric("range<-", function(x, i, value) standardGeneric("range<-"))

# units<-
setGeneric("units<-", function(x, value) standardGeneric("units<-"))

# desc
setGeneric('desc', function(object, ...) standardGeneric('desc'))
setGeneric('desc<-', function(object, ..., value) standardGeneric('desc<-'))

# dims
setGeneric("dims", function(obj, ...) standardGeneric("dims"))

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

# FLStock
setGeneric('FLStock', function(object, ...) standardGeneric('FLStock'))

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

# computeLandings
setGeneric("computeLandings", function(object, ...)
		standardGeneric("computeLandings"))

# computeDiscards
	setGeneric("computeDiscards", function(object, ...)
		standardGeneric("computeDiscards"))

# computeCatch
	setGeneric("computeCatch", function(object, ...)
		standardGeneric("computeCatch"))

# computeStock
setGeneric("computeStock", function(object, ...)
		standardGeneric("computeStock"))

# ssb
	setGeneric("ssb", function(object, ...)
		standardGeneric("ssb"))

# tsb
	setGeneric("tsb", function(object, ...)
		standardGeneric("tsb"))

# fbar
	setGeneric("fbar", function(object, ...)
		standardGeneric("fbar"))

# as.FLStock
	setGeneric("as.FLStock", function(object, ...)
		standardGeneric("as.FLStock"))

# ssbpurec
	setGeneric("ssbpurec", function(object, ...)
		standardGeneric("ssbpurec"))

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

# FLBiol
setGeneric('FLBiol', function(object, ...)
  standardGeneric('FLBiol'))

# mean.lifespan
setGeneric("mean.lifespan", function(x, ...)
	standardGeneric("mean.lifespan"))

# as.FLBiol
setGeneric("as.FLBiol", function(object, ...)
  standardGeneric("as.FLBiol"))

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

# rec
setGeneric("rec", function(object, ...)
  standardGeneric("rec"))

# FLMetier
setGeneric('FLMetier', function(catches, ...)
		standardGeneric('FLMetier'))

# gear
  setGeneric('gear', function(object, ...) standardGeneric('gear'))
  setGeneric('gear<-', function(object, ..., value) standardGeneric('gear<-'))

# effshare
  setGeneric('effshare', function(object, ...) standardGeneric('effshare'))
  setGeneric('effshare<-', function(object, ..., value) standardGeneric('effshare<-'))

# vcost
  setGeneric('vcost', function(object, ...) standardGeneric('vcost'))
  setGeneric('vcost<-', function(object, ..., value) standardGeneric('vcost<-'))

# catches
  setGeneric('catches', function(object, ...) standardGeneric('catches'))
  setGeneric('catches<-', function(object, ..., value) standardGeneric('catches<-'))

# effort
  setGeneric("effort", function(object, metier, ...) standardGeneric("effort"))
  setGeneric("effort<-", function(object, ..., value) standardGeneric("effort<-"))

# fcost
  setGeneric('fcost', function(object, ...) standardGeneric('fcost'))
  setGeneric('fcost<-', function(object, ..., value) standardGeneric('fcost<-'))

# capacity
  setGeneric('capacity', function(object, ...) standardGeneric('capacity'))
  setGeneric('capacity<-', function(object, ..., value) standardGeneric('capacity<-'))

# crewshare
  setGeneric('crewshare', function(object, ...) standardGeneric('crewshare'))
  setGeneric('crewshare<-', function(object, ..., value) standardGeneric('crewshare<-'))

# metiers
  setGeneric('metiers', function(object, ...) standardGeneric('metiers'))
  setGeneric('metiers<-', function(object, ..., value) standardGeneric('metiers<-'))

# FLFleet
setGeneric('FLFleet', function(object, ...)
		standardGeneric('FLFleet'))

# metier
setGeneric('metier', function(object, metier, ...)
		standardGeneric('metier'))
setGeneric('metier<-', function(object, metier, ..., value)
		standardGeneric('metier<-'))

# type
setGeneric('type', function(object, ...)
		standardGeneric('type'))
setGeneric('type<-', function(object, ..., value)
		standardGeneric('type<-'))

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

# FLIndex
setGeneric('FLIndex', function(object, ...)
	standardGeneric('FLIndex'))

# as.FLindex
setGeneric("as.FLIndex", function(object, ...)
  standardGeneric("as.FLIndex"))

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

# ssb
setGeneric('ssb', function(object, ...)
		standardGeneric('ssb'))
setGeneric('ssb<-', function(object, ..., value)
		standardGeneric('ssb<-'))

# covar
setGeneric('covar', function(object, ...)
		standardGeneric('covar'))
setGeneric('covar<-', function(object, ..., value)
		standardGeneric('covar<-'))

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

# price
setGeneric('price', function(object, ...)
		standardGeneric('price'))
setGeneric('price<-', function(object, ..., value)
		standardGeneric('price<-'))

# spr0
setGeneric("spr0", function(ssb, rec, fbar, ...)
	  standardGeneric("spr0"))

# ab
setGeneric('ab', function(object, ...)
		standardGeneric('ab'))

# quant, quant<-
setGeneric("quant", function(object, ...)
  standardGeneric("quant"))
setGeneric("quant<-", function(object, value)
  standardGeneric("quant<-"))

# iter, iter<-
setGeneric("iter", function(object, ...)
	standardGeneric("iter"))
setGeneric("iter<-", function(object, value)
  standardGeneric("iter<-"))

# trim  
setGeneric("trim", function(x, ...)
	standardGeneric("trim"))

# expand
setGeneric("expand", function(x, ...)
	standardGeneric("expand"))

# FLCatch
setGeneric('FLCatch', function(object, ...)
		standardGeneric('FLCatch'))

# addFLCatch
setGeneric('addFLCatch', function(e1, e2, ...)
		standardGeneric('addFLCatch'))

# catchNames
setGeneric('catchNames', function(object, ...)
		standardGeneric('catchNames'))
setGeneric('catchNames<-', function(object, ..., value)
		standardGeneric('catchNames<-'))

# catch.sel
setGeneric('catch.sel', function(object, ...)
		standardGeneric('catch.sel'))

# FLCohort
setGeneric("FLCohort", function(object, ...)
	standardGeneric("FLCohort"))

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

# FLPar
setGeneric("FLPar", function(object, ...)
	standardGeneric("FLPar"))

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

# params, params<-
setGeneric("params", function(object, ...)
  standardGeneric("params"))
setGeneric("params<-", function(object, value)
  standardGeneric("params<-"))

# FLModel
setGeneric('FLModel', function(model, ...)
    standardGeneric('FLModel'))

# as.FLSR
setGeneric("as.FLSR", function(object, ...)
  standardGeneric("as.FLSR"))

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
setGeneric('sp', function(stock, catch, ...)
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

# revenue
setGeneric('revenue', function(object, ...)
		standardGeneric('revenue'))

# catches
setGeneric('catches', function(object, ...)
		standardGeneric('catches'))
setGeneric('catches<-', function(object, catch, ..., value)
		standardGeneric('catches<-'))

# FLGrowth
setGeneric('FLGrowth', function(model, ...)
  standardGeneric('FLGrowth'))

# mass
setGeneric('mass', function(object, ...)
  standardGeneric('mass'))

# FLlst
setGeneric("FLlst", function(object, ...)
	standardGeneric("FLlst"))
 
# FLStocks
setGeneric("FLStocks", function(object, ...)
	standardGeneric("FLStocks"))
