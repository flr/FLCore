# FLGrowth - «Short one line description»
# R/FLGrowth

# Copyright 2003-2008 FLR Team. Distributed under the GPL 2 or later
# Maintainer: Iago Mosqueira, Cefas
# $Id: FLArray.R 613 2010-03-30 09:59:23Z imosqueira $

# class FLGrowth
setClass('FLGrowth', representation('FLModel',
   mass='FLArray'))

# constructor	{{{
setMethod('FLGrowth', signature(model='ANY'),
   function(model, ...)
     return(FLModel(model, ..., class='FLGrowth')))

setMethod('FLGrowth', signature(model='missing'),
 function(...)
  return(FLModel(formula(NULL), ..., class='FLGrowth')))	# }}}

# accessor
setMethod('mass', signature(object='FLGrowth'),
  function(object)
    return(object@mass))

