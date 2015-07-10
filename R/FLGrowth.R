# FLGrowth - An FLModel-based class for growth models
# R/FLGrowth

# Copyright 2003-2015 FLR Team. Distributed under the GPL 2 or later
# Maintainer: Iago Mosqueira, EC JRC G03

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

