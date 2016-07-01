# FLGrowth - An FLModel-based class for growth models
# FLCore/R/FLGrowth

# Copyright 2003-2016 FLR Team. Distributed under the GPL 2 or later
# Maintainer: Iago Mosqueira, EC JRC

# class FLGrowth {{{

#' Class FLGrowth
#' 
#' A class for growth models.
#'
#' @name FLGrowth
#' @aliases FLGrowth-class
#' @docType class
#' @section Slots: \describe{
#'   \item{mass}{Mass (weight) at age. \code{FLArray}.} }
#' @author The FLR Team
#' @seealso \link{FLModel},
#' @keywords classes
#' @examples
#' 
#' data(ple4)
#' FLGrowth(mass=stock.wt(ple4)[,1])
#'

setClass('FLGrowth', representation('FLModel',
   mass='FLArray')) # }}}

# constructor	{{{
setMethod('FLGrowth', signature(model='ANY'),
   function(model, ...)
     return(FLModel(model, ..., class='FLGrowth')))

setMethod('FLGrowth', signature(model='missing'),
 function(...)
  return(FLModel(formula(NULL), ..., class='FLGrowth')))	# }}}

# accessor {{{
setMethod('mass', signature(object='FLGrowth'),
  function(object)
    return(object@mass)) # }}}
