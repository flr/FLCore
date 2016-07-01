# FLQuantJK.R - FLQuantJK class and methods
# FLCore/R/FLQuantJK.R

# Copyright 2003-2016 FLR Team. Distributed under the GPL 2 or later
# Maintainer: Iago Mosqueira, EC JRC

# jackknife  {{{

#' Method jacknife
#'
#' Jacknife resampling
#' 
#' The \code{jacknife} method sets up objects ready for jacknifing, i.e. to
#' systematically recompute a given statistic leaving out one observation at a
#' time. From this new set of "observations" for the statistic, estimates for
#' the bias and variance of the statstic can be calculated.
#' 
#' Input objects cannot have length > 1 along the \code{iter} dimension, and
#' the resulting object will have one more \code{iter} than the number of
#' elements in the original object.
#' 
#' @name jacknife
#' @aliases jacknife jacknife-methods jacknife,FLQuant-method
#' @docType methods
#' @section Generic function: jacknife(object, ...)
#' @author The FLR Team
#' @seealso \code{\link{FLQuant}}
#' @keywords methods
#' @examples
#' 
#' flq <- FLQuant(1:8)
#' flj <- jacknife(flq)
#' iters(flj)
#'
#' # Calculate the bias of the mean and variance estimators
#' (mean(iter(yearMeans(flj),2:9))-c(iter(yearMeans(flj),1)))*7
#' (mean(iter(yearVars(flj),2:9))-c(iter(yearVars(flj),1)))*7
#'

setMethod('jackknife', signature(object='FLQuant'),
  function(object, dim='year', rm.na=TRUE) {

    # tests
      # object must have no iters
      if(dim(object)[6] > 1)
        stop("object cannot already contain iters: dim(object)[6] > 1")

      # only one dim
      if(length(dim) > 1)
        stop(paste("Objects can only be jackknifed along 1 dimension:"), dim)

      # dim cannot be 'iter'
      if(dim == 6 | dim =='iter')
        stop("iter dimension cannot be jackknifed")

    # match dim name
    if(is.character(dim))
      dim <- seq(1:6)[names(object) %in% dim]

    # propagate
    res <- propagate(object, dim(object)[dim])
  
    # permutaion vector, place dim one before last
    perms <- rep(5, 5)
    perms[-dim] <- 1:4

    # create array with 1 at each location by iter
    idx <- aperm(array(c(rep(T, prod(dim(object)[-dim])), rep(F, prod(dim(res)[-6]))),
        dim=c(dim(object)[seq(1:5)[-dim]], dim(object)[dim], dim(object)[dim])),
      c(perms, 6))

    res[idx] <- NA

    res <- new("FLQuantJK", res, orig=object)

    jk.NA=function(x,object){
        flag=seq(dims(x)$iter)[c(!is.na(object))]
        return(x[,,,,,flag])}
    
    if (rm.na)  res=jk.NA(res,object)
 
    return(res)
  }
) # }}}

# orig {{{
setMethod("orig", signature(object="FLQuantJK"),
  function(object) {
    return(object@orig)
  }
) # }}}
