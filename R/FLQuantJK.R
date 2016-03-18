
# FLQuantJK.R - FLQuantJK class and methods
# FLCore/R/FLQuantJK.R

# Copyright 2003-2016 FLR Team. Distributed under the GPL 2 or later
# Maintainer: Iago Mosqueira, EC JRC

# FLQuantJK     {{{

#' A class for jackkifing of FLQuant objects
#'
#' Jackknife resampling works by calculating a given statistic on replicates of
#' the original data by systematically leaving out each observation. The mean of
#' the statistic value is then computed. It can be considered a linear approximation
#' of the bootstrap.
#'
#' The \code{FLQuantJK} class contains those N replicates of the original
#' \code{\linkS4class{FLQuant}} object, in which each \code{iter} corresponds
#' contains one less observation. Jackknifing on an \code{\linkS4class{FLQuant}}
#' object is carried out along a single dimension. See \code{\link{FLCore}{jackknife}}
#' for further details.
#' 
#' @name FLQuantJK
#' @rdname FLQuantJK
#' @docType class
#' @aliases FLQuantJK FLQuantJK-methods FLQuantJK-class
#'
#' @section Slots:
#'  \describe{
#'    \item{.Data}{A jackknifed object with replicates stored along the \code{iter}
#' dimension, (\code{FLQuant}).}
#'    \item{orig}{The original \code{FLQuant}.}
#' }
#'
#' @section Validity:
#'
#'  \describe{
#'    \item{Element dimensions}{The objects in the \code{.Data} and \code{orig}
#'    slots must share all but rehe last dimension.}
#' }
#'
#' You can inspect the class validity function by using
#'    \code{getValidity(getClassDef("FLQuantJK"))}
#'
#' @section Constructor:
#' A construction method exists for this class that takes a single object of
#' class \code{FLQuant}, in which the \code{iter} dimensions if of length 1. See
#' \code{\link[FLCore]{jackknife}} for further details.
#'
#' @section Methods:
#' Methods exist for various calculations based on values stored in the class:
#'
#' @section Accessors:
#' \describe{
#'  \item{orig}{Returns the original \code{FLQuant} stored in the \code{orig} slot.}
#' }
#'
#' @author The FLR Team
#' @seealso \code{\linkS4class{FLQuant}} \link{FLCore}{jackknife}
#' @keywords classes
#' @examples
#'
#' flq <- FLQuant(rnorm(50), dim=c(5,10))
#' flj <- FLQuantJK(flq)
#' 
#' # using 'quant' dim
#' flj <- FLQuantJK(flq, dim='quant')
#' 
setClass("FLQuantJK",
  representation("FLQuant", orig="FLQuant"),
  prototype(new("FLQuant"), orig=new("FLQuant")),
  validity=function(object) {
    # CHECK dim .Data & orig only differ in iter
    if(any(dim(object@orig)[-6] != dim(object@.Data)[-6])) {
      return("original and jackknifed objects in class must share dimensions 1:5")
    }
    return(TRUE)
  })

setMethod("FLQuantJK", signature(object="FLQuant"),
  function(object, ...) {
    return(jackknife(object, ...))
  }) # }}}


# jackknife  {{{

#' A method for creating an object of class FLQuantJK, a jackknifed FLQuant
#'
#' Jackknife resampling works by calculating a given statistic on replicates of
#' the original data by systematically leaving out each observation. The mean of
#' the statistic value is then computed. It can be considered a linear approximation
#' of the bootstrap.
#'
#' This method resamples along the stated dimension name (defaults to \code{year})
#' and returns an object with as many \code{iter} as the length of the selected
#' dimension.
#'
#' @param object An object to be jackknifed, \code{\linkS4class{FLQuant}}.
#' @param dim Name or position of the dimension to resample through, defaults to "year".
#'
#' @return RETURN Lorem ipsum dolor sit amet
#'
#' @name jackknife
#' @rdname jackknife
#' 
#' @author The FLR Team
#' @seealso \code{\linkS4class{FLQuantJK}}, \code{\linkS4class{FLQuant}}
#' @keywords classes
#' @examples
#'
setMethod("jackknife", signature(object="FLQuant"),
  function(object, dim="year", rm.na=TRUE) {

    # tests
      # object must have no iters
      if(dim(object)[6] > 1)
        stop("object cannot already contain iters: dim(object)[6] > 1")

      # only one dim
      if(length(dim) > 1)
        stop(paste("Objects can only be jackknifed along 1 dimension:"), dim)

      # dim cannot be "iter"
      if(dim == 6 | dim == "iter")
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
    idx <- aperm(array(c(rep(T, prod(dim(object)[-dim])),
      rep(F, prod(dim(res)[-6]))), dim=c(dim(object)[seq(1:5)[-dim]],
        dim(object)[dim], dim(object)[dim])), c(perms, 6))

    res[idx] <- NA

    res <- new("FLQuantJK", res, orig=object)

    jk.NA <- function(x,object){
        flag <- seq(dims(x)$iter)[c(!is.na(object))]
        return(x[,,,,,flag])
    }

    if (rm.na) {
      res <- jk.NA(res,object)
    }

    return(res)
  }
) # }}}

# orig {{{
setMethod("orig", signature(object="FLQuantJK"),
  function(object) {
    return(object@orig)
  }
) # }}}
