# jackknife.R - FLQuantJK and FLParJK class and methods
# FLCore/R/jackknife.R

# Copyright 2003-2017 FLR Team. Distributed under the GPL 2 or later
# Maintainer: Iago Mosqueira, EC JRC

# jackknife  {{{

#' Method jackknife
#'
#' Jackknife resampling
#' 
#' The \code{jackknife} method sets up objects ready for jackknifing, i.e. to
#' systematically recompute a given statistic leaving out one observation at a
#' time. From this new set of "observations" for the statistic, estimates for
#' the bias and variance of the statstic can be calculated.
#' 
#' Input objects cannot have length > 1 along the \code{iter} dimension, and
#' the resulting object will have one more \code{iter} than the number of
#' elements in the original object.
#' 
#' @name jackknife
#' @aliases jackknife jackknife-methods jackknife,FLQuant-method
#' @docType methods
#' @section Generic function: jackknife(object, ...)
#' @author The FLR Team
#' @seealso \code{\link{FLQuant}}
#' @keywords methods
#' @examples
#' 
#' flq <- FLQuant(1:8)
#' flj <- jackknife(flq)
#' iters(flj)
#'

setMethod('jackknife', signature(object='FLQuant'),
  function(object, dim='year', na.rm=TRUE) {

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

    # FIND NAs along dim
    nas <- apply(object, dim, function(x) sum(is.na(x)))
    nas <- list(c(!nas == prod(dim(object)[-dim])))
    names(nas) <- c("i", "j", "k", "l", "m")[dim]
    
    res <- propagate(object, sum(nas[[1]]))
    dres <- dim(do.call("[", c(list(x=res), nas)))
  
    # permutation vector, place dim one before last
    perms <- rep(5, 5)
    perms[-dim] <- 1:4

    # create array with 1 at each location by iter
    idx <- aperm(array(c(rep(TRUE, prod(dres[-c(dim, 6)])),
      rep(FALSE, prod(dres[-6]))), dim=dres[c(seq(1, 6)[-dim], dim)]),
      c(perms, 6))
 
    # ASSIGN idx to corresponding dims in res   
    if(dim==1)
      res[nas[[1]], ][idx] <- NA
    if(dim==2)
      res[,nas[[1]], ][idx] <- NA
    if(dim==3)
      res[,,nas[[1]], ][idx] <- NA
    if(dim==4)
      res[,,,nas[[1]], ][idx] <- NA
    if(dim==5)
      res[,,,,nas[[1]], ][idx] <- NA

    res <- new("FLQuantJK", res, orig=object)

    return(res)
  }
)

setMethod('jackknife', signature(object='FLQuants'),
  function(object, ...) {

    # GET dims
    lens <- unlist(lapply(object, function(x) prod(dim(x))))

    # JK each element
    res <- lapply(object, jackknife, ...)

    # ADD copies of orig as length of other elements
    for(i in seq(lens)) {

      # before
      if(i > 1) {
        before <- propagate(object[[i]], sum(lens[seq(0, i-1)]))
        res[[i]] <- combine(before, res[[i]])
      }

      # after
      if(i < length(lens)) {
        after <- propagate(object[[i]], sum(lens[-seq(1, i)]))
        res[[i]] <- combine(res[[i]], after)
      }

      # CREATE FLQuantJK(s)
      res[[i]] <- new("FLQuantJK", res[[i]], orig=object[[i]])
    }

    return(res)
  }
)

setMethod("jackknife", signature(object="FLModel"),
  function(object, slot) {
    
    # RUN fit on orig
    ori <- fmle(object)

    # CHECK inputs are JK
    slot(object, slot) <- jackknife(slot(object, slot))

    # RUN fit on JK
    res <- fmle(object)

    # SET FLParJK @params
    params(res) <- FLParJK(params(res), orig=params(ori))

    # SET fitted
    fitted(res) <- FLQuantJK(fitted(res), orig=fitted(ori))

    # SET residuals

    # SET vcov

    return(res)

  }
) # }}}

# FLQuantJK {{{
setMethod("FLQuantJK", signature(object="ANY", orig="ANY"),
  function(object, orig) {
    return(new("FLQuantJK", object, orig=orig))
  }) # }}}

# FLParJK {{{
setMethod("FLParJK", signature(object="ANY", orig="ANY"),
  function(object, orig) {
    return(new("FLParJK", object, orig=orig))
  }) # }}}

# orig {{{
setMethod("orig", signature(object="FLQuantJK"),
  function(object) {
    return(object@orig)
  }
)

setMethod("orig", signature(object="FLParJK"),
  function(object) {
    return(object@orig)
  }
)

setMethod("orig", signature(object="FLQuants"),
  function(object) {
    return(lapply(object,
      function(x)
        if(is(x, "FLQuantJK"))
          orig(x)
        else
          x
      ))
  }
) # }}}

# apply {{{
setMethod("apply", signature(X="FLQuantJK", MARGIN="numeric", FUN="function"),
  function(X, MARGIN, FUN, ...) {
    return(apply(new('FLQuant', X@.Data, units=units(X)), MARGIN, FUN, ...))
  })

setMethod("apply", signature(X="FLParJK", MARGIN="numeric", FUN="function"),
  function(X, MARGIN, FUN, ...) {
    return(apply(new('FLPar', X@.Data, units=units(X)), MARGIN, FUN, ...))
  }) # }}}

# bias {{{
# $ \widehat{Bias}_{(\theta)} = (n - 1)((\frac{1}{n}\sum\limits_{i=1}^n\hat{\theta}_{(i)})-\hat{\theta}) $
setMethod("bias", signature(x="FLQuantJK"),
  function(x) {
      return((dim(x)[6] - 1) * (iterMeans(x) - orig(x)))
  }) 

setMethod("bias", signature(x="FLParJK"),
  function(x) {
    return((dim(x)[length(dim(x))] - 1) * (iterMeans(FLPar(x@.Data)) - orig(x)))
  }) # }}}

# var {{{
# $ var = \frac{n-1}{n}\sum\limits_{i=1}^n (\bar{x}_i - (\frac{1}{n} \sum\limits_{i}^n \bar{x}_i))^2 $
setMethod("var", signature(x="FLQuantJK"),
  function(x) {
    ns <- dim(x)[6]
    return(((ns - 1) / ns) * iterSums((x - iterMeans(x))^2))
  }) 

setMethod("var", signature(x="FLParJK"),
  function(x) {
    ns <- dim(x)[length(dim(x))]
    return(((ns - 1) / ns) * iterSums((x - propagate(iterMeans(x), ns)) ^ 2))
  }) # }}}

# corrected {{{
# $ \hat{\theta}_{jack} = n\hat{\theta} - (n - 1)\hat{\theta}_{(.)} $
setMethod("corrected", signature(x="FLQuantJK"),
  function(x) {
    return(dim(x)[6] * orig(x) - (dim(x)[6]-1) * iterMeans(x))
  }) 

setMethod("corrected", signature(x="FLParJK"),
  function(x) {
    return(dim(x)[length(dim(x))] * orig(x) - (dim(x)[length(dim(x))]-1) * iterMeans(x))
  }) 

# }}}

# cov {{{
setMethod("cov", signature(x="FLParJK", y="missing"),
  function(x) {
    ns <- dim(x)[length(dim(x))]
    return(FLPar(cov(model.frame(x)[, dimnames(x)[[1]]]) * (ns - 1) * (ns - 1) / ns))
  }) # }}}

# cor {{{
setMethod("cor", signature(x="FLParJK", y="missing"),
  function(x) {
    return(FLPar(cor(cov(x)[drop=TRUE])))
  }) # }}}

# window {{{
setMethod("window", signature(x="FLQuantJK"),
  function(x, ...) {
    new("FLQuantJK",
      window(FLQuant(x@.Data), ...),
      orig=window(orig(x), ...), units=units(x))
  }) # }}}
