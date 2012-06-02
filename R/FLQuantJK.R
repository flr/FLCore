
# FLQuantJK.R - FLQuantJK class and methods
# FLCore/R/FLQuantJK.R

# Copyright 2003-2007 FLR Team. Distributed under the GPL 2 or later
# Maintainer: Iago Mosqueira, JRC
# $Id: FLQuant.R 1348 2011-10-28 06:52:43Z lauriekell $


# FLQuantJK     {{{
setClass("FLQuantJK",
	representation("FLQuant", orig="FLQuant"),
  prototype(new("FLQuant"), orig=new("FLQuant")))
# }}}

# jackknife  {{{
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

