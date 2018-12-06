# FLArray- Base class for FLQuant and FLCohort
# FLCore/R/FLarray.R

# Copyright 2003-2016 FLR Team. Distributed under the GPL 2 or later
# Maintainer: Iago Mosqueira, EC JRC

# units {{{

#' Method units
#'
#' \code{units} attribute for FLQuant and FLArray-derived objects
#' 
#' Objects of \code{FLArray}-based classes (e.g. \code{\link{FLQuant}}) contain a
#' \code{units} attribute of class \code{character}. This should be used to store
#' the corresponding units of measurement.  This attribute can be directly accessed
#' and modified using the \code{units} and \code{units<-} methods.
#' 
#' For complex objects, \code{units} will return a named list containing the
#' attributes of all \code{FLQuant} slots. \code{units} of a complex object can
#' be modified for all slots or a subset of them, by passing a named list with
#' the new values. See examples below.
#'
#' @name units
#' @aliases units,FLArray-method units<-,FLArray,character-method
#' @docType methods
#' @section Generic function: units(x)
#' 
#' units<-(x,value)
#' @author The FLR Team
#' @seealso \linkS4class{FLQuant}, \linkS4class{FLPar}, \linkS4class{FLCohort}
#' @keywords methods
#' @examples
#' 
#' flq <- FLQuant(rnorm(100), dim=c(5,20), units='kg')
#' units(flq)
#' units(flq) <- 't'
#' summary(flq)
#' 
#' # units for a complex object
#'   data(ple4)
#'   units(ple4)
#'   units(ple4) <- list(harvest='hr')
#'

setMethod("units", signature(x="FLArray"),
  function(x)
    return(x@units)
) # }}}

# units<- {{{
setMethod("units<-", signature(x="FLArray", value="character"),
  function(x, value) {
    x@units <- value
    return(x)
  }
) # }}}

# quant, quant<-        {{{

#' @rdname quant
#' @aliases quant,FLArray-method
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
setMethod("quant", signature(object="FLArray"),
  function(object)
  {
    return(names(dimnames(object))[1])
  }
)
#' @rdname quant
#' @aliases quant<-,FLArray,character-method
setMethod("quant<-", signature(object="FLArray", value='character'),
  function(object, value)
  {
    if(length(value) > 1)
      stop('quant must be a single character string')
    names(attributes(object)$dimnames) <- c(value, names(dimnames(object))[-1])
    return(object)
  }
) # }}}

#  [             {{{

#' Extract
#'
#' Extract or replace parts of an FLR Object
#' 
#' Operators acting on FLQuant, FLCohort, FLPar, FLComp, and derived classes to
#' extract or replace sections of an object.
#' 
#' Please note the differences between referencing sections of an object by
#' position using values of class \code{numeric}, or by using dimnames of class
#' \code{character}. See examples below.
#' 
#' All classes that are derived from \code{FLComp} (for example, \code{FLStock}
#' and \code{FLBiol}) can be subset along the six dimensions of their
#' \code{FLQuant} slots.
#' 
#' Classes that are derived from \code{FLlst} (for example, \code{FLStocks} and
#' \code{FLBiols}) can be subset in a similar way to ordinary list objects.
#'
#' '$' for the \code{FLPar} and \code{FLQuant} classes operate only along the first
#' dimension ('params' or 'quant'), and are provided to be used specially in
#' formulas.
#'
#' @name Extract
#' @rdname Extract
#' @aliases [,FLArray,ANY,ANY,ANY-method
#' @docType methods
#' @section Generic function: \describe{ \item{}{[x,i,j,drop]}
#' \item{}{[<-(x,i,j,value)} \item{}{[[<-(x,i,j,value)}
#' \item{}{\$<-(x,name,value)} }
#' @param x object from which to extract or replace element(s)
#' @param i,j,k,l,m,n indices specifying elements to extract or replace on any of the six dimensions.
#' @param ... indices specifying elements to extract or replace by dimension name.
#' @param drop If 'TRUE' the result is coerced to the lowest possible dimension, and so
#' might change class (e.g. drop='TRUE' on an \code{FLQuant} might return an
#' \code{array} of less dimensions, a \code{matrix} or a \code{vector}.
#' @param value An object of the same class, or simpler if \code{drop=TRUE}, than 'x'.
#' @param name
#' See \link[base]{Extract} for further details.
#' @author The FLR Team
#' @seealso \link[base]{Extract}
#' @keywords methods
#' @examples
#' 
#' flq <- FLQuant(rnorm(200), dimnames=list(age=0:4, year=1991:2000,
#'   season=1:4))
#'
#' # Extracting by position...
#'   flq[1,]
#'   flq[,1:5]
#'   flq[1:2,,,c(1,3)]
#'
#' # ...by dimnames
#'   flq['0',]
#'   flq[,'1991']
#'   flq[,as.character(1991:1995),,'1']
#'
#' # Dimensions of length one can be drop
#'   flq[1, drop=TRUE]
#'
#' # Replacing part of the object
#'   flq['0',,,1]<-0
#'
setMethod("[", signature(x="FLArray"),
    function(x, i, j, k, l, m, n, ..., drop=FALSE) {
   
      # INTERPRET i as vector of elements
      if(!missing(i) && missing(j) && missing(k) && missing(l) &&
        missing(m) && missing(n) && drop) {
        return(x@.Data[i, drop=TRUE])
      }

      # PARSE named arguments
      if(missing(i) && missing(j) && missing(k) && missing(l) &&
        missing(m) && missing(n)) {
      
        args <- list(...)
        
        if(length(args) > 0) {
          nms <- names(args)
          names(args) <- c("i", "j", "k", "l", "m", "n")[match(nms, names(x))]
          return(do.call("[", c(list(x=x), args, list(drop=drop))))
        }
      }

      dx <- dim(x)
      
      if (missing(i))
        i  <-  seq(1, dx[1])
      if (missing(j))
        j  <-  seq(1, dx[2])
      if (missing(k))
        k  <-  seq(1, dx[3])
      if (missing(l))
        l  <-  seq(1, dx[4])
      if (missing(m))
        m  <-  seq(1, dx[5])
      if (missing(n))
        n  <-  seq(1, dx[6])

      if(drop)
        return(x@.Data[i, j, k, l, m, n, drop=TRUE])
      else
        x@.Data <- x@.Data[i, j, k, l, m, n, drop=FALSE]
      return(x)
  }
)

#' @rdname Extract
#' @aliases [,FLArray,array,missing,missing-method
setMethod("[", signature(x="FLArray", i="array", j="missing", drop="missing"),
  function(x, i)
  {
    dimn <- dimnames(i)
    for(d in 1:6) {
      dimn[[d]] <- dimn[[d]][apply(i@.Data, d, any, FALSE)==TRUE]
    }
    if(length(x@.Data[i]) != prod(unlist(lapply(dimn, length)))) {
        warning("Selected elements do not form a coherent 6D array")
      return(x@.Data[i])
    } else {
      return(new(class(x), array(x@.Data[i], dimnames=dimn,
        dim=unlist(lapply(dimn, length)))))
    }
  }
)   # }}}

# [<-            {{{
#' @rdname Extract
#' @aliases `[<-,FLArray,ANY,ANY,ANY-method`
setMethod("[<-", signature(x="FLArray"),
  function(x, i, j, k, l, m, n, ..., value)
  {
    if(length(list(...)) > 0)
      stop(paste(class(x), 'objects only have 6 dimensions'))

    if(!missing(i) && is.array(i))
    {
    x@.Data[i] <- value
      return(x)
    }
    
    dx <- dim(x)

    if (missing(i))
      i  <-  seq(1, dx[1])
    if (missing(j))
      j  <-  seq(1, dx[2])
    if (missing(k))
      k  <-  seq(1, dx[3])
    if (missing(l))
      l  <-  seq(1, dx[4])
    if (missing(m))
      m  <-  seq(1, dx[5])
    if (missing(n))
      n  <-  seq(1, dx[6])

    x@.Data[i,j,k,l,m,n] <- value

    return(x)
  }
) 

#' @rdname Extract
#' @aliases `[<-,FLArray,ANY,ANY,FLArray-method`
setMethod("[<-", signature(x="FLArray", value="FLArray"),
  function(x, i, j, k, l, m, n, ..., value)
  {
    # check dims !> 6
    if(length(list(...)) > 0)
      stop(paste(class(x), 'objects only have 6 dimensions'))

    # array (logical) used to index
    if(!missing(i) && is.array(i))
    {
    x@.Data[i] <- value
      return(x)
    }
    
    # default dims = all
    dx <- dim(x)
    if (missing(i))
      i  <-  seq(1, dx[1])
    if (missing(j))
      j  <-  seq(1, dx[2])
    if (missing(k))
      k  <-  seq(1, dx[3])
    if (missing(l))
      l  <-  seq(1, dx[4])
    if (missing(m))
      m  <-  seq(1, dx[5])
    if (missing(n))
      n  <-  seq(1, dx[6])

    # aperm array, not common dims last
    same <- which(dim(value) == dim(x))
    diff <- which(dim(value) != dim(x))
    dper <- c(same, diff)
    y <- aperm(x, dper)
    
    # 
    iper <- list(i, j, k, l, m, n)[dper]
    names(iper) <- c('i','j','k','l','m','n')
    
    # call [<-
    y <- do.call('[<-', c(list(x=y), iper, list(value=aperm(unname(value), dper))))

    # re-aperm
    y <- aperm(y, order(dper))

     return(new(class(x), y, units=units(x)))
  }
)   # }}}

# names         {{{

#' Method names
#' 
#' The \code{names} method returns the names of the dimnames of an object. For
#' some classes, the names attribute can be modified directly using names<-.
#'
#' @name names
#' @aliases names,FLArray-method
#' @docType methods
#' @section Generic function: names(x) names<-(x, value)
#' @author The FLR Team
#' @seealso \link[base]{names}
#' @keywords methods
#' @examples
#' # FLQuant
#' data(ple4)
#' names(catch.n(ple4))
#'
#' # Contrast this with
#' dimnames(catch.n(ple4))
#'
setMethod("names", signature(x="FLArray"),
  function(x)
    names(dimnames(x))
)
# }}}

# iter     {{{

#' @rdname iter
#' @aliases iter,FLArray-method iter,FLQuant,ANY-method iter,FLCohort,ANY-method
#' @examples
#'
#' # For an FLQuant
#'   flq <- FLQuant(rnorm(800), dim=c(4,10,2), iter=10)
#'   iter(flq, 2)
#'
#' # For the more complex FLStock object
#'   fls <- FLStock(catch.n=flq, m=FLQuant(0.2, dim=c(4,10,2)))
#'   summary(fls)
#'
#'   # Extraction using iter...
#'     fls2 <- iter(fls, 2)
#'     summary(fls2)
#'   # ...in contrast to using [ which returns an error
#'     \dontrun{fls[,,,,,2]}
#' 
setMethod("iter", signature(obj="FLArray"),
  function(obj, iter) {
    if(dims(obj)$iter == 1)
      return(obj)
    else
      return(obj[,,,,,iter])
  }
)   # }}}

# summary          {{{

#' Method summary
#' 
#' Outputs a general summary of the structure and content of an fwdControl
#' object. The method invisibly returns the data.frame shown on screen.
#'
#' @rdname summary-methods
#' @aliases summary,FLArray-method
#' @docType methods
#' @section Generic function: summary(object)
#' @author The FLR Team
#' @seealso \link[base]{summary}
#' @keywords methods
#' @examples
#' 
#' flq <- FLQuant(rlnorm(90), dim=c(3,10), units='kg')
#' summary(flq)
#'
#' data(ple4)
#' summary(ple4)
#' 
#' data(nsher)
#' summary(nsher)
#'
setMethod("summary", signature(object="FLArray"),
  function(object, ...)
  {
    cat("An object of class \"", as.character(class(object)), "\" with:\n", sep="")
    cat("dim  : ", dim(object), "\n")
    cat("quant: ", quant(object), "\n")
    cat("units: ", units(object), "\n\n")
    if(all(is.na(object)))
    {
      cat("Min    :  NA\n")
      cat("1st Qu.:  NA\n")
      cat("Mean   :  NA\n")
      cat("Median :  NA\n")
      cat("3rd Qu.:  NA\n")
      cat("Max    :  NA\n")
    }
    else
    {
      cat("Min    : ", min(object, na.rm=TRUE), "\n")
      cat("1st Qu.: ", quantile(as.vector(object), 0.25, na.rm=TRUE), "\n")
      cat("Mean   : ", mean(as.vector(object), na.rm=TRUE), "\n")
      cat("Median : ", median(as.vector(object), na.rm=TRUE), "\n")
      cat("3rd Qu.: ", quantile(as.vector(object), 0.75, na.rm=TRUE), "\n")
      cat("Max    : ", max(object, na.rm=TRUE), "\n")
    }
    cat("NAs    : ", format(length(as.vector(object)
      [!complete.cases(as.vector(object))])/length(as.vector(object))*100,
      digits=2), "%\n")
  }
)   # }}}

# show     {{{

#' Method show
#' 
#' Standard display of an object contents in an interactive session. Objects of
#' class \code{\linkS4class{FLQuant}} with length > 1 along the sixth dimension
#' (\emph{iter}) are output in a summarised form, as \code{median(mad)}, where
#' mad is the median absolute deviation. See \code{\link[stats]{mad}}.
#' 
#' The same format is used for objects of class \code{\linkS4class{FLPar}} with
#' length > 1 on the last dimension (\emph{iter}).
#'
#' @name show
#' @aliases show,FLArray-method
#' @aliases show,FLQuants-method show,FLPar-method
#' @docType methods
#' @section Generic function: show(object)
#' @author The FLR Team
#' @seealso \link{FLComp}
#' @keywords methods
#' @examples
#' 
#' # no 'iter'
#'   flq <- FLQuant(rnorm(80), dim=c(4,20), quant='age', units='kg')
#'   flq
#' 
#' # with 'iter'
#'   flq <- FLQuant(rnorm(800), dim=c(4,20,1,1,1,10), quant='age', units='kg')
#'   flq
#'

setMethod("show", signature(object="FLArray"),
  function(object){
    cat("An object of class \"", as.character(class(object)), "\"\n", sep="")
    if(dim(object)[6] != 1)
      cat("iters: ", dim(object)[6],"\n\n")
    if(dim(object)[6] > 1)
    {
      v1 <- apply(object@.Data, 1:5, median, na.rm=TRUE)
      v2 <- apply(object@.Data, 1:5, mad, na.rm=TRUE)   
      v3 <- paste(format(v1,digits=5),"(", format(v2, digits=3), ")", sep="")
    }
    else
      v3 <- paste(format(apply(object@.Data, 1:5, median, na.rm=TRUE),digits=5))
    
    print(array(v3, dim=dim(object)[1:5], dimnames=dimnames(object)[1:5]), quote=FALSE)
  }
)   # }}}

# trim {{{

#' @rdname trim
#' @aliases trim,FLArray-method
#' @examples
#' 
#' flq <- FLQuant(rnorm(90), dimnames=list(age=1:10, year=2000:2016))
#' 
#' trim(flq, year=2000:2005)
#' # which is equivalent to
#' window(flq, start=2000, end=2005)
#'
#' trim(flq, year=2000:2005, age=1:2)
#' 

setMethod('trim', signature(x='FLArray'),
  function(x, ...)
  {
    args <- list(...)
    nargs <- names(args)

    # dimension names
    qnames <- names(dimnames(x))
    
    # check input names match dimnames
    if(!all(nargs%in%qnames))
      stop(paste("Wrong dimension name provided: ", nargs[!nargs%in%qnames]))

    # Create list with given standard elements in right position ...
    select <- args[match(qnames, nargs)]

    # change names to those for '['
    names(select) <- c('i', 'j', 'k', 'l', 'm', 'n')
    
    # get rid of unspecified dimensions
    select <- select[!unlist(lapply(select, is.null))]
    
    # turn into characters
    select <- lapply(select, as.character)
    
    do.call('[', c(list(x=x), select))
  }
) # }}}

# expand {{{
setMethod('expand', signature(x='FLArray'),
  function(x, ..., fill=FALSE) {

    args <- list(...)
    dnx <- dimnames(x)
    
    # dimension names
    nargs <- names(args)
    qnames <- names(dnx)
    
    # check input names match dimnames
    if(!all(nargs %in% qnames))
      stop(paste("Wrong dimension name provided: ", nargs[!nargs%in%qnames]))

    # turn into characters
    select <- lapply(args, as.character)
    
    # match specified dimensions and dimnames
    dimnames <- dnx
    
    # new dimnames
    dimnames[names(select)] <- select

    # output object
    res <- new(class(x), array(as.numeric(NA), dimnames=dimnames,
      dim=unlist(lapply(dimnames, length))), units=units(x))
    
    # list for assignment of x data
    dimnames <- dimnames(res)

    # extended or new?
    if(!fill) {
      for(i in nargs) {
      
        # are all old dimnames in the new ones?
        idx <- all(dnx[[i]] %in% dimnames[[i]])
        # if so, recover them
        if(idx) {
          dimnames[[i]] <- dnx[[i]]
        } else {
          if(length(dnx[[i]]) > 1) {
            stop("trying to expand to new dims where existing have length > 1", i)
          }
        }
      }
    }
    
    # list names to match '[<-' signature
    names(dimnames) <- c('i', 'j', 'k', 'l', 'm', 'n')

    return(do.call('[<-', c(list(x=res, value=x), dimnames)))
  }
) # }}}

# Arith    {{{

#' Arithmetic operators for FLCore classes
#' 
#' Overloaded arithmetic operators for FLCore classes
#'
#' These methods apply the standard arithmetic operators included in the
#' \code{\link[methods]{Arith}} group ("+", "-", "*", "^", "%%", "%/%", and
#' "/"), so that they return an object of the appropriate class.
#'
#' When the operation involves objects of two classes (e.g. [`FLPar`] and [`FLQuant`]),
#' the class is the returned object is that of the more complexs object, in this
#' case [`FLQuant`].
#'
#' @rdname Arith-methods
#' @md
#' @author The FLR Team
#' @seealso [methods::Arith] [base::Arithmetic]
#' @keywords methods
#' @examples
#' 
#' flq <- FLQuant(rlnorm(90), dim=c(3,10), units='kg')
#' flp <- FLPar(a=99)
#'
#' # FLQuant and numeric
#' flq * 25
#' # Two FLQuant objects
#' flq + flq
#'
setMethod("Arith", #  "+", "-", "*", "^", "%%", "%/%", "/"
  signature(e1 = "numeric", e2 = "FLArray"),
  function(e1, e2)
  {
    return(new(class(e2), callGeneric(e1, e2@.Data), units=units(e2)))
  }
)

#' @rdname Arith-methods
setMethod("Arith",
  signature(e1 = "FLArray", e2 = "numeric"),
  function(e1, e2)
  {
    if(length(e2) == 1 & !is.na(match(as.character(e2)[1], uoms[nums])))
      units <- uom(as.character(get('.Generic')), units(e1), as.character(e2))
    else
      units <- units(e1)

    return(new(class(e1), callGeneric(e1@.Data, e2), units=units))
  }
)

#' @rdname Arith-methods
setMethod("Arith",
  signature(e1 = "FLArray", e2 = "FLArray"),
  function(e1, e2)
  {
    if(!all(dim(e1)[-6] == dim(e2)[-6]))
      stop("non-conformable arrays")

    if(dim(e1)[6] == 1 & dim(e2)[6] > 1) {
      e <- e2
      e[,,,,,] <- e1
      e <- array(callGeneric(unclass(e), unclass(e2)),
        dimnames=dimnames(e2), dim=dim(e2))
    }
    else if(dim(e2)[6] == 1 & dim(e1)[6] > 1) {
      e <- e1
      e[,,,,,] <- e2
      e <- array(callGeneric(unclass(e1), unclass(e)),
        dimnames=dimnames(e1), dim=dim(e1))
    }
    else
      e <- array(callGeneric(drop(unclass(e1)), drop(unclass(e2))),
        dimnames=dimnames(e1), dim=dim(e1))
    
    # units
    op <- as.character(get('.Generic'))
    if(op == "^")
      units <- units(e1)
    else
      units <- uom(op, units(e1), units(e2))
    
    return(new(class(e1), e, units=units))
  }
)   # }}}

# as.data.frame        {{{
setMethod("as.data.frame", signature(x="FLArray", row.names="missing",
  optional="missing"),
  function(x) {
    as(x, 'data.frame')
  }
)
setMethod("as.data.frame", signature(x="FLArray", row.names="ANY",
  optional="missing"),
  function(x, row.names=NULL) {
    df <- as(x, 'data.frame')
    row.names(df) <- row.names
    return(df)
  }
) # }}}

# scale {{{
setMethod("scale", signature(x="FLArray", center="ANY", scale="ANY"),
  function(x, center, scale)
  {
    new(class(x), array(scale(x@.Data, center=center, scale=scale), dim=dim(x),
      dimnames=dimnames(x)), units=units(x))
  }
)

setMethod("scale", signature(x="FLArray", center="ANY", scale="missing"),
  function(x, center)
  {
    new(class(x), array(scale(x@.Data, center=center, scale=TRUE), dim=dim(x),
      dimnames=dimnames(x)), units=units(x))
  }
)

setMethod("scale", signature(x="FLArray", center="missing", scale="ANY"),
  function(x, scale)
  {
    new(class(x), array(scale(x@.Data, center=TRUE, scale=scale), dim=dim(x),
      dimnames=dimnames(x)), units=units(x))
  }
)

setMethod("scale", signature(x="FLArray", center="missing", scale="missing"),
  function(x)
  {
    new(class(x), array(scale(x@.Data, center=TRUE, scale=TRUE), dim=dim(x),
      dimnames=dimnames(x)), units=units(x))
  }
) # }}}

# sweep {{{

#' Method sweep for FLCore classes
#' 
#' Use R's sweep method on FLCore classes
#'
#' These methods call base R \code{\link[base]{sweep}} method on **FLCore** classes and then ensure
#' that the returned object is of same class.
#'
#' @rdname sweep-methods
#' @docType methods
#' @section Generic function: sweep(x, MARGIN, STATS, FUN = "-", check.margin = TRUE, ...)
#' @author The FLR Team
#' @seealso \link[base]{sweep}
#' @keywords methods
#' @examples
#' 
#' flq <- FLQuant(rlnorm(90), dim=c(3,10), units='kg')
#' # Get ratio of max value by year
#' sweep(flq, 2, apply(flq, 2, max), "/")

setMethod('sweep', signature(x='FLArray'),
  function(x, MARGIN, STATS, FUN, check.margin=TRUE, ...)
  {
    res <- callNextMethod()
    do.call(class(x), list(res, units=units(x)))
  }
) # }}}

# sigma {{{
setMethod('sigma', signature(object='FLArray'),
  function(object, hat=rep(0, length(object)))
  {
    # calculates sigma squared for use in concentrated likelihood
    if(all(is.na(hat)))
      return(Inf)

    SS <- sum((object - hat) ^ 2, na.rm=TRUE)

    return((SS/length(hat[!is.na(hat)])) ^ 0.5)
   }
) # }}}

# qmax, qmin {{{
setMethod("qmax", signature(x="FLArray"),
  function(x, ..., na.rm=TRUE)
  {
    args <- c(list(x), list(...))
    args <- lapply(args, function(x) x@.Data)
    res <- do.call(pmax, args)
    do.call(class(x), list(object=res, units=units(x)))
  }
) 
setMethod("qmin", signature(x="FLArray"),
  function(x, ..., na.rm=TRUE) 
  {
    args <- c(list(x), list(...))
    args <- lapply(args, function(x) x@.Data)
    res <- do.call(pmin, args)
    do.call(class(x), list(object=res, units=units(x)))
  }
)

# }}}

# apply {{{

#' apply method for FLCore classes
#'
#' Applies a function over the margins of an array-based FLCore class
#' 
#' These methods call R's [base::apply] on an [FLArray] the standard arithmetic operators included in the
#' \code{\link[methods]{Arith}} group ("+", "-", "*", `"^", "%%", "%/%", and
#' "/"), so that they return an object of the appropriate class.
#'
#' When the operation involves objects of two classes (e.g. [`FLPar`] and [`FLQuant`]),
#' the class is the returned object is that of the more complexs object, in this
#' case [`FLQuant`].
#'
#' @rdname apply-methods
#' @md
#' @author The FLR Team
#' @seealso [base::apply]
#' @keywords methods
#' @examples
#' 
#' flq <- FLQuant(rlnorm(90), dim=c(3,10), units='kg')
#' flp <- FLPar(a=99)
#'
#' # FLQuant and numeric
#' flq * 25
#' # Two FLQuant objects
#' flq + flq
#'

setMethod("apply", signature(X="FLArray", MARGIN="numeric", FUN="function"),
  function(X, MARGIN, FUN, ...) {

  data <- apply(X@.Data, MARGIN, FUN, ...)
  
  if(length(dim(data))<=length(MARGIN)){

    # set dim
    dim <- c(1,1,1,1,1,1)
    # if apply generated a new dimension
    if (is.null(dim(data)))
      dim[MARGIN] <- length(data)
    else
      dim[MARGIN] <- dim(data)
    # new object
    flq <- array(NA, dim=dim)
    # inject data
    flq[1:dim[1],1:dim[2],1:dim[3],1:dim[4],1:dim[5],1:dim[6]] <- data
    # set dimnames
    MRG <- dim(X) == dim(flq)
    if(all(MRG))
      dimnames(flq) <- dimnames(X)
    else
    {
      dimnames(flq)[MRG] <- dimnames(X)[MRG]
      dimnames(flq)[!MRG] <- dimnames(new(class(X)))[!MRG]
      names(dimnames(flq)) <- names(dimnames(X))
    } 
    # new FLobject
    flq <- new(class(X), flq, units=units(X))
    # set quant
    if(is(flq, 'FLQuant')) quant(flq) <- quant(X)
    return(flq)
  } else {
    X[] <- data
    return(X)
  }
})   # }}}

# survprob {{{
# estimate survival probabilities by year or cohort
setMethod("survprob", signature(object="FLArray"),
  function(object, ...) {
    
    ps <- mm <- object

    # estimate by year
      ps[1,,,,,] <- 1  
      for(a in 2:dim(ps)[1])
        ps[a,,,,,] <- ps[a-1,,,,,]*exp(-mm[a-1,,,,,])

    return(ps)
  }
) # }}}

# window           {{{
setMethod("window", signature(x="FLArray"),
  function(x, start=dims(x)$minyear, end=dims(x)$maxyear, extend=TRUE, frequency=1)
  {
    # get original min and max
    min <- dims(x)$minyear
    max <- dims(x)$maxyear

    # if extend=FALSE and end/start ask for it, error
    if(!extend && (start < min | end > max))
      stop("FLQuant to be extended but extend=FALSE")

    # if extend is a number, added to end
    if(is.numeric(extend))
        if (missing(end))
          end <- dims(x)$maxyear + extend
        else
          stop("'extend' is numeric and 'end' provided, don't know what to do")
    
    # construct new FLQuant
    years <- seq(start, end, by=frequency)
    dnames <- dimnames(x)
    dnames[[2]] <- years
    flq <- do.call(class(x), list(NA, units=units(x), dimnames=dnames))

    # add data for matching years
    flq[,dimnames(x)$year[dimnames(x)$year%in%as.character(years)],,,]  <-
      x[,dimnames(x)$year[dimnames(x)$year%in%as.character(years)],,,]

    return(flq)
  }
)   # }}}

# cv        {{{
setMethod("cv", signature(x="FLArray"),
  function(x, na.rm=TRUE){
    return(sd(c(x), na.rm=na.rm) / mean((x), na.rm=na.rm))
  }
)   # }}}

# subset {{{
setMethod('subset', signature(x='FLArray'),
  function(x, ...) {
    x <- as.data.frame(x, cohort=TRUE)
    subset(x, ...)
  }
) # }}}

# median        {{{
setMethod("median", signature(x="FLArray"),
  function(x, na.rm=TRUE){
    return(median(c(x), na.rm=na.rm))
  }
)   # }}}

# dim {{{
setMethod("dim", signature(x="FLArray"),
  function(x) {
    return(unname(dim(x@.Data)))
  }
) # }}}

# drop {{{

#' drop method for FLCore array-based classes
#'
#' Delete the dimensions of an array which have only one level.
#' 
#' This method calls R's [base::drop] on the `@.Data` slot of an [FLArray].
#' Dimensions of length one are thus dropped, as is the class attribute and the
#' `units` slot, and an array of equal or less
#' dimensions, a matrix or a vector is returned.
#'
#' On an FLQuant object with
#'
#' @rdname drop-methods
#' @aliases drop,FLQuant-method
#' @md
#' @author The FLR Team
#' @seealso [base::drop]
#' @keywords methods
#' @examples
#' x <- FLQuant(1:3, dim=c(3,3))
#' drop(x)
#' is(drop(x))
#' dim(drop(x))
#'
#' # Result of drop can be used for matrix algebra
#' # for example to calculate aging error
#'
#' data(ple4)
#' aging.error <- diag(0.8, 10)
#' diag(aging.error[-1,]) <- c(rep(0.1, 8), 0.2)
#' diag(aging.error[, -1]) <- c(0.2, rep(0.1, 8))
#' t(aging.error) %*% drop(catch.n(ple4))

setMethod("drop", signature(x="FLArray"),
  function(x) {
    return(drop(x@.Data))   
  }
) # }}}

# exp & log {{{

#' exp and log methods FLCore array-based classes
#'
#' Compute the exponential and logarithmic functions
#' 
#' This method simply calls R's [base::exp] and [base::drop], but take care of
#' returning the right units of measurement, that is "" or character(1).
#'
#' @rdname exp-methods
#' @aliases exp,FLQuant-method
#' @md
#' @author The FLR Team
#' @seealso [base::exp] [base::log]
#' @keywords methods
#' @examples
#' x <- FLQuant(c(4,2,7,4,2,9), units="1000")
#' log(x)
#' units(log(x))

setMethod("exp", signature(x="FLQuant"),
  function(x) {
    res <- callNextMethod()
    units(res) <- ""
    return(res)
  })

#' @rdname exp-methods
#' @aliases log,FLQuant-method
setMethod("log", signature(x="FLQuant"),
  function(x, ...) {
    res <- callNextMethod()
    units(res) <- ""
    return(res)
  }) # }}}

# dbind {{{

#' @rdname dbind-methods
#' @param dim Dimension to bind on, *numeric* or *character*.
#' @examples
#'
#' # By iter
#' x <- FLQuant(rnorm(80000), dim=c(4,20,1,1,1,1000))
#' y <- FLQuant(rnorm(80000), dim=c(4,20,1,1,1,1000))
#'   dimnames(y) <- list(iter=1001:2000)
#' ibind(x,y)
#' 
#' # By quant (age)
#' x <- FLQuant(1, dimnames=list(age=1:3, year=1:10))
#' y <- FLQuant(2, dimnames=list(age=4:12, year=1:10))
#' qbind(x, y)
#' 
#' # By year
#' x <- FLQuant(1, dimnames=list(age=1:3, year=1:10))
#' y <- FLQuant(2, dimnames=list(age=1:3, year=11:20))
#' z <- FLQuant(3, dimnames=list(age=1:3, year=21:30))
#' ybind(x, y, z)
#' 
#' # By season
#' x <- FLQuant(1, dimnames=list(year=1:10, season=1:2))
#' y <- FLQuant(2, dimnames=list(year=1:10, season=3:4))
#' sbind(x, y)
 
setMethod('dbind', signature(x='FLArray', y='FLArray'),
  function(x, y, ..., dim=1) {

    args <- c(list(x=x, y=y), list(...))
    
    # Input dim and dimnames
    dms <- lapply(args, dim)
    dnms <- lapply(args, dimnames)
    
    # new dim
    dms[[1]][dim] <- sum(unlist(lapply(dms, `[`, dim)))
    ndms <- dms[[1]]
    
    # CHECK names(dimnames) match
    if(!all.equal(Reduce(intersect, lapply(dnms, names)), names(dnms[[1]])))
       stop("Names of dimnames must match")
    
    # CHECK dimnames[dim] do not match
    nnms <- lapply(dnms, `[`, dim)
    if(any(Vectorize(identical, 'x')(nnms[-1], nnms[1])))
      stop("dimnames to combine must be different")
    
    # CHECK dimnames[-c(dim, iter)] match
    onms <- lapply(dnms, `[`, -dim)
    if(!all(Vectorize(identical, 'x')(onms[-1], onms[[1]])))
      stop("dimnames across all object must match for common dimensions")

    # names
    nams <- lapply(dnms, `[`, dim)

    # aperm to join by dim
    idx <- c(seq(1, 6)[-dim], dim)
    pargs <- lapply(args, aperm, idx)
    ndms <- ndms[idx]

    # then re-aperm
    idx <- match(seq(1, 6), idx)
    res <- aperm(array(unlist(pargs), dim=ndms), idx)

    # dimnames
    dmns <- dimnames(x)
    dmns[[dim]] <- unname(unlist(lapply(dnms, "[[", dim)))
    dimnames(res) <- dmns

    x@.Data <- res
    return(x)
  }
)

#' @rdname dbind-methods
#' @aliases qbind
qbind <- function(...)
  dbind(..., dim=1)

#' @rdname dbind-methods
#' @aliases ybind
ybind <- function(...)
  dbind(..., dim=2)

#' @rdname dbind-methods
#' @aliases ubind
ubind <- function(...)
  dbind(..., dim=3)

#' @rdname dbind-methods
#' @aliases sbind
sbind <- function(...)
  dbind(..., dim=4)

#' @rdname dbind-methods
#' @aliases abind
abind <- function(...)
  dbind(..., dim=5)

#' @rdname dbind-methods
#' @aliases ibind
ibind <- function(...)
  dbind(..., dim=6)

 # }}}
