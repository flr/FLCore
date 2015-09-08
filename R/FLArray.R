# FLArray-class - Base class for FLQuant and FLCohort

# Copyright 2003-2008 FLR Team. Distributed under the GPL 2 or later
# Maintainer: Iago Mosqueira, JRC

# units {{{
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

# quant        {{{
setMethod("quant", signature(object="FLArray"),
	function(object)
  {
		return(names(dimnames(object))[1])
	}
) # }}}

# quant<-      {{{
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
#' @name Extract
#' @rdname Extract
#' @aliases [,FLArray,ANY,ANY,ANY-method
#' @docType methods
#' @section Generic function: \describe{ \item{}{[x,i,j,drop]}
#' \item{}{[<-(x,i,j,value)} \item{}{[[<-(x,i,j,value)}
#' \item{}{\$<-(x,name,value)} }
#' @param x object from which to extract or replace element(s)
#' @param i,j,k,l,m,n,... indices specifying elements to extract or replace.
#' @param drop If 'TRUE' the result is coerced to the lowest possible dimension, and so
#' might chmnage class (e.g. drop='TRUE' on an \code{FLQuant} might return an \code{array}.
#' @param value An object of a similar or simpler class than 'x'.
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
#' # Replacing part of the object
#'   flq['0',,,1]<-0
#'
setMethod("[", signature(x="FLArray"),
    function(x, i, j, k, l, m, n, ..., drop=FALSE)
    {
      if(length(list(...)) > 0)
        stop(paste(class(x), 'objects only have 6 dimensions'))
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
    for(d in 1:6)
      dimn[[d]] <- dimn[[d]][apply(i@.Data, d, any, FALSE)==TRUE]

    if(length(x@.Data[i]) != prod(unlist(lapply(dimn, length)))) {
      	warning("Selected elements do not form a coherent 6D array")
      return(x@.Data[i])
		} else {
      return(new(class(x), array(x@.Data[i], dimnames=dimn,
        dim=unlist(lapply(dimn, length)))))
		}
  }
)   # }}}

# "[<-"            {{{
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

    #
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
setMethod("names", signature(x="FLArray"),
	function(x)
    names(dimnames(x))
)
# }}}

# iter     {{{
setMethod("iter", signature(obj="FLArray"),
	function(obj, iter) {
    if(dims(obj)$iter == 1)
      return(obj)
    else
      return(obj[,,,,,iter])
	}
)   # }}}

# summary          {{{
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

		# cat("units: ", object@units, "\n")
	}
)   # }}}

# trim {{{
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
  function(x, ...) {

    args <- list(...)
    nargs <- names(args)
    
    # dimension names
    qnames <- names(dimnames(x))
    
    # check input names match dimnames
    if(!all(nargs%in%qnames))
      stop(paste("Wrong dimension name provided: ", nargs[!nargs%in%qnames]))

    # Create list with given standard elements in right position ...
    select <- args[match(qnames, nargs)]
    
    # get rid of unspecified dimensions
    select <- select[!unlist(lapply(select, is.null))]
    
    # turn into characters
    select <- lapply(select, as.character)
    
    # match specified dimensions and dimnames
    dimnames <- dimnames(x)
    
    # check new dimnames contain old ones
    for(i in names(select))
      if(!all(dimnames[[i]] %in% select[[i]]))
        stop("New dimnames do not contain existing ones")
    
		dimnames[names(select)] <- select

    # output object
    res <- new(class(x), array(as.numeric(NA), dimnames=dimnames, dim= unlist(lapply(dimnames, length))), units=units(x))
    
    # list for assignment of x data
    dimnames <- dimnames(x)
    names(dimnames) <- c('i', 'j', 'k', 'l', 'm', 'n')

    do.call('[<-', c(list(x=res, value=x), dimnames))
  }
) # }}}

# uom {{{

uoms <- c(
	'1','10','100','1000','10000','100000','1000000','10000000','100000000', '1000000000',
	'10^0', '10^1', '10^2', '10^3', '10^4', '10^5', '10^6', '10^7', '10^8', '10^9',
	'1e0', '1e1', '1e2', '1e3', '1e4', '1e5', '1e6', '1e7', '1e8', '1e9',
	'kg', 't', 'm', 'f', 'z', 'hr', 'NA')
nums <- c(1:30)
nnums <- seq(max(nums) + 1, length(uoms))
snums <- c(1,2,3,4,25,26,27,28,29,30)

# NC: not computable; NA: unitless
uomTable <- array('NC', dimnames=list(op=c('*', '/', '+', '-'), e1=uoms, e2=uoms), dim=c(4, length(uoms), length(uoms)))

# N +- N = N
diag(uomTable['+',nums,nums]) <- rep(uoms[snums], 3)
diag(uomTable['-',nums,nums]) <- rep(uoms[snums], 3)
diag(uomTable['+',nnums,nnums]) <- uoms[nnums]
diag(uomTable['-',nnums,nnums]) <- uoms[nnums]

# 1 * N = N
uomTable['*', c('1', '1e0', '10^0'), nums] <- rep(rep(uoms[snums], 3), each=3)
uomTable['*', nums, c('1', '1e0', '10^0')] <- rep(uoms[snums], 9)

# N * N = NN TODO Turn into loop
# 10
uomTable['*', c(2, 12, 22), nums[-c(1, 11, 21, 10, 20, 30)]] <- 
	rep(uoms[snums][-c(1,2)], each=3)
uomTable['*', nums[-c(1, 11, 21, 10, 20, 30)], c(2, 12, 22)] <-
	rep(uoms[snums][-c(1,2)], 9)
# 100
uomTable['*', c(3, 13, 23), nums[-c(1, 11, 21, 2, 12, 22, 9, 19, 29, 10, 20, 30)]] <- 
	rep(uoms[snums][-c(1,2,3,4)], each=3)
uomTable['*', nums[-c(1, 11, 21, 2, 12, 22, 9, 19, 29, 10, 20, 30)], c(3, 13, 23)] <- 
	rep(uoms[snums][-c(1,2,3,4)], 9)
# 1000
uomTable['*', c(4, 14, 24), nums[-c(1, 11, 21, 2, 12, 22, 3, 13, 23, 8, 18, 28, 9, 19, 29, 10, 20, 30)]] <- 
	rep(uoms[snums][-c(1,2,3,4,5,6)], each=3)
uomTable['*', nums[-c(1, 11, 21, 2, 12, 22, 3, 13, 23, 8, 18, 28, 9, 19, 29, 10, 20, 30)], c(4, 14, 24)] <- 
	rep(uoms[snums][-c(1,2,3,4,5,6)], 9)
# 1e4
uomTable['*', c(5, 15, 25), nums[-c(1, 11, 21, 2, 12, 22, 3, 13, 23, 4, 14, 24, 7, 17, 27, 8, 18, 28, 9, 19, 29, 10, 20, 30)]] <- 
	rep(uoms[snums][-c(1,2,3,4,5,6,7,8)], each=3)
uomTable['*', nums[-c(1, 11, 21, 2, 12, 22, 3, 13, 23, 8, 18, 28, 9, 19, 29, 10, 20, 30)], c(4, 14, 24)] <- 
	rep(uoms[snums][-c(1,2,3,4,5,6)], 9)
# 1e5
uomTable['*', c(6, 16, 26), nums[-c(1, 11, 21, 2, 12, 22, 3, 13, 23, 4, 14, 24, 6, 16, 26, 7, 17, 27, 8, 18, 28, 9, 19, 29, 10, 20, 30)]] <-
	rep(uoms[snums][-c(1,2,3,4,5,6,7,8,9)], each=3)
uomTable['*', nums[-c(1, 11, 21, 2, 12, 22, 3, 13, 23, 4, 14, 24, 6, 16, 26, 7, 17, 27, 8, 18, 28, 9, 19, 29, 10, 20, 30)], c(6, 16, 26)] <-
	rep(uoms[snums][-c(1,2,3,4,5,6,7,8,9)], 9)

# U / U = NA
diag(uomTable['/',uoms,uoms]) <- 'NA'

# NA /*+- NA = NA
uomTable[,'NA', 'NA'] <- 'NA'

# kg * 1000 = t
uomTable['*', 'kg', c('1000', '1e3', '10^3')] <- 't'
uomTable['*', c('1000', '1e3', '10^3'), 'kg'] <- 't'

# kg * 100 = kg*100
uomTable['*', 'kg', c('100', '1e2', '10^2')] <- 'kg*100'
uomTable['*', c('100', '1e2', '10^2'), 'kg'] <- 'kg*100'

# kg * 10 = kg*10
uomTable['*', 'kg', c('10', '1e1', '10^1')] <- 'kg*10'
uomTable['*', c('10', '1e1', '10^1'), 'kg'] <- 'kg*10'

# kg * Numbers = t * Numbers/1000
uomTable['*', 'kg', c('10000', '1e4', '10^4')] <- 't*10'
uomTable['*', c('10000', '1e4', '10^4'), 'kg'] <- 't*10'
uomTable['*', 'kg', c('100000', '1e5', '10^5')] <- 't*100'
uomTable['*', c('100000', '1e5', '10^5'), 'kg'] <- 't*100'
uomTable['*', 'kg', c('1000000', '1e6', '10^6')] <- 't*1000'
uomTable['*', c('1000000', '1e6', '10^6'), 'kg'] <- 't*1000'
uomTable['*', 'kg', c('10000000', '1e7', '10^7')] <- 't*1e4'
uomTable['*', c('10000000', '1e7', '10^7'), 'kg'] <- 't*1e4'
uomTable['*', 'kg', c('100000000', '1e8', '10^8')] <- 't*1e5'
uomTable['*', 'kg', c('100000000', '1e8', '10^8')] <- 't*1e5'
uomTable['*', c('1000000000', '1e9', '10^9'), 'kg'] <- 't*1e6'
uomTable['*', c('1000000000', '1e9', '10^9'), 'kg'] <- 't*1e6'

# U */ NA = U
uomTable[c('*','/'), 'NA', nums] <- rep(rep(uoms[snums], each=2), 3)
uomTable[c('*','/'), nums, 'NA'] <- rep(rep(uoms[snums], each=2), 3)
uomTable[c('*','/'), nnums, 'NA'] <- rep(uoms[nnums], each=2)
uomTable[c('*','/'), 'NA', nnums] <- rep(uoms[nnums], each=2)

# z, m, f = 1/timestep
uomTable['+', 'f', 'm'] <- 'z'
uomTable['+', 'm', 'f'] <- 'z'
uomTable['-', 'z', 'f'] <- 'm'
uomTable['-', 'z', 'm'] <- 'f'

# m,f,z
uomTable['/', c('z','f','m'), c('z','f','m')] <- 'NA'
uomTable[c('*', '/'), nums, 'f'] <- rep(rep(uoms[snums], each=2), 3)
uomTable[c('*', '/'), nums, 'm'] <- rep(rep(uoms[snums], each=2), 3)
uomTable[c('*', '/'), nums, 'z'] <- rep(rep(uoms[snums], each=2), 3)
uomTable[c('*', '/'), nums, 'hr'] <- rep(rep(uoms[snums], each=2), 3)
uomTable[c('*', '/'), 'f', nums] <- rep(rep(uoms[snums], each=2), 3)
uomTable[c('*', '/'), 'm', nums] <- rep(rep(uoms[snums], each=2), 3)
uomTable[c('*', '/'), 'z', nums] <- rep(rep(uoms[snums], each=2), 3)
uomTable[c('*', '/'), 'hr', nums] <- rep(rep(uoms[snums], each=2), 3)

#' Method uom
#'
#' Units of Measurement
#' 
#' The \code{units} attribute of \code{FLQuant} objects provides a mechanism for
#' keeping track of the units of measurement of that particular piece of data.
#' This method in the form \code{uom()} carries out a conversion across units.
#' 
#' Arithmetic operators for \code{FLQuant} objects operate with a limited set of
#' units of measurement, and will output the right unit when two appropriate
#' objects are arithmetically combined. For example, the product of objects with
#' units of 'kg' and '1000' will output an object with units of 't' (for
#' metric tonnes).
#' 
#' Operations involving combinations of units not defined will result in the
#' \code{units} attribute simply storing a string indicating the input units of
#' measurement and the operation carried out, as in '10 * 1000'.
#' 
#' Note that no scaling or modification of the values in the object takes
#' place.
#'
#' @name uom
#' @docType methods
#' @param op The arithmetic operator to be used, one of '+', '-', '*' or '/'
#' @param u1 The units of measurement string of the first object
#' @param u2 The units of measurement string of the second object
#' @return a string with the corresponding units of measurement, a string such
#' as '10 *100' when not compatible
#'
#' @section Recognized Units:
#' 
#' The following units of measurement are recognized by the 'Arith' operators
#' (+, -, * /). \describe{ \item{Weight}{'kg', 't'} \item{Numbers}{1 -
#' 100000000, 1e0 - 1e8, 10^0 - 10^8} \item{Mortality}{'m', 'f', 'z', 'hr'}
#' \item{Other}{'NA'} }
#' @author The FLR Team
#' @seealso \code{\linkS4class{FLQuant}} \code{\link{units,FLArray-method}}
#' @keywords function
#' @examples
#' 
#' # Conversion between weights
#'   flq1 <- FLQuant(1, units='kg')
#'   flq2 <- FLQuant(5, units='1000')
#'   flq1 * flq2
#'
#' # Conversion between mortalities
#'   flq1 <- FLQuant(0.2, units='m')
#'   flq2 <- FLQuant(0.34, units='f')
#'   flq1 + flq2
#'

uom <- function(op, u1, u2) {

	u <- c(u1, u2)
	
	# max length of string, max(nchar(FLCore:::uoms))
	if(any(nchar(u) > 10))
		return(sprintf("%s %s %s", u1, op, u2))

	# ""
	if(!all(nzchar(u)))
		return(sprintf("%s %s %s", u1, op, u2))
	
	idx <- match(u, uoms)

	# undefined unit
	if(any(is.na(idx)))
		return(sprintf("%s %s %s", u1, op, u2))

	# use uomTable
	res <- uomTable[op, idx[1], idx[2]]
	
	# incompatible units ('NA')
#	 if(res == 'NC') {
#			warning('incompatible units of measurements in FLQuant objects: ',
#			sprintf("%s %s %s", u1, op, u2))
		
#		return(sprintf("%s %s %s", u1, op, u2))
#	}
	return(res)
}
# }}}

# Arith    {{{
#' Method Arith
#'
#' Arithmetic methods for FLR objects
#'
#' The \code{Arith} group of methods, comprising addition, substraction,
#' product, division, exponentiation, modulus and integer division ().
#' These methods work exactly as in an object of class \code{array}, but always
#' return an \code{FLQuant} object, thus no dimension is dropped.
#'
#' Operations between an \code{FLQuant} and an \code{FLPar} object will always
#' return an FLQuant, with the \code{FLPar} dimensions being place on top of
#' the correspoding ones at the \code{FLQuant} object. See examples below.
#'
#' @name Arith
#' @rdname Arith
#' @aliases Arith,numeric,FLArray-method
#' @docType methods
#' @section Generic function: Arith(e1,e2)
#' @param e1,e2 Objects
#' @author The FLR Team
#' @seealso \code{\link[base]{Arithmetic}}, \code{\link[methods]{Arith}}
#' @keywords methods
#' @examples
#' 
#' flq <- FLQuant(c(9,5), dim=c(2,5))
#' fl2 <- FLQuant(2, dim=c(2,5))
#' flq * fl2
#' flq / fl2
#'
#' # FLQuant against numeric vector
#' flq * 2.5
#' # Recycling rule applies
#' flq * c(1, 2)
#'
setMethod("Arith", ##  "+", "-", "*", "^", "%%", "%/%", "/"
	signature(e1 = "numeric", e2 = "FLArray"),
	function(e1, e2)
	{
		return(new(class(e2), callGeneric(e1, e2@.Data), units=units(e2)))
	}
)
#' @rdname Arith
#' @aliases Arith,FLArray,numeric-method
setMethod("Arith",
	signature(e1 = "FLArray", e2 = "numeric"),
	function(e1, e2)
	{
		return(new(class(e1), callGeneric(e1@.Data, e2), units=units(e1)))
	}
)
#' @rdname Arith
#' @aliases Arith,FLArray,FLArray-method
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
		if(identical(units(e1), units(e2))) {
			units <- units(e1)
		} else {
			op <- as.character(get('.Generic'))
			units <- uom(op, units(e1), units(e2))
		}
    return(new(class(e1), e, units=units))
	}
)   # }}}

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
    ## calculates sigma squared for use in concentrated likelihood
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
#' Method apply
#' 
#' Functions can be applied to margins of an \code{FLQuant} or \code{FLPar} array
#' using this method. In contrast with the standard R method, dimensions are not collapsed
#' in the output object.
#' 
#' \code{FUN} in the case of an \code{FLQuant} must be a function whose results is least one
#' dimension less when applied over an array (e.g. sum, mean, ...).
#' 
#' For further details see \link[base]{apply}.
#'
#' @name apply
#' @aliases apply,ANY,missing,missing-method apply,FLQuant,numeric,function-method
#' apply,FLArray,numeric,function-method
#' @docType methods
#' @section Generic function: apply(X,MARGIN,FUN)
#' @author The FLR Team
#' @seealso \link[base]{apply}
#' @keywords methods
#' @examples
#' 
#' flq <- FLQuant(rlnorm(1000), dim=c(10,20,1,1,1,5))
#' apply(flq, 1, sum)
#' apply(flq, 2:6, sum)
#' 

setMethod("apply", signature(X="FLArray", MARGIN="numeric", FUN="function"),
	function(X, MARGIN, FUN, ...)
  {
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
		return(data)
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

#' Method window
#'
#' Extract time (year) windows of an FLR object
#' 
#' This method extracts a section of, or extends an \code{FLQuant} or other FLR
#' objects along the \code{year} dimension. If a frequency is specified, the
#' new object contains data at only those year steps.
#' 
#' Although objects of class \code{\link{FLQuant}} do have another temporal
#' dimension, \code{season},  window currently only works along the \code{year}
#' dimension. To subset/extend along other dimensions, refer to
#' \link{Extract-FLCore}, \code{\link{trim}}, or \code{\link{expand}}.
#'
#' @name window
#' @aliases window,FLQuant-method
#' @docType methods
#' @section Generic function: window(x)
#' @author The FLR Team
#' @seealso \link[stats]{window}, \link{Extract-FLCore}
#' @keywords methods
#' @examples
#' 
#' flq <- FLQuant(rnorm(50), dimnames=list(age=1:5, year=1991:2000))
#' window(flq, start=1995, end=1998)
#' window(flq, start=1990, end=2010, frequency=2)
#' 
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

# log & exp {{{
setMethod('log', signature(x='FLArray'),
	function(x, ...) {
		x@.Data <- log(x@.Data, ...)
		units(x) <- 'NA'
		return(x)
	}
)

setMethod('exp', signature(x='FLArray'),
	function(x) {
		x@.Data <- exp(x@.Data)
		units(x) <- 'NA'
		return(x)
	}
) # }}}

# median        {{{
setMethod("median", signature(x="FLArray"),
	function(x, na.rm=TRUE){
		return(median(c(x), na.rm=na.rm))
	}
)   # }}}
