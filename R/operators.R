# operators.R - DESC
# operators.R

# Copyright 2003-2015 FLR Team. Distributed under the GPL 2 or later
# Maintainer: Iago Mosqueira, EC JRC G03

#' FLQuant arithmetic operators that extend objects
#'
#' Arithmetic operations between two \linkS4class{FLQuant} objects using the
#' standars operators (`+`, `-`, `*`, `/`, `^`, see \link{Arith}) need all dimensions in
#' both objects to match. This requirement is relaxed by using the percent
#' version of those four: `%+%`, `%-%`, `%*%`, `%/%` and `%^%`.
#'
#' If any of the objects is of length one in a dimensions where the other is
#' longer, the dimensions will be extended and the element-by-element operation
#' then conducted. Dimensions and dimnames of the output will be those of the
#' larger object. See the examples to observe their behaviour.
#'
#' Please note that this behjaviour is present on the \link{Arith} methods for
#' \linkS4class{FLArray}-derived classes but only on the 6th, `iter`, dimension.
#'
#' The original use of the `%*%` operator, as vector product, is not available
#' for \linkS4class{FLQuant} objects, but can be applied to the \link{array}
#' inside them, as in the example below.
#'
#' Methods for operations between an \linkS4class{FLQuant} and an
#' \linkS4class{FLPar} object will match dimensions by names of dimnames,
#' regardless of position.
#'
#' @name operators
#' @aliases operators
#' @docType methods
#' @section Generic function: x %+% y, x %-% y, x %*% y, e1 %/% e2, x %^% y
#' @author The FLR Team
#' @seealso \code{\linkS4class{FLQuant}}, \code{\link[base]{matmult}}
#' @keywords methods
#' @examples
#' 
#' a <- FLQuant(2, dim=c(3,3,2))
#' b <- FLQuant(3, dim=c(3,3,1))
#'
#' # This should fail
#' \dontrun{ a * b }
#'
#' a %*% b
#' a %+% b
#' # To use base's %*% vector product, apply it to a matrix from @.Data
#' b@.Data[,,,,,] %*% 1:3
#' # or
#' b[,,drop=TRUE] %*% 1:3
#'
#' # FLPar vs. FLQuant works by dimnames' names
#' flp <- FLPar(2, dimnames=list(params='a', year=2000:2005, iter=1))
#' flq <- FLQuant(3, dimnames=list(year=2000:2005))
#' flp %*% flq
NULL

# FLQuant, FLQuant {{{
# %*% {{{

#' @rdname operators
#' @aliases %*%,FLQuant,FLQuant-method
setMethod("%*%", signature(x="FLQuant", y="FLQuant"),
	function(x, y) {

    # get dims
    dx <- dim(x)
    dy <- dim(y)

    # final dims
    di <- pmax(dx, dy)
    dli <- lapply(as.list(di), function(x) rep(1, x))

    # TEST: No expansion n -> m allowed, must be originally 1
    if(any(di != dx &  dx != 1) | any(di != dy &  dy != 1))
      stop("dims to be expanded cannot be of length > 1")

    # new x
    dlx <- lapply(as.list(dx), seq)
    dlx[di > dx] <- dli[di > dx]

    rx <- do.call('[', c(list(x=x@.Data, drop=FALSE), dlx))

    # new y
    dly <- lapply(as.list(dy), seq)
    dly[di > dy] <- dli[di > dy]

    ry <- do.call('[', c(list(x=y@.Data, drop=FALSE), dly))

    # dimnames
    dni <- dimnames(x)
    dni[di > dx] <- dimnames(y)[di > dx]

		# units
		if(identical(units(x), units(y))) {
			units <- units(x)
		} else {
			units <- uom('*', units(x), units(y))
		}

    return(FLQuant(rx * ry, dimnames=dni, units=units))
  }
) # }}}

# %/% {{{

#' @rdname operators
#' @aliases %/%,FLQuant,FLQuant-method
setMethod("%/%", signature(e1="FLQuant", e2="FLQuant"),
	function(e1, e2) {

    # get dims
    de1 <- dim(e1)
    de2 <- dim(e2)

    # final dims
    di <- pmax(de1, de2)
    dli <- lapply(as.list(di), function(x) rep(1, x))

    # TEST: No expansion n -> m allowed, must be originally 1
    if(any(di != de1 &  de1 != 1) | any(di != de2 &  de2 != 1))
      stop("dims to be expanded cannot be of length > 1")

    # new x
    dle1 <- lapply(as.list(de1), seq)
    dle1[di > de1] <- dli[di > de1]

    re1 <- do.call('[', c(list(x=e1@.Data, drop=FALSE), dle1))

    # new y
    dle2 <- lapply(as.list(de2), seq)
    dle2[di > de2] <- dli[di > de2]

    re2 <- do.call('[', c(list(x=e2@.Data, drop=FALSE), dle2))

    # dimnames
    dni <- dimnames(e1)
    dni[di > de1] <- dimnames(e2)[di > de1]

		# units
		units <- uom('/', units(e1), units(e2))
    
		return(FLQuant(re1 / re2, dimnames=dni, units=units))
  }
) # }}}

# %+% {{{

#' @rdname operators
#' @aliases %+%,FLQuant,FLQuant-method
setMethod("%+%", signature(x="FLQuant", y="FLQuant"),
	function(x, y) {

    # get dims
    dx <- dim(x)
    dy <- dim(y)

    # final dims
    di <- pmax(dx, dy)
    dli <- lapply(as.list(di), function(x) rep(1, x))

    # TEST: No expansion n -> m allowed, must be originally 1
    if(any(di != dx &  dx != 1) | any(di != dy &  dy != 1))
      stop("dims to be expanded cannot be of length > 1")

    # new x
    dlx <- lapply(as.list(dx), seq)
    dlx[di > dx] <- dli[di > dx]

    rx <- do.call('[', c(list(x=x@.Data, drop=FALSE), dlx))

    # new y
    dly <- lapply(as.list(dy), seq)
    dly[di > dy] <- dli[di > dy]

    ry <- do.call('[', c(list(x=y@.Data, drop=FALSE), dly))

    # dimnames
    dni <- dimnames(x)
    dni[di > dx] <- dimnames(y)[di > dx]
	
		# units
		if(identical(units(x), units(y))) {
			units <- units(x)
		} else {
			units <- uom('+', units(x), units(y))
		}

    return(FLQuant(rx + ry, dimnames=dni, units=units))
  }
) # }}}

# %-% {{{

#' @rdname operators
#' @aliases %-%,FLQuant,FLQuant-method
setMethod("%-%", signature(x="FLQuant", y="FLQuant"),
	function(x, y) {

    # get dims
    dx <- dim(x)
    dy <- dim(y)

    # final dims
    di <- pmax(dx, dy)
    dli <- lapply(as.list(di), function(x) rep(1, x))

    # TEST: No expansion n -> m allowed, must be originally 1
    if(any(di != dx &  dx != 1) | any(di != dy &  dy != 1))
      stop("dims to be expanded cannot be of length > 1")

    # new x
    dlx <- lapply(as.list(dx), seq)
    dlx[di > dx] <- dli[di > dx]

    rx <- do.call('[', c(list(x=x@.Data, drop=FALSE), dlx))

    # new y
    dly <- lapply(as.list(dy), seq)
    dly[di > dy] <- dli[di > dy]

    ry <- do.call('[', c(list(x=y@.Data, drop=FALSE), dly))

    # dimnames
    dni <- dimnames(x)
    dni[di > dx] <- dimnames(y)[di > dx]

		# units
		if(identical(units(x), units(y))) {
			units <- units(x)
		} else {
			units <- uom('-', units(x), units(y))
		}

    return(FLQuant(rx - ry, dimnames=dni, units=units))
  }
) # }}}

# %^% {{{

#' @rdname operators
#' @aliases %^%,FLQuant,FLQuant-method
setMethod("%^%", signature(x="FLQuant", y="FLQuant"),
	function(x, y) {

    # get dims
    dx <- dim(x)
    dy <- dim(y)

    # final dims
    di <- pmax(dx, dy)
    dli <- lapply(as.list(di), function(x) rep(1, x))

    # TEST: No expansion n -> m allowed, must be originally 1
    if(any(di != dx &  dx != 1) | any(di != dy &  dy != 1))
      stop("dims to be expanded cannot be of length > 1")

    # new x
    dlx <- lapply(as.list(dx), seq)
    dlx[di > dx] <- dli[di > dx]

    rx <- do.call('[', c(list(x=x@.Data, drop=FALSE), dlx))

    # new y
    dly <- lapply(as.list(dy), seq)
    dly[di > dy] <- dli[di > dy]

    ry <- do.call('[', c(list(x=y@.Data, drop=FALSE), dly))

    # dimnames
    dni <- dimnames(x)
    dni[di > dx] <- dimnames(y)[di > dx]

		# units
		if(identical(units(x), units(y))) {
			units <- units(x)
		} else {
			units <- uom('^', units(x), units(y))
		}

    return(FLQuant(rx ^ ry, dimnames=dni, units=units))
  }
) # }}}

# }}}

# FLPar, FLQuant {{{
# %*% {{{

#' @rdname operators
#' @aliases %*%,FLPar,FLQuant-method
setMethod("%*%", signature(x="FLPar", y="FLQuant"),
	function(x, y) {

    # dims & dimnames
    dx <- dim(x)
    dnx <- dimnames(x)
    dy <- dim(y)
    dny <- dimnames(y)

    # TEST: non-matching dims in x should be of length 1
    idy <- !names(dnx) %in% names(dny)
    if(any(dx[idy] > 1))
      stop("dimensions in 'x' not matching those in 'y' must be of length=1")

    # aperm if FLPar dimnames sorted differently to FLQuant's
    idx <- matchDimnames(dnx, dny)
    if(any(idx != sort(idx))) {
      x <- aperm(x, idx)
      dx <- dx[idx]
      dnx <- dnx[idx]
    }

    # tmp FLQuant dims
    di <- rep(1, 6)
    di[names(dny) %in% names(dnx)] <- dx[names(dnx) %in% names(dny)]

    # x data in 6D array
    rx <- array(x@.Data, dim=di)

    # expansion done in %*%(FLQuant, FLQuant)
    return(FLQuant(rx, quant=quant(y)) %*% y)
  }
) # }}}

# %/% {{{

#' @rdname operators
#' @aliases %/%,FLPar,FLQuant-method

setMethod("%/%", signature(e1="FLPar", e2="FLQuant"),
	function(e1, e2) {

    # dims & dimnames
    de1 <- dim(e1)
    dne1 <- dimnames(e1)
    de2 <- dim(e2)
    dne2 <- dimnames(e2)

    # TEST: non-matching dims in e1 should be of length 1
    ide2 <- !names(dne1) %in% names(dne2)
    if(any(de1[ide2] > 1))
      stop("dimensions in 'e1' not matching those in 'e2' must be of length=1")

    # aperm if FLPar dimnames sorted differently to FLQuant's
    ide1 <- matchDimnames(dne1, dne2)
    if(any(ide1 != sort(ide1))) {
      e1 <- aperm(e1, ide1)
      de1 <- de1[ide1]
      dne1 <- dne1[ide1]
    }

    # tmp FLQuant dims
    di <- rep(1, 6)
    di[names(dne2) %in% names(dne1)] <- de1[names(dne1) %in% names(dne2)]

    # e1 data in 6D array
    re1 <- array(e1@.Data, dim=di)

    # expansion done in %/%(FLQuant, FLQuant)
    return(FLQuant(re1, quant=quant(e2)) %/% e2)
  }
) # }}}

# %+% {{{

#' @rdname operators
#' @aliases %+%,FLPar,FLQuant-method
setMethod("%+%", signature(x="FLPar", y="FLQuant"),
	function(x, y) {

    # dims & dimnames
    dx <- dim(x)
    dnx <- dimnames(x)
    dy <- dim(y)
    dny <- dimnames(y)

        # TEST: non-matching dims in x should be of length 1
    idy <- !names(dnx) %in% names(dny)
    if(any(dx[idy] > 1))
      stop("dimensions in 'x' not matching those in 'y' must be of length=1")

    # aperm if FLPar dimnames sorted differently to FLQuant's
    idx <- matchDimnames(dnx, dny)
    if(any(idx != sort(idx))) {
      x <- aperm(x, idx)
      dx <- dx[idx]
      dnx <- dnx[idx]
    }

    # tmp FLQuant dims
    di <- rep(1, 6)
    di[names(dny) %in% names(dnx)] <- dx[names(dnx) %in% names(dny)]

    # x data in 6D array
    rx <- array(x@.Data, dim=di)

    # expansion done in %+%(FLQuant, FLQuant)
    return(FLQuant(rx, quant=quant(y)) %+% y)
  }
) # }}}

# %-% {{{

#' @rdname operators
#' @aliases %-%,FLPar,FLQuant-method
setMethod("%-%", signature(x="FLPar", y="FLQuant"),
	function(x, y) {

    # dims & dimnames
    dx <- dim(x)
    dnx <- dimnames(x)
    dy <- dim(y)
    dny <- dimnames(y)

        # TEST: non-matching dims in x should be of length 1
    idy <- !names(dnx) %in% names(dny)
    if(any(dx[idy] > 1))
      stop("dimensions in 'x' not matching those in 'y' must be of length=1")

    # aperm if FLPar dimnames sorted differently to FLQuant's
    idx <- matchDimnames(dnx, dny)
    if(any(idx != sort(idx))) {
      x <- aperm(x, idx)
      dx <- dx[idx]
      dnx <- dnx[idx]
    }

    # tmp FLQuant dims
    di <- rep(1, 6)
    di[names(dny) %in% names(dnx)] <- dx[names(dnx) %in% names(dny)]

    # x data in 6D array
    rx <- array(x@.Data, dim=di)

    # expansion done in %-%(FLQuant, FLQuant)
    return(FLQuant(rx, quant=quant(y)) %-% y)
  }
) # }}}

# %^% {{{

#' @rdname operators
#' @aliases %^%,FLPar,FLQuant-method
setMethod("%^%", signature(x="FLPar", y="FLQuant"),
	function(x, y) {

    # dims & dimnames
    dx <- dim(x)
    dnx <- dimnames(x)
    dy <- dim(y)
    dny <- dimnames(y)

    # TEST: non-matching dims in x should be of length 1
    idy <- !names(dnx) %in% names(dny)
    if(any(dx[idy] > 1))
      stop("dimensions in 'x' not matching those in 'y' must be of length=1")

    # aperm if FLPar dimnames sorted differently to FLQuant's
    idx <- matchDimnames(dnx, dny)
    if(any(idx != sort(idx))) {
      x <- aperm(x, idx)
      dx <- dx[idx]
      dnx <- dnx[idx]
    }

    # tmp FLQuant dims
    di <- rep(1, 6)
    di[names(dny) %in% names(dnx)] <- dx[names(dnx) %in% names(dny)]

    # x data in 6D array
    rx <- array(x@.Data, dim=di)

    # expansion done in %^%(FLQuant, FLQuant)
    return(FLQuant(rx, quant=quant(y)) %^% y)
  }
) # }}}
# }}}

# FLQuant, FLPar {{{
# %*% {{{

#' @rdname operators
#' @aliases %*%,FLQuant,FLPar-method
setMethod("%*%", signature(x="FLQuant", y="FLPar"),
	function(x, y) {

    # dims & dimnames
    dx <- dim(x)
    dnx <- dimnames(x)
    dy <- dim(y)
    dny <- dimnames(y)

    # TEST: non-matching dims in y should be of length 1
    idx <- !names(dny) %in% names(dnx)
    if(any(dy[idx] > 1))
      stop("dimensions in 'y' not matching those in 'x' must be of length=1")

    # aperm if FLPar dimnames sorted differently to FLQuant's
    idy <- matchDimnames(dny, dnx)
    if(any(idy != sort(idy))) {
      y <- aperm(y, idy)
      dy <- dy[idy]
      dny <- dny[idy]
    }

    # tmp FLQuant dims
    di <- rep(1, 6)
    di[names(dnx) %in% names(dny)] <- dy[names(dny) %in% names(dnx)]

    # y data in 6D array
    ry <- array(y@.Data, dim=di)

    # expansion done in %*%(FLQuant, FLQuant)
    return(x %*% FLQuant(ry))
  }
) # }}}

# %/% {{{

#' @rdname operators
#' @aliases %/%,FLQuant,FLPar-method
setMethod("%/%", signature(e1="FLQuant", e2="FLPar"),
	function(e1, e2) {

    # dims & dimnames
    de1 <- dim(e1)
    dne1 <- dimnames(e1)
    de2 <- dim(e2)
    dne2 <- dimnames(e2)

    # TEST: non-matching dims in e1 should be of length 1
    ide1 <- !names(dne2) %in% names(dne1)
    if(any(de2[ide1] > 1))
      stop("dimensions in 'e2' not matching those in 'e1' must be of length=1")

    # aperm if FLPar dimnames sorted differently to FLQuant's
    ide2 <- matchDimnames(dne2, dne1)
    if(any(ide2 != sort(ide2))) {
      e2 <- aperm(e2, ide2)
      de2 <- de2[ide2]
      dne2 <- dne2[ide2]
    }

    # tmp FLQuant dims
    di <- rep(1, 6)
    di[names(dne1) %in% names(dne2)] <- de2[names(dne2) %in% names(dne1)]

    # e2 data in 6D array
    re2 <- array(e2@.Data, dim=di)

    # expansion done in %/%(FLQuant, FLQuant)
    res <- e1 %/% FLQuant(re2)
		
    # units
    units(res) <- uom('/', units(e1), units(e2))

    return(res)
  }
) # }}}

# %+% {{{

#' @rdname operators
#' @aliases %+%,FLQuant,FLPar-method
setMethod("%+%", signature(x="FLQuant", y="FLPar"),
	function(x, y) {

    # dims & dimnames
    dx <- dim(x)
    dnx <- dimnames(x)
    dy <- dim(y)
    dny <- dimnames(y)

    # TEST: non-matching dims in y should be of length 1
    idx <- !names(dny) %in% names(dnx)
    if(any(dy[idx] > 1))
      stop("dimensions in 'y' not matching those in 'x' must be of length=1")

    # aperm if FLPar dimnames sorted differently to FLQuant's
    idy <- matchDimnames(dny, dnx)
    if(any(idy != sort(idy))) {
      y <- aperm(y, idy)
      dy <- dy[idy]
      dny <- dny[idy]
    }

    # tmp FLQuant dims
    di <- rep(1, 6)
    di[names(dnx) %in% names(dny)] <- dy[names(dny) %in% names(dnx)]

    # y data in 6D array
    ry <- array(y@.Data, dim=di)

    # expansion done in %+%(FLQuant, FLQuant)
    return(x %+% FLQuant(ry))
  }
) # }}}

# %-% {{{

#' @rdname operators
#' @aliases %-%,FLQuant,FLPar-method
setMethod("%-%", signature(x="FLQuant", y="FLPar"),
	function(x, y) {

    # dims & dimnames
    dx <- dim(x)
    dnx <- dimnames(x)
    dy <- dim(y)
    dny <- dimnames(y)

    # TEST: non-matching dims in y should be of length 1
    idx <- !names(dny) %in% names(dnx)
    if(any(dy[idx] > 1))
      stop("dimensions in 'y' not matching those in 'x' must be of length=1")

    # aperm if FLPar dimnames sorted differently to FLQuant's
    idy <- matchDimnames(dny, dnx)
    if(any(idy != sort(idy))) {
      y <- aperm(y, idy)
      dy <- dy[idy]
      dny <- dny[idy]
    }

    # tmp FLQuant dims
    di <- rep(1, 6)
    di[names(dnx) %in% names(dny)] <- dy[names(dny) %in% names(dnx)]

    # y data in 6D array
    ry <- array(y@.Data, dim=di)

    # expansion done in %-%(FLQuant, FLQuant)
    return(x %-% FLQuant(ry))
  }
) # }}}

# %^% {{{

#' @rdname operators
#' @aliases %^%,FLQuant,FLPar-method
setMethod("%^%", signature(x="FLQuant", y="FLPar"),
	function(x, y) {

    # dims & dimnames
    dx <- dim(x)
    dnx <- dimnames(x)
    dy <- dim(y)
    dny <- dimnames(y)

    # TEST: non-matching dims in y should be of length 1
    idx <- !names(dny) %in% names(dnx)
    if(any(dy[idx] > 1))
      stop("dimensions in 'y' not matching those in 'x' must be of length=1")

    # aperm if FLPar dimnames sorted differently to FLQuant's
    idy <- matchDimnames(dny, dnx)
    if(any(idy != sort(idy))) {
      y <- aperm(y, idy)
      dy <- dy[idy]
      dny <- dny[idy]
    }

    # tmp FLQuant dims
    di <- rep(1, 6)
    di[names(dnx) %in% names(dny)] <- dy[names(dny) %in% names(dnx)]

    # y data in 6D array
    ry <- array(y@.Data, dim=di)

    # expansion done in %-%(FLQuant, FLQuant)
    return(x %^% FLQuant(ry))
  }
) # }}}

# }}}

# FLPar, FLPar {{{
# %*% {{{

#' @rdname operators
#' @aliases %*%,FLPar,FLPar-method
setMethod("%*%", signature(x="FLPar", y="FLPar"),
	function(x, y) {

    # dimnames
    dnx <- dimnames(x)
    dny <- dimnames(y)
		
		ldx <- unlist(lapply(dnx, length))
		ldy <- unlist(lapply(dny, length))

		# apply operation directly if dimnames match
		if(identical(ldx, ldy))
			return(x * y)
    
		# vector of final dim
    dnd <- rbind(ldx, ldy)
    
    # TEST: non-matching dnames in x or y should be of length 1
    if(any(apply(dnd, 2, function(x) all(x > 0) && max(x)/min(x) != max(x))))
      stop("dimensions in 'x' not matching in length those in 'y' must be of length=1")

		# new dim
    dr <- pmax(ldx, ldy)

		# new dimnames
		dni <- apply(dnd, 2, which.max)
		dnx[dni == 2] <- dny[dni == 2]

    # TODO expand & aperm FLPars
    FLPar(array(x@.Data, dim=dr, dimnames=dnx) * array(y@.Data, dim=dr, dimnames=dnx))
  }
) # }}}

# %+% {{{

#' @rdname operators
#' @aliases %+%,FLPar,FLPar-method
setMethod("%+%", signature(x="FLPar", y="FLPar"),
	function(x, y) {

    # dimnames
    dnx <- dimnames(x)
    dny <- dimnames(y)
		
		ldx <- unlist(lapply(dnx, length))
		ldy <- unlist(lapply(dny, length))

		# apply operation directly if dimnames match
		if(identical(ldx, ldy))
			return(x + y)
    
		# vector of final dim
    dnd <- rbind(ldx, ldy)
    
    # TEST: non-matching dnames in x or y should be of length 1
    if(any(apply(dnd, 2, function(x) all(x > 0) && max(x)/min(x) != max(x))))
      stop("dimensions in 'x' not matching in length those in 'y' must be of length=1")

		# new dim
    dr <- pmax(ldx, ldy)

		# new dimnames
		dni <- apply(dnd, 2, which.max)
		dnx[dni == 2] <- dny[dni == 2]

    # TODO expand & aperm FLPars
    FLPar(array(x@.Data, dim=dr, dimnames=dnx) + array(y@.Data, dim=dr, dimnames=dnx))
  }
) # }}}

# %-% {{{

#' @rdname operators
#' @aliases %-%,FLPar,FLPar-method
setMethod("%-%", signature(x="FLPar", y="FLPar"),
	function(x, y) {

    # dimnames
    dnx <- dimnames(x)
    dny <- dimnames(y)
		
		ldx <- unlist(lapply(dnx, length))
		ldy <- unlist(lapply(dny, length))

		# apply operation directly if dimnames match
		if(identical(ldx, ldy))
			return(x - y)
    
		# vector of final dim
    dnd <- rbind(ldx, ldy)
    
    # TEST: non-matching dnames in x or y should be of length 1
    if(any(apply(dnd, 2, function(x) all(x > 0) && max(x)/min(x) != max(x))))
      stop("dimensions in 'x' not matching in length those in 'y' must be of length=1")

		# new dim
    dr <- pmax(ldx, ldy)

		# new dimnames
		dni <- apply(dnd, 2, which.max)
		dnx[dni == 2] <- dny[dni == 2]

    # TODO expand & aperm FLPars
    FLPar(array(x@.Data, dim=dr, dimnames=dnx) - array(y@.Data, dim=dr, dimnames=dnx))
  }
) # }}}

# %/% {{{

#' @rdname operators
#' @aliases %/%,FLPar,FLPar-method
setMethod("%/%", signature(e1="FLPar", e2="FLPar"),
	function(e1, e2) {

    # dimnames
    dn1 <- dimnames(e1)
    dn2 <- dimnames(e2)
		
		ld1 <- unlist(lapply(dn1, length))
		ld2 <- unlist(lapply(dn2, length))

		# apply operation directly if dimnames match
		if(identical(ld1, ld2))
			return(e1 / e2)
    
		# vector of final dim
    dnd <- rbind(ld1, ld2)
    
    # TEST: non-matching dnames in x or y should be of length 1
    if(any(apply(dnd, 2, function(x) all(x > 0) && max(x)/min(x) != max(x))))
      stop("dimensions in 'x' not matching in length those in 'y' must be of length=1")

		# new dim
    dr <- pmax(ld1, ld2)

		# new dimnames
		dni <- apply(dnd, 2, which.max)
		dn1[dni == 2] <- dn2[dni == 2]
    
		# TODO expand & aperm FLPars
    FLPar(array(e1@.Data, dim=dr, dimnames=dn1) / array(e2@.Data, dim=dr, dimnames=dn1))
  }
) # }}}

# %^% {{{

#' @rdname operators
#' @aliases %^%,FLPar,FLPar-method
setMethod("%^%", signature(x="FLPar", y="FLPar"),
	function(x, y) {

    # dimnames
    dnx <- dimnames(x)
    dny <- dimnames(y)
		
		ldx <- unlist(lapply(dnx, length))
		ldy <- unlist(lapply(dny, length))

		# apply operation directly if dimnames match
		if(identical(ldx, ldy))
			return(x ^ y)
    
		# vector of final dim
    dnd <- rbind(ldx, ldy)
    
    # TEST: non-matching dnames in x or y should be of length 1
    if(any(apply(dnd, 2, function(x) all(x > 0) && max(x)/min(x) != max(x))))
      stop("dimensions in 'x' not matching in length those in 'y' must be of length=1")

		# new dim
    dr <- pmax(ldx, ldy)

		# new dimnames
		dni <- apply(dnd, 2, which.max)
		dnx[dni == 2] <- dny[dni == 2]

    # TODO expand & aperm FLPars
    FLPar(array(x@.Data, dim=dr, dimnames=dnx) ^ array(y@.Data, dim=dr, dimnames=dnx))
  }
) # }}}
# }}}

# matchDimnames {{{
matchDimnames <- function(dnp, dnq) {

  # too tricky to explain ...
  idx <- match(names(dnq)[sort(match(names(dnp), names(dnq)))], names(dnp))
  sx <- seq(names(dnp))
  sx[sx %in% idx] <- idx

  return(sx)
} # }}}

# FLQuants, FLPar

# %/% {{{

#' @rdname operators
#' @aliases /,FLQuants,FLPar-method
setMethod("/", signature(e1="FLQuants", e2="FLPar"),
	function(e1, e2) {

    res <- lapply(names(e1), function(x) e1[[x]] / e2[x,])
    names(res) <- names(e1)
    return(FLQuants(res))
  }
) # }}}
