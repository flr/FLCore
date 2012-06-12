# operators.R - DESC
# operators.R

# Copyright 2003-2012 FLR Team. Distributed under the GPL 2 or later
# Maintainer: Iago Mosqueira, JRC
# $Id: $


# FLQuant, FLQuant {{{
# %*% {{{
# Multiply two FLQuant objects by matching dimensions, expands 1 to n
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
      stop("dims to be expanded in cannot be of length > 1")

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

    return(FLQuant(rx * ry, dimnames=dni, units=paste(units(x), units(y), sep="*")))
  }
) # }}}

# %/% {{{
# Divide two FLQuant objects by matching dimensions, expands 1 to n
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

    return(FLQuant(re1 / re2, dimnames=dni, units=paste(units(e1), units(e2), sep="/")))
  }
) # }}}

# %+% {{{
# Add two FLQuant objects by matching dimensions, expands 1 to n
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
      stop("dims to be expanded in cannot be of length > 1")

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

    return(FLQuant(rx + ry, dimnames=dni, units=paste(units(x), units(y), sep="+")))
  }
) # }}}

# %-% {{{
# Sustract two FLQuant objects by matching dimensions, expands 1 to n
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
      stop("dims to be expanded in cannot be of length > 1")

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

    return(FLQuant(rx - ry, dimnames=dni, units=paste(units(x), units(y), sep="-")))
  }
) # }}}

# }}}

# FLPar, FLQuant {{{
# %*% {{{
# Multiply FLPar against FLQuant by matching dimnames, expands 1 to n
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
    return(FLQuant(rx) %*% y)
  }
) # }}}

# %/% {{{
# Divide FLPar against FLQuant by matching dimnames, expands 1 to n
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
    return(FLQuant(re1) %/% e2)
  }
) # }}}

# %+% {{{
# Add FLPar and FLQuant by matching dimnames, expands 1 to n
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
    return(FLQuant(rx) %+% y)
  }
) # }}}

# %-% {{{
# Substract FLPar and FLQuant by matching dimnames, expands 1 to n
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
    return(FLQuant(rx) %-% y)
  }
) # }}}
# }}}

# FLQuant, FLPar {{{
# %*% {{{
# Multiply FLQuant against FLPar by matching dimnames, expands 1 to n
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
# Divide FLPar against FLQuant by matching dimnames, expands 1 to n
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
    return(e1 %/% FLQuant(re2))
  }
) # }}}

# %+% {{{
# Add FLQuant and FLPar by matching dimnames, expands 1 to n
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
# Substract FLQuant and FLPar by matching dimnames, expands 1 to n
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

# }}}

# FLPar, FLPar {{{
# Multiply FLPar against FLPar by matching dimnames, expands 1 to n, creates missing
setMethod("%*%", signature(x="FLPar", y="FLPar"),
	function(x, y) {

    # dims & dimnames
    dnx <- dimnames(x)
    dny <- dimnames(y)

    # vector of final dim
    dnsx <- unlist(lapply(dnx, length))
    dnsy <- unlist(lapply(dny, length))
    dnd <- rbind(dnsx, dnsy)
    

    dr <- pmax(dnsx, dnsy)

    # select dimnames from larger FLPar
    if(length(dnx) > length(dny)) {
      dnr <- names(dnx)
      dnmr <- dnx
    }
    else {
      dnr <- names(dny)
      dnmr <- dny
    }

    # TEST: non-matching dnames in x or y should be of length 1
    if(any(apply(dnd, 2, function(x) all(x > 0) && max(x)/min(x) != max(x))))
      stop("dimensions in 'x' not matching in length those in 'y' must be of length=1")
    
    # TODO expand & aperm FLPars
    FLPar(
          array(x@.Data, dim=dr, dimnames=dnmr)
          * 
          array(y@.Data, dim=dr, dimnames=dnmr)
          )
  }
) # }}}

# matchDimnames {{{
matchDimnames <- function(dnp, dnq) {

  # too tricky to explain ...
  idx <- match(names(dnq)[sort(match(names(dnp), names(dnq)))], names(dnp))
  sx <- seq(names(dnp))
  sx[sx %in% idx] <- idx

  return(sx)
} # }}}
