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

    # TEST: non-matching dnames in x should be of length 1
    idy <- !dnx %in% dny
    if(any(dx[idy] > 1))
      stop("dimensions in 'x' not matching those in 'y' must be of length=1")

    # result dims
    di <- rep(1, 6)
    di[dny %in% dnx] <- dx

    # x data in 6D array
    rx <- array(x@.Data, dim=di)

    # expansion done in %*%(FLQuant, FLQuant)
    return(FLQuant(rx) %*% y)
  }
) # }}}

# %/% {{{
# Multiply FLQuant against FLPar by matching dimnames, expands 1 to n
setMethod("%/%", signature(e1="FLPar", e2="FLQuant"),
	function(e1, e2) {

    # dims & dimnames
    de1 <- dim(e1)
    dne1 <- dimnames(e1)
    de2 <- dim(e2)
    dne2 <- dimnames(e2)

    # TEST: non-matching dnames in x should be of length 1
    ide2 <- !names(dne1) %in% names(dne2)
    if(any(de1[ide2] > 1))
      stop("dimensions in 'e1' not matching those in 'e2' must be of length=1")

    # result dims
    di <- rep(1, 6)
    di[dne2 %in% dne1] <- de1

    # x data in 6D array
    re1 <- array(e1@.Data, dim=di)

    # expansion done in %/%(FLQuant, FLQuant)
    return(FLQuant(re1) %/% e2)
  }
) # }}}

# %+% {{{
# Add FLQuant against FLPar by matching dimnames, expands 1 to n
setMethod("%+%", signature(x="FLPar", y="FLQuant"),
	function(x, y) {

    # dims & dimnames
    dx <- dim(x)
    dnx <- dimnames(x)
    dy <- dim(y)
    dny <- dimnames(y)

    # TEST: non-matching dnames in x should be of length 1
    idy <- !dnx %in% dny
    if(any(dx[idy] > 1))
      stop("dimensions in 'x' not matching those in 'y' must be of length=1")

    # result dims
    di <- rep(1, 6)
    di[dny %in% dnx] <- dx

    # x data in 6D array
    rx <- array(x@.Data, dim=di)

    # expansion done in %+%(FLQuant, FLQuant)
    return(FLQuant(rx) %+% y)
  }
) # }}}

# %-% {{{
# Substracting FLQuant against FLPar by matching dimnames, expands 1 to n
setMethod("%-%", signature(x="FLPar", y="FLQuant"),
	function(x, y) {

    # dims & dimnames
    dx <- dim(x)
    dnx <- dimnames(x)
    dy <- dim(y)
    dny <- dimnames(y)

    # TEST: non-matching dnames in x should be of length 1
    idy <- !dnx %in% dny
    if(any(dx[idy] > 1))
      stop("dimensions in 'x' not matching those in 'y' must be of length=1")

    # result dims
    di <- rep(1, 6)
    di[dny %in% dnx] <- dx

    # x data in 6D array
    rx <- array(x@.Data, dim=di)

    # expansion done in %-%(FLQuant, FLQuant)
    return(FLQuant(rx) %-% y)
  }
) # }}}
# }}}

# FLQuant, FLPar {{{
# %*% {{{
# Multiply FLPar against FLQuant by matching dimnames, expands 1 to n
setMethod("%*%", signature(x="FLQuant", y="FLPar"),
	function(x, y) {

    # dims & dimnames
    dx <- dim(x)
    dnx <- dimnames(x)
    dy <- dim(y)
    dny <- dimnames(y)

    # TEST: non-matching dnames in x should be of length 1
    idx <- !dny %in% dnx
    if(any(dy[idx] > 1))
      stop("dimensions in 'x' not matching those in 'y' must be of length=1")

    # result dims
    di <- rep(1, 6)
    di[dnx %in% dny] <- dy

    # x data in 6D array
    ry <- array(y@.Data, dim=di)

    # expansion done in %*%(FLQuant, FLQuant)
    return(x %*% FLQuant(ry))
  }
) # }}}

# %/% {{{
# Multiply FLQuant against FLPar by matching dimnames, expands 1 to n
setMethod("%/%", signature(e1="FLQuant", e2="FLPar"),
	function(e1, e2) {

    # dims & dimnames
    de1 <- dim(e1)
    dne1 <- dimnames(e1)
    de2 <- dim(e2)
    dne2 <- dimnames(e2)

    # TEST: non-matching dnames in x should be of length 1
    ide2 <- !names(dne2) %in% names(dne1)
    if(any(de2[ide1] > 1))
      stop("dimensions in 'e1' not matching those in 'e2' must be of length=1")

    # result dims
    di <- rep(1, 6)
    di[dne1 %in% dne2] <- de2

    # x data in 6D array
    re2 <- array(e2@.Data, dim=di)

    # expansion done in %/%(FLQuant, FLQuant)
    return(e1 %/% FLQuant(re2))
  }
) # }}}

# %+% {{{
# Add FLQuant against FLPar by matching dimnames, expands 1 to n
setMethod("%+%", signature(x="FLQuant", y="FLPar"),
	function(x, y) {

    # dims & dimnames
    dx <- dim(x)
    dnx <- dimnames(x)
    dy <- dim(y)
    dny <- dimnames(y)

    # TEST: non-matching dnames in x should be of length 1
    idx <- !dny %in% dnx
    if(any(dy[idx] > 1))
      stop("dimensions in 'x' not matching those in 'y' must be of length=1")

    # result dims
    di <- rep(1, 6)
    di[dnx %in% dny] <- dy

    # y data in 6D array
    ry <- array(y@.Data, dim=di)

    # expansion done in %+%(FLQuant, FLQuant)
    return(x %+% FLQuant(ry))
  }
) # }}}

# %-% {{{
# Substracting FLQuant against FLPar by matching dimnames, expands 1 to n
setMethod("%-%", signature(x="FLQuant", y="FLPar"),
	function(x, y) {

    # dims & dimnames
    dx <- dim(x)
    dnx <- dimnames(x)
    dy <- dim(y)
    dny <- dimnames(y)

    # TEST: non-matching dnames in x should be of length 1
    idx <- !dny %in% dnx
    if(any(dy[idx] > 1))
      stop("dimensions in 'x' not matching those in 'y' must be of length=1")

    # result dims
    di <- rep(1, 6)
    di[dnx %in% dny] <- dy

    # y data in 6D array
    ry <- array(y@.Data, dim=di)

    # expansion done in %-%(FLQuant, FLQuant)
    return(x %-% FLQuant(ry))
  }
) # }}}
# }}}

# FLPar, FLPar

