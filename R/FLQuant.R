# FLQuant.R - FLQuant class and methods
# FLCore/R/FLQuant.R

# Copyright 2003-2022 FLR Team. Distributed under the GPL 2 or later
# Maintainer: Iago Mosqueira (MWR)

# FLQuant(missing){{{

#' @rdname FLQuant
#' @aliases FLQuant,missing-method
#' @examples
#'
#' FLQuant()
#' summary(FLQuant())

setMethod("FLQuant", signature(object="missing"),
  function(object, dim=rep(1,6), dimnames="missing", quant=NULL,  
    units="NA", iter=1) {

    # no dim or dimnames
    if (missing(dim) && missing(dimnames)) {
      dim <- c(1,1,1,1,1,iter)
      dimnames <- list(quant='all', year=1, unit='unique', season='all',
        area='unique', iter=1:dim[6])
    } else if (missing(dim)) {
      # dim missing
      dimnames <- filldimnames(dimnames, iter=iter)
      dim <- as.numeric(sapply(dimnames, length))
    } else if (missing(dimnames)) {
    # dimnames missing
    dim <- c(dim, rep(1,6))[1:6]
        # but iter
        if(!missing(iter))
        dim[6] <- iter
        dimnames <- list(
            quant=if(dim[1]==1){"all"}else{1:dim[1]},
            year=1:dim[2],
            unit=if(dim[3]==1){"unique"}else{1:dim[3]},
            season=if(dim[4]==1){"all"}else{1:dim[4]},
            area=if(dim[5]==1){"unique"}else{1:dim[5]},
            iter=1:dim[6])
    } else {
        # both missing
        dim <- c(dim, rep(1,6))[1:6]
        # but iter
        if(!missing(iter))
            dim[6] <- iter
        dimnames <- filldimnames(dimnames, dim=dim, iter=iter)
    }

  flq <- new("FLQuant", array(as.numeric(NA), dim=dim, dimnames=dimnames),
    units=units)

    if (!is.null(quant))
        quant(flq) <- quant

    return(flq)
    }
)# }}}

# FLQuant(vector){{{

#' @rdname FLQuant
#' @aliases FLQuant,vector-method
#' @examples
#'
#' FLQuant(1:10)

setMethod("FLQuant", signature(object="vector"),
function(object, dim=rep(1,6), dimnames="missing", quant=NULL, units="NA", iter=1,
    fill.iter=TRUE) {
  
  # no dim or dimnames
  if (missing(dim) && missing(dimnames)) {
    dim <- c(1,length(object),1,1,1,iter)
    dimnames <- list(quant='all', year=1:length(object), unit='unique',
    season='all', area='unique', iter=seq(1,iter))
  }

  # dim missing
  else if (missing(dim)) {
    dimnames <- filldimnames(dimnames, iter=iter)
    dim <- as.numeric(sapply(dimnames, length))
  }

  # dimnames missing
  else if (missing(dimnames)) {
    dim <- c(dim, rep(1,6))[1:6]
    if(!missing(iter)) {
      dim[6] <- iter
    }
    dimnames <- list(
      quant=if(dim[1]==1){"all"}else{1:dim[1]},
      year=1:dim[2],
      unit=if(dim[3]==1){"unique"}else{1:dim[3]},
      season=if(dim[4]==1){"all"}else{1:dim[4]},
      area=if(dim[5]==1){"unique"}else{1:dim[5]},
      iter=1:dim[6])
  }
  # both provided
  else {
    dim <- c(dim, rep(1,6))[1:6]
    dimnames <- filldimnames(dimnames, dim=dim)
    if(!missing(iter)) {
      dim[6] <- iter
      dimnames <- filldimnames(dimnames, dim=dim, iter=iter)
    }
  }

  flq <- new("FLQuant", array(as.double(object), dim=dim, dimnames=dimnames),
    units=units)
  
  # Set extra iters to NA
  if(dims(flq)$iter > 1 && !fill.iter)
      flq[,,,,,2:dims(flq)$iter] <- as.numeric(NA)

  if (!is.null(quant))
    quant(flq) <- quant

  return(flq)
  }
) # }}}

# FLQuant(array){{{

#' @rdname FLQuant
#' @aliases FLQuant,array-method
#' @examples
#'
#' FLQuant(array(rnorm(9), dim=c(3,3,3)))

setMethod("FLQuant", signature(object="array"),
function(object, dim=rep(1,6), dimnames="missing", quant=NULL, units="NA",
  iter=1, fill.iter=TRUE) {

  # no dim or dimnames
  if (missing(dim) && missing(dimnames)) {
    # get dim from object and complete
    dim <- c(dim(object), rep(1,5))[1:6]
    # change dim[6] if iter is set
    if(!missing(iter))
      dim[6] <- iter
    # if object has dimnames, use then
    if(!is.null(dimnames(object))) {
      dimnames <- filldimnames(dimnames(object), dim=dim)
    }
    # otherwise create from dim
    else {
      dimnames <- list(quant=1:dim[1], year=1:dim[2], unit=1:dim[3],
      season=1:dim[4], area=1:dim[5], iter=1:dim[6])
      dimnames[which(dim==1)] <- list(quant='all', year=1, unit='unique',
      season='all', area='unique', iter='1')[which(dim==1)]
    }
  }

  # dim missing
  else if (missing(dim)) {
    if(missing(iter) && length(dim(object)) == 6)
      iter <- dim(object)[6]
    dimnames <- filldimnames(dimnames, dim=c(dim(object), rep(1,6))[1:6], iter=iter)
  # extract dim from dimnames
  dim <- c(dim(object),
    as.numeric(sapply(dimnames, length))[length(dim(object))+1:6])[1:6]
  if(!missing(iter))
    dim[6] <- iter
  }

  # dimnames missing
  else if (missing(dimnames)) {
    dim <- c(dim, rep(1,6))[1:6]
    if(!missing(iter))
      dim[6] <- iter
    # create dimnames from dim
    dimnames <- list(quant=1:dim[1], year=1:dim[2], unit=1:dim[3],
    season=1:dim[4], area=1:dim[5], iter=1:iter)
    dimnames[which(dim==1)] <- list(quant='all', year=1, unit='unique', season='all',
    area='unique', iter='1')[which(dim==1)]
  }
  flq <- new("FLQuant", array(as.double(object), dim=dim, dimnames=dimnames),
    units=units)

  # Set extra iters to NA, unless array has 6 dimensions
  if(dims(flq)$iter > 1 && !fill.iter)
    flq[,,,,,2:dims(flq)$iter] <- as.numeric(NA)

  if (!is.null(quant))
    quant(flq) <- quant

  return(flq)
  }
) # }}}

# FLQuant(matrix){{{

#' @rdname FLQuant
#' @aliases FLQuant,matrix-method
#' @examples
#' FLQuant(matrix(rnorm(12), nrow=4, ncol=3))

setMethod("FLQuant", signature(object="matrix"),
  function(object, dim=lapply(dimnames, length), dimnames="missing", ...) {

  if(missing(dim))
    dim <- c(nrow(object), ncol(object), rep(1,5))[1:6]
  
  if(!missing(dimnames))
    return(FLQuant(array(object, dim=dim, dimnames=filldimnames(dimnames, dim=dim)), ...))

  if(!is.null(dimnames(object)) && missing(dimnames))
    return(FLQuant(array(object, dim=dim), dimnames=filldimnames(dimnames(object),
      dim=dim), ...))

  return(FLQuant(array(object, dim=dim), ...))
  }
) # }}}

# FLQuant(FLQuant){{{

#' @rdname FLQuant
#' @aliases FLQuant,FLQuant-method
#' @examples
#'
#' FLQuant(FLQuant(array(rnorm(9), dim=c(3,3,3)), units='kg'), units='t')

setMethod("FLQuant", signature(object="FLQuant"),
  function(object, quant=attributes(object)[['quant']], units=attributes(object)[['units']],
    dimnames=attributes(object)[['dimnames']], iter=dim(object)[6], fill.iter=TRUE,
    dim=attributes(object)[['dim']])
  {
    # generate dimnames
    dnames <- dimnames(object)
    dnames[names(dimnames)]   <- lapply(dimnames, as.character)

    # dim
    if(!missing(dim))
    {
      dims <- dim(object)
      if(any(dim > dims[1:length(dim)]))
        stop("resizing an object using 'dim' only allowed for trimming: use dimnames")
      dims[1:length(dim)] <- dim
      for(i in seq(length(dnames)))
      {
        dnames[[i]] <- dnames[[i]][1:dims[i]]
      }
    }

    # change iter
    if(!missing(iter))
      dnames['iter'] <- list(as.character(seq(length=iter)))

    # create empty FLQuant
    res <- FLQuant(dimnames=dnames, quant=quant, units=units)

    odnames <- dimnames(object)
    if(!missing(iter))
      odnames$iter <- seq(1, length(odnames$iter))
    if(fill.iter==TRUE)
      res[odnames[[1]][odnames[[1]]%in%dnames[[1]]],
          odnames[[2]][odnames[[2]]%in%dnames[[2]]],
          odnames[[3]][odnames[[3]]%in%dnames[[3]]],
          odnames[[4]][odnames[[4]]%in%dnames[[4]]],
          odnames[[5]][odnames[[5]]%in%dnames[[5]]],]  <-
      object[odnames[[1]][odnames[[1]]%in%dnames[[1]]],
             odnames[[2]][odnames[[2]]%in%dnames[[2]]],
             odnames[[3]][odnames[[3]]%in%dnames[[3]]],
             odnames[[4]][odnames[[4]]%in%dnames[[4]]],
             odnames[[5]][odnames[[5]]%in%dnames[[5]]],]
    else
      res[odnames[[1]][odnames[[1]]%in%dnames[[1]]],
          odnames[[2]][odnames[[2]]%in%dnames[[2]]],
          odnames[[3]][odnames[[3]]%in%dnames[[3]]],
          odnames[[4]][odnames[[4]]%in%dnames[[4]]],
          odnames[[5]][odnames[[5]]%in%dnames[[5]]],
          odnames[[6]][odnames[[6]]%in%dnames[[6]]]]  <-
      object[odnames[[1]][odnames[[1]]%in%dnames[[1]]],
             odnames[[2]][odnames[[2]]%in%dnames[[2]]],
             odnames[[3]][odnames[[3]]%in%dnames[[3]]],
             odnames[[4]][odnames[[4]]%in%dnames[[4]]],
             odnames[[5]][odnames[[5]]%in%dnames[[5]]],
             odnames[[6]][odnames[[6]]%in%dnames[[6]]]]

    # listo!
return(res)
}
)# }}}

# as.FLQuant(array){{{
setMethod("as.FLQuant", signature(x="array"),
  function(x, ...) {
    return(FLQuant(x, ...))
  }
)

setAs("array", "FLQuant", function(from)
  return(FLQuant(from)))
# }}}

# as.FLQuant(matrix){{{
setMethod("as.FLQuant", signature(x="matrix"),
  function(x, ...) {
    return(FLQuant(x, ...))
  }
)
setAs("matrix", "FLQuant", function(from)
  return(FLQuant(from)))
# }}}

# as.FLQuant(FLQuant){{{
setMethod("as.FLQuant", signature(x="FLQuant"),
  function(x, ...) {
    return(FLQuant(x, ...))
  }
)# }}}

# as.FLQuant(vector){{{
setMethod("as.FLQuant", signature(x="vector"),
function(x, ...) {
return(FLQuant(x, ...))
}
)
setAs("vector", "FLQuant", function(from)
  return(FLQuant(from)))
# }}}

# as.FLQuant(data.frame){{{
setMethod("as.FLQuant", signature(x="data.frame"),
function(x, units="NA", ...) {

    # get data.frame names and compare
    names(x) <- tolower(names(x))

    # extract units if set
    if("units" %in% names(x)) {
      if(missing(units)) {
          units <- unique(as.character(x$units))
      }
        if(length(units) > 1)
          stop("Two or more units of measurement set in 'units' column")
      x$units <- NULL
    }

    validnames <-c("year", "unit", "season", "area", "iter", "data")

    indices <- match(validnames, names(x))
    indices <- indices[!is.na(indices)]

    # get quant
    qname <- names(x)
    qname[indices] <- NA
    qname <- qname[!is.na(qname)]

    if (length(qname) > 1)
      stop("too many columns in data.frame")
    if(length(qname) == 0)
      qname <- "quant"

    # sort years if present
    if('year' %in% names(x))
      x <- x[order(x$year),]

    # check and fill up missing dimensions
    n <- dim(x)[1]
    # TODO conversion to/x factor messes up dimnames order
    em <- data.frame(quant=rep('all', n), year=rep(1,n), unit=rep('unique',n),
      season=rep('all',n), area=rep('unique',n), iter=rep(1,n), 
      stringsAsFactors=FALSE)
    names(em)[names(em)=="quant"] <- qname

    x[, !names(x) %in% c("data")] <- as.data.frame(x[,!names(x) %in% c("data")],
      stringsAsFactors=FALSE)
    em[names(x)] <- x

    # create array
    flq <- tapply(em[,"data"],
      list(factor(x = em[,qname], levels = unique(em[,qname])),
        factor(x = em[,"year"], levels = unique(em[,"year"])),
        factor(x = em[,"unit"], levels = unique(em[,"unit"])),
        factor(x = em[,"season"], levels = unique(em[,"season"])),
        factor(x = em[,"area"], levels = unique(em[,"area"])),
        factor(x = em[,"iter"], levels = unique(em[,"iter"]))),
      sum)

    # fix dimnames names
    names(dimnames(flq)) <- c(qname, 'year', 'unit', 'season', 'area', 'iter')

    # create FLQuant
    flq <- FLQuant(flq, units=units, ...)

    # units
    if(exists("units"))
      units(flq) <- units
    else if(!is.null(attr(x, 'units')))
      units(flq) <- attr(x, 'units')

    # fill up missing years
    if(length(dimnames(flq)[['year']]) != length(as.character(seq(dims(flq)$minyear,
      dims(flq)$maxyear))))
    {
      res <- FLQuant(dimnames=c(dimnames(flq)[1], list(year=seq(dims(flq)$minyear,
        dims(flq)$maxyear)), dimnames(flq)[3:6]), units=units, ...)
      res[,dimnames(flq)[['year']],] <- flq
      flq <- res
    }
return(flq)
  }
) # }}}

# dimnames<-       {{{
setMethod("dimnames<-", signature(x="FLQuant", value='list'),
  function(x, value) {

    if(length(names(value)[!names(value)%in%c("year","unit","season","area","iter")]) > 1)
      stop("more than one vector of names given for the first dimension")

    xnames <- dimnames(x)

    for(i in 1:length(value)) {
      if(any(names(value)[i]==c("year","unit","season","area","iter")))
        xnames[[names(value)[i]]] <- value[[i]]
      else {
      xnames[[1]] <- value[[i]]
      names(xnames)[1] <- names(value)[i]
      }
    }
    attributes(x)$dimnames <- xnames

    return(x)
  }
) # }}}

# dims       {{{

#' @rdname dims
#' @aliases dims,FLQuant-method
#' @examples
#' 
#' flq <- FLQuant(rnorm(96), dim=c(3,8,1,4), quant='age')
#' dims(flq)
#' 
#' # Number of seasons
#'   dims(flq)$season
#' 
#' # Length of first dimension
#'   dims(flq)[[quant(flq)]]
#'

setMethod("dims", signature(obj="FLQuant"),
  function(obj, element, ...){

    names <- names(dimnames(obj))
    quant   <-  as.numeric(dim(obj)[names == quant(obj)])
    min <- suppressWarnings(as.numeric(dimnames(obj)[[quant(obj)]][1]))
    max <- suppressWarnings(as.numeric(dimnames(obj)[[quant(obj)]][length(dimnames(obj)[[quant(obj)]])]))
    year<-  as.numeric(dim(obj)[names == "year"])
    minyear <-  suppressWarnings(as.numeric(dimnames(obj)$year[1]))
    maxyear <-  suppressWarnings(as.numeric(dimnames(obj)$year[dim(obj)[names == "year"]]))
    unit<-  dim(obj)[names == "unit"]
    season  <-  dim(obj)[names == "season"]
    area <-  dim(obj)[names == "area"]
    iter <- dim(obj)[names == "iter"]
    list <- list(quant=quant, min=min, max=max, year=year, minyear=minyear,
    maxyear=maxyear, unit=unit, season=season, area=area, iter=iter)
    names(list)[1] <- quant(obj)

    if(!missing(element))
      return(list[[element]])

    return(list)
  }
)   # }}}

# ages {{{

#' @examples
#' ages(m(ple4))
#' # Seasonal objects get decimal ages
#' ages(expand(m(ple4), season=1:4))[,,,1]
#' ages(expand(m(ple4), season=1:4))[,,,2]

setMethod("ages", signature(object="FLQuant"),
  function(object) {
    
  res <- FLQuant(an(dimnames(object)$age), dimnames=dimnames(object),
    units="")

  # TODO: DEAL with (spawning) units + seasons (e.g. SKJ)

  # SEASONAL object
  if(dim(res)[4] > 1) {

    nseas <- dim(object)[4]
    
    seas <- do.call(expand, c(list(x=FLQuant(seq(0, length=nseas, by=1 / nseas),
      dimnames=list(season=dimnames(object)$season), quant=quant(object), 
      units="")), dimnames(object)[-4]))

    res <- res + seas
  }

  return(res)
  }
)
# }}}

# is.FLQuant       {{{
is.FLQuant  <-  function(x)
return(is(x, "FLQuant"))
# }}}

# totals {{{
setMethod('quantTotals', signature(x='FLQuant'),
  function(x, na.rm=TRUE) {
    sums <- x
    for (i in 1:dim(x)[2])
      sums[,i,,,,] <- rowSums(x, na.rm=na.rm, dims=1)
    return(sums)
  }
)

setMethod('yearTotals', signature(x='FLQuant'),
  function(x, na.rm=TRUE) {
    sums <- x
    for (i in 1:dim(x)[1])
      sums[i,,,,] <- colSums(x, na.rm=na.rm)[,1,1,1,1]
    return(sums)
  }
) # }}}

# sums         {{{

#' @rdname dimSummaries
setMethod('quantSums', signature(x='FLQuant'), function(x, na.rm=TRUE) {

  res <- colSums(x, na.rm=na.rm)
  dim(res) <- c(1, dim(res))

  # FIX all NAs
  if(na.rm & any(res == 0)) {
    z <- is.na(x)
    y <- colSums(z) == dim(x)[1]
    res[y] <- NA
  }

  return(FLQuant(res, dimnames= c(list(quant='all'),dimnames(x)[2:6]),
    quant=quant(x), units=units(x)))
})

#' @rdname dimSummaries
setMethod('yearSums', signature(x='FLQuant'), function(x, na.rm=TRUE) {
  return(apply(x,c(1,3,4,5,6), function(x, NA.RM=na.rm){
    z <- x[!is.na(x)]; ifelse(length(z), sum(z, na.rm=NA.RM), as.numeric(NA))
  }))
})

#' @rdname dimSummaries
setMethod('unitSums', signature(x='FLQuant'), function(x, na.rm=TRUE) {
  return(FLQuant(c(rowSums(aperm(x, c(1,2,4,5,6,3)), na.rm=na.rm, dims=5)),
  dimnames=c(dimnames(x)[-3], unit='unique'), units=units(x)))
})

#' @rdname dimSummaries
setMethod('seasonSums', signature(x='FLQuant'), function(x, na.rm=TRUE) {
 
  # output, 1 season
  res <- x[,,,1]
  dimnames(res)$season <- "all"
  
  # DROP dimnames
  dimnames(x) <- NULL
  # PERMUTATE to season first
  x <- aperm(x, c(4,1,2,3,5,6))

  # SUM across dim 1
  res[] <- colSums(x, dims=1, na.rm=TRUE)

  idx <- colSums(is.na(x), dims=1) == dim(x)[1]
  res[idx] <- as.numeric(NA)

  return(res)
})

#' @rdname dimSummaries
setMethod('areaSums', signature(x='FLQuant'), function(x, na.rm=TRUE) {
  return(apply(x,c(1,2,3,4,6), function(x, NA.RM=na.rm){
    z <- x[!is.na(x)]; ifelse(length(z), sum(z, na.rm=NA.RM), as.numeric(NA))
  }))
})

#' @rdname dimSummaries
setMethod('iterSums', signature(x='FLQuant'), function(x, na.rm=TRUE) {
  return(apply(x,1:5, function(x, NA.RM=na.rm){
    z <- x[!is.na(x)]; ifelse(length(z), sum(z, na.rm=NA.RM), as.numeric(NA))
  }))
})

# }}}

# means         {{{
#' @rdname dimSummaries
setMethod('quantMeans', signature(x='FLQuant'), function(x, na.rm=TRUE) {
  return(FLQuant(c(colMeans(x, na.rm=na.rm, dims=1)),
    dimnames=c(dimnames(x)[-1], quant='all'), quant=quant(x), units=units(x)))
})

#' @rdname dimSummaries
setMethod('yearMeans', signature(x='FLQuant'), function(x, na.rm=TRUE) {
  return(FLQuant(c(rowMeans(aperm(x, c(1,3,4,5,6,2)), na.rm=na.rm, dims=5)),
  dimnames=c(dimnames(x)[-2], year='1'), units=units(x)))
})

#' @rdname dimSummaries
setMethod('unitMeans', signature(x='FLQuant'), function(x, na.rm=TRUE) {
  return(FLQuant(c(rowMeans(aperm(x, c(1,2,4,5,6,3)), na.rm=na.rm, dims=5)),
  dimnames=c(dimnames(x)[-3], unit='unique'), units=units(x)))
})

#' @rdname dimSummaries
setMethod('seasonMeans', signature(x='FLQuant'), function(x, na.rm=TRUE) {
  return(FLQuant(c(rowMeans(aperm(x, c(1,2,3,5,6,4)), na.rm=na.rm, dims=5)),
  dimnames=c(dimnames(x)[-4], season='all'), units=units(x)))
})

#' @rdname dimSummaries
setMethod('areaMeans', signature(x='FLQuant'), function(x, na.rm=TRUE) {
  return(FLQuant(c(rowMeans(aperm(x, c(1,2,3,4,6,5)), na.rm=na.rm, dims=5)),
  dimnames=c(dimnames(x)[-5], area='unique'), units=units(x)))
})

#' @rdname dimSummaries
setMethod('iterMeans', signature(x='FLQuant'), function(x, na.rm=TRUE) {
  return(FLQuant(c(rowMeans(x, na.rm=na.rm, dims=5)),
  dimnames=c(dimnames(x)[-6], iter='1'), units=units(x)))
})
# }}}

# medians {{{
#' @rdname dimSummaries
setMethod('yearMedians', signature(x='FLQuant'), function(x, na.rm=TRUE) {
  return(apply(x, c(1, 3:6), median, na.rm=na.rm))
}) 
#' @rdname dimSummaries
setMethod('iterMedians', signature(x='FLQuant'), function(x, na.rm=TRUE) {
  return(apply(x, c(1:5), median, na.rm=na.rm))
}) 

# }}}

# vars         {{{
#' @rdname dimSummaries
setMethod('quantVars', signature(x='FLQuant'), function(x, na.rm=TRUE) {
  return(apply(x, 2:6, var, na.rm=na.rm))
})

#' @rdname dimSummaries
setMethod('yearVars', signature(x='FLQuant'), function(x, na.rm=TRUE) {
  return(apply(x, c(1,3:6), var, na.rm=na.rm))
})

#' @rdname dimSummaries
setMethod('unitVars', signature(x='FLQuant'), function(x, na.rm=TRUE) {
  return(apply(x, c(1:2,4:6), var, na.rm=na.rm))
})

#' @rdname dimSummaries
setMethod('seasonVars', signature(x='FLQuant'), function(x, na.rm=TRUE) {
  return(apply(x, c(1:3,5:6), var, na.rm=na.rm))
})

#' @rdname dimSummaries
setMethod('areaVars', signature(x='FLQuant'), function(x, na.rm=TRUE) {
  return(apply(x, c(1:4,6), var, na.rm=na.rm))
})

#' @rdname dimSummaries
setMethod('iterVars', signature(x='FLQuant'), function(x, na.rm=TRUE) {
  return(apply(x, c(1:5), var, na.rm=na.rm))
}) # }}}

# CVs {{{
#' @rdname dimSummaries
setMethod('iterCVs', signature(x='FLQuant'), function(x, na.rm=TRUE) {
  return(sqrt(iterVars(x))/iterMeans(x))
}) # }}}

# prob {{{
#' @rdname dimSummaries
setMethod('iterProb', signature(x='FLQuant'), function(x, na.rm=TRUE) {
  return(apply(x, c(1:5), sum, na.rm=na.rm) / dim(x)[6])
}) # }}}

# yearSample {{{

#' Samples along the year dimension
#'
#' A resample from an FLQuant object along the 'year' dimension is returned. The
#' 'year' dimnames of the output object can be specified, although that is not
#' needed if the resample is to be assigned in a slot.
#'
#' @param x An FLQuant object.
#' @param size Number of samples (years), non-negative integer.
#' @param years Optional vector to set as 'year' dimnames in output.
#' @param replace should sampling be with replacement? Defaults to TRUE.
#' @param prob a vector of probability weights.
#'
#' @return RETURN Description, class
#'
#' @author Iago Mosqueira (WMR)
#' @seealso [FLQuant-class] [sample()]
#' @keywords classes
#' @examples
#' data(ple4)
#' # Take 20 samples of recent recruitment 
#' yearSample(rec(ple4)[, ac(2013:2017)], 20)
#' # Providing 'years' sets the output object dimnames
#' yearSample(rec(ple4)[, ac(2013:2017)], 20, year=2000:2019)

yearSample <- function(x, size=length(years), years, replace=TRUE, prob=NULL) {

  # STOP if !FLQuant
  if(!is(x, "FLQuant"))
    stop("'yearSample' expects an 'FLQuant' with year as 2nd dimension")

  # SAMPLE along 2nd (year) dimension
  res <- x[, sample(seq(dim(x)[2]), size, replace=replace, prob=prob)]
  
  # ASSIGn year dimnames if provided
  if(!missing(years))
    dimnames(res)$year <- years
  
  return(res)
}
# }}}

# quantile   {{{
setMethod("quantile", signature(x="FLQuant"),
  function(x, probs=seq(0, 1, 0.25), na.rm=FALSE, ...) {
    res <- FLQuant(NA, dimnames=c(dimnames(x)[-6],
      list(iter=paste(probs*100, "%", sep=""))), units=units(x))
  res[,,,,,] <- aperm(apply(x@.Data, 1:5, quantile, c(0, probs), na.rm=na.rm),
    c(2:6,1))[,,,,,-1,drop=FALSE]
  return(res)
  }
) # }}}

# iters     {{{
setGeneric("iters", function(object, ...) {
value  <-  standardGeneric("iters")
value
}
)

setMethod("iters", signature(object="FLQuant"),
function(object) {
for (i in dimnames(object)$iter) {
cat("-- iter: ", i,"\n")
print(object@.Data[,,,,,i])
}
cat("\nunits: ", object@units, "\n")
}
)   # }}}

# iter<-     {{{
setMethod("iter<-", signature(object="FLQuant", value="FLQuant"),
  function(object, iter, value) {
    object[,,,,,iter] <- value
    return(object)
  }
) 

setMethod("iter<-", signature(object="FLQuant", value="numeric"),
  function(object, iter, value) {
    object[,,,,,iter] <- c(value)
    return(object)
  }
) 
# }}}

# propagate {{{

#' @rdname propagate
#' @param object Object to be propagated.
#' @param iters No. of iterations in output.
#' @param fill.iter Should first array be copied to others? Defaults to FALSE.
#' @examples
#' 
#' # An FLQuant with one iter (dim(flq)[6] == 1)
#' flq <- FLQuant(rnorm(80), dim=c(4,20), quant='age')
#'
#' # can now be extended along the `iter` dimension, with
#' #' copies of the first
#' propagate(flq, 100)
#'
#' # or without
#' iter(propagate(flq, 100, fill.iter=FALSE), 2)

setMethod("propagate", signature(object="FLQuant"),
  function(object, iter, fill.iter=TRUE) {
    
    # RETURN object if iter == iters
    dob <- dim(object)

    if(iter == dob[6])
      return(object)

    # CHECK no iters in object
    if(dob[6] > 1)
      stop("propagate can only extend objects with no iters")

    # fill.iter
    if(fill.iter) {
      return(new('FLQuant', array(rep(c(object), iter), dim=c(dob[-6], iter),
        dimnames=c(dimnames(object)[-6], list(iter=seq(iter)))),
        units=units(object)))
    # or NAs
    } else {
      return(new('FLQuant', array(c(object, rep(NA, prod(dob)*(iter-1))),
        dim=c(dim(object)[-6], iter),
        dimnames=c(dimnames(object)[-6], list(iter=seq(iter)))), units=units(object)))
    }
  }
) # }}}

# rnorm{{{
setMethod("rnorm", signature(n="numeric", mean="FLQuant", sd="FLQuant"),
  function(n=1, mean, sd) {
    if(!dim(mean)[6] %in% c(1, n) | !dim(sd)[6]  %in% c(1, n))
      stop("mean or sd can only have iter=1")
    if(any(dim(mean) != dim(sd)))
      stop("dims of mean and sd must be equal")
    FLQuant(array(rnorm(prod(dim(mean)[-6]) * n, rep(iter(mean, 1)[drop=TRUE], n),
      rep(iter(sd, 1)[drop=TRUE], n)), dim=c(dim(mean)[-6], n)),
      dimnames=c(dimnames(mean)[-6], list(iter=seq(n))), units=units(mean))
  }
)

setMethod("rnorm", signature(n="numeric", mean="FLQuant", sd="numeric"),
  function(n=1, mean, sd) {
    rnorm(n, mean, FLQuant(sd, dimnames=dimnames(mean)))
  }
)
setMethod("rnorm", signature(n="numeric", mean="numeric", sd="FLQuant"),
  function(n=1, mean, sd) {
    rnorm(n, FLQuant(mean, dimnames=dimnames(sd)), sd)
  }
)
setMethod("rnorm", signature(n="numeric", mean="FLQuant", sd="missing"),
  function(n=1, mean) {
    rnorm(n, mean, 1)
  }
)
setMethod("rnorm", signature(n="numeric", mean="missing", sd="FLQuant"),
  function(n=1, sd) {
    rnorm(n, 0, sd)
  }
)
setMethod("rnorm", signature(n="missing", mean="FLQuant", sd="numeric"),
  function(mean, sd=1) {
    mean[] <- rnorm(length(mean), c(mean), sd)
    return(mean)
  }
)
setMethod("rnorm", signature(n="missing", mean="numeric", sd="FLQuant"),
  function(mean=0, sd) {
    sd[] <- rnorm(length(sd), mean, c(sd))
    return(sd)
  }
)
setMethod("rnorm", signature(n="missing", mean="FLQuant", sd="FLQuant"),
  function(mean, sd) {
    if(!all(dim(mean) == dim(sd)))
      stop("dimensions of 'mean' and 'sd' must be the same")
    mean[] <- rnorm(length(mean), c(mean), c(sd))
    return(mean)
  }
)
# }}}

# rlnorm {{{
setMethod("rlnorm", signature(n="numeric", meanlog="FLQuant", sdlog="FLQuant"),
  function(n=1, meanlog, sdlog) {

    # CHECK iters match
    if(!all(dim(meanlog)[6] %in% c(1, n), dim(sdlog)[6] %in% c(1, n)))
      stop("meanlog or sdlog can only have iter=1 or n")

    # CHECK dims match
    if(any(dim(meanlog)[-6] != dim(sdlog)[-6]))
      stop("dims of mean and sd, except iter, must be equal")

    mlog <- rep(c(meanlog), n / dim(meanlog)[6])
    slog <- rep(c(sdlog), n / dim(sdlog)[6])
    ns <- prod(dim(meanlog)[-6]) * n

    return(FLQuant(array(rlnorm(ns, mlog, slog)), dim=c(dim(meanlog)[-6], n),
      dimnames=c(dimnames(meanlog)[-6], list(iter=seq(n))),
      units=units(meanlog)))
  }
)

setMethod("rlnorm", signature(n='numeric', meanlog="FLQuant", sdlog="numeric"),
  function(n=1, meanlog, sdlog) {
    rlnorm(n, meanlog, FLQuant(sdlog, dimnames=dimnames(meanlog)))
  }
)

setMethod("rlnorm", signature(n='numeric', meanlog="numeric", sdlog="FLQuant"),
  function(n=1, meanlog, sdlog)
    rlnorm(n, FLQuant(meanlog, dimnames=dimnames(sdlog)), sdlog)
)

setMethod("rlnorm", signature(n='numeric', meanlog="FLQuant", sdlog="missing"),
  function(n=1, meanlog, sdlog)
    rlnorm(n, meanlog, 1)
)
setMethod("rlnorm", signature(n='numeric', meanlog="missing", sdlog="FLQuant"),
  function(n=1, meanlog, sdlog)
    rlnorm(n, 0, sdlog)
)

setMethod("rlnorm", signature(n='FLQuant', meanlog="ANY", sdlog="ANY"),
  function(n, meanlog=0, sdlog=1) {
    FLQuant(rlnorm(length(n), meanlog, sdlog), dimnames=dimnames(n), units=units(n))
  }
)
setMethod("rlnorm", signature(n="missing", meanlog="FLQuant", sdlog="ANY"),
  function(meanlog, sdlog=1) {
    meanlog[] <- rlnorm(length(meanlog), c(meanlog), sdlog)
    return(meanlog)
  }
)
setMethod("rlnorm", signature(n="missing", meanlog="FLQuant", sdlog="FLQuant"),
  function(meanlog, sdlog) {
    if(!all(dim(meanlog)[-6] == dim(sdlog)[-6]))
      stop("dimensions of 'meanlog' and 'sdlog' must be the same")
    meanlog[] <- rlnorm(length(meanlog), c(meanlog), c(sdlog))
    return(meanlog)
  }
)
# }}}

# rpois{{{
setMethod("rpois", signature(n='numeric', lambda="FLQuant"),
function(n=1, lambda) {
    if(dim(lambda)[6] > 1)
      stop("lambda can only have iter=1")
FLQuant(array(rnorm(prod(dim(lambda)[-6])*n, rep(iter(lambda, 1)[drop=TRUE], n)),
      dim=c(dim(lambda)[-6], n)),
dimnames=c(dimnames(lambda)[-6], list(iter=seq(n))), fill.iter=TRUE, units=units(lambda))
}
) # }}}

# mvrnorm {{{
setMethod("mvrnorm",
  signature(n="numeric", mu="FLQuant", Sigma="matrix"),
  function(n=1, mu, Sigma) {

    dmu <- dim(mu)

    # input checks
    if(any(dmu[3:6] > 1))
      stop("mvrnorm can only work on 'quant' by 'year' FLQuant objects")
    if (!isSymmetric(Sigma, tol = sqrt(.Machine$double.eps),
      check.attributes = FALSE)) {
      stop("Sigma must be a symmetric matrix")
    }
    if (dmu[1] != nrow(sigma)) {
      stop("mu and Sigma have non-conforming size")
    }

    # "chol" method, from mvtnorm pkg
    retval <- chol(Sigma, pivot = TRUE)
    o <- order(attr(retval, "pivot"))
    retval <- retval[, o]

    retval <- matrix(rnorm(n * prod(dmu)), nrow = n * dmu[2]) %*% retval

    # iter, year, age, unit, season, area
    dim(retval)<-c(n, dmu[2], dmu[1], 1, 1, 1)

    # aperm into FLQ
    retval <- mu + FLQuant(aperm(retval, c(3,2,4,5,6,1)))

    return(retval)
  }
)

setMethod("mvrnorm",
  signature(n="numeric", mu="FLQuant", Sigma="missing"),
  function(n=1, mu) {

    #
    Sigma <- cov(t(mu[, drop=TRUE]))

    return(mvrnorm(n, mu, Sigma))

  }
)# }}}

# PV{{{
setGeneric("pv", function(object, ...)
standardGeneric("pv"))

# Heath. 2006. Oikos 115:573-581
setMethod('pv', signature(object='FLQuant'),
function(object, dist=FALSE)
{
# dimensions (currently working for (1,n,1,1,1,1)
if(any(dim(object)[c(1,3:6)] != rep(1,5)))
stop('the pv method is currently defined for yearly time series only, dim = c(1,n,1,1,1,1)')

# delete NAs
object <- as.vector(object)
object <- object[!is.na(object)]

# number of possible combinations (Eq. 1)
len <- length(object)
c <- len*(len-1)/2

# all possible combinations
grid <- as.data.frame(t(combn(as.vector(object), 2)))

# absolut value (Eq. 2)
grid$Abs <- abs(grid$V1-grid$V2)

# max and min by row (Eq. 2)
grid$Max <- apply(cbind(grid$V1, grid$V2), 1, max)
grid$Min <- apply(cbind(grid$V1, grid$V2), 1, min)

# calculate PV and D(PV)
pv <- grid$Abs/grid$Max
pv[grid$Abs == 0]  <- 0
pv <- sum(pv) / c
pvd <- 1- (grid$Min/grid$Max)
pvd[grid$Abs == 0]  <- 0

if(dist == TRUE)
return(pvd)
return(pv)
}
)
# }}}

# setPlusGroup{{{
setMethod("setPlusGroup", signature(x='FLQuant', plusgroup='numeric'),
function(x, plusgroup, na.rm=FALSE, by='sum') {
# only valid for age-based FLQuant
if(quant(x) != 'age')
stop('setPlusGroup onoy makes sense for age-based FLQuant objects')
# plusgroup not < than minage
  if ((plusgroup) < dims(x)$min)
  stop("plusgroup < min age")

  # expand
  if ((plusgroup) > dims(x)$max) {
    dmns <-dimnames(x)
    dmns$age <-dims(x)$min:plusgroup
    oldMaxage<-dims(x)$max

    # create extra ages and fill with plusgroup
    res  <-FLQuant(x, dimnames=dmns)
    res[ac((oldMaxage+1):plusgroup)] <- 0;
    res[ac((oldMaxage+1):plusgroup)] <- sweep(res[ac((oldMaxage+1):plusgroup)],
      2:6, res[ac(oldMaxage)],"+")

    if (by != "mean")
      res[ac((oldMaxage + 1):plusgroup)] <- res[ac((oldMaxage + 1):plusgroup)]/
        (plusgroup-oldMaxage + 1)

  }
  # trim
  else {
    res <- trim(x, age=dims(x)$min:plusgroup)
    res[as.character(plusgroup)] <- switch(by,
      'mean'= quantMeans(x[as.character(plusgroup:dims(x)$max)], na.rm=na.rm),
      'sum'= quantSums(x[as.character(plusgroup:dims(x)$max)], na.rm=na.rm))
  }

return(res)
}
)# }}}

# as.data.frame(FLQuant) {{{
setMethod("as.data.frame", signature(x="FLQuant", row.names="missing",
  optional="missing"),
    function(x, cohort=FALSE, timestep=FALSE, date=FALSE, drop=FALSE, units=FALSE) {
        as.data.frame(x, row.names=NULL, cohort=cohort, timestep=timestep,
            date=date, drop=drop, units=units)
    }
)
setMethod("as.data.frame", signature(x="FLQuant", row.names="ANY",
  optional="missing"),
function(x, row.names, cohort=FALSE, timestep=FALSE, date=FALSE, drop=FALSE,
  units=FALSE) {

    res <- callNextMethod(x)

    # create cohort column as year - age
    if(cohort) {
      res$cohort  <-  "all"
      if(quant(x) == "age")
        if(!any(is.na(dims(x)[c('min', 'max')])))
          res$cohort <- res$year - res$age
    }

    # create timestep column
    if(timestep) {
        res$timestep <- (as.numeric(res$year) - min(res$year)) * dim(x)[4] +
            as.numeric(res$season)
    }

    # create date column
    if(date) {
      if(dim(x)[4] == 12)
        res$date <- ISOdate(res$year, res$season, 1)
      else {
        lens <- (ISOdate(2014, 12, 31) - ISOdate(2014, 1, 1)) / dim(x)[4]
        res$date <- ISOdate(res$year, 1, 1) + lens *
          (as.numeric(res$season) - 1)
      }
      # ADD decade and lustrum
      res$decade <- res$year - (res$year %% 10)
      res$lustrum <- res$year - (res$year %% 5)
    }

    # drops columns with a single value, i.e. dims of length=1
    if(drop) {
      idx <- names(x)[dim(x) == 1]
      res <- res[, !colnames(res) %in% idx]
    }

    # create units column
    if(units) {
      res$units <- units(x)
    }

    return(res)
  }
) # }}}

# divide {{{

setMethod("divide", signature(object="FLQuant"),
  function(object, dim=6, names=dimnames(object)[[dim]]) {
    
    # LENGTH in dim
    idx <- dim(object)[dim[1]]

    if(idx == 1)
      return(object)

    # CALL [ on dim
    res <- lapply(seq(idx), function(i) {
      args <- list(x=object, d=i)
      names(args)[2] <- c("i", "j", "k", "l", "m", "n")[dim[1]]
      do.call("[", args)
    })

    # RENAME
    names(res) <- names
 
    return(do.call(getPlural(res[[1]]), res))

  }
) # }}}

# join {{{

#' @rdname join
#' @examples
#' data(ple4)
#' # JOIN over age dimension
#' x <- catch.n(ple4)[1,]
#' y <- catch.n(ple4)[2,]
#' join(x, y)
#' # JOIN over year dimension
#' x <- catch.n(ple4)[,10:20]
#' y <- catch.n(ple4)[,21:25]
#' join(x, y)

setMethod('join', signature(x='FLQuant', y='FLQuant'),
  function(x, y) {

    args <- c(list(x, y))

    # GET dim(s) & dimnames
    ds <- lapply(args, dim)
    dns <- lapply(args, dimnames)

    # FIND dim to join over
    dif <- unlist(Map(function(x, y) identical(x, y),
      x=dns[[1]], y=dns[[2]]))
    dm <- seq(1, 6)[!dif]

    # CHECK dimnames in only one dim is different
    if(length(dm) > 1)
      stop("Objects can only differ in one dimension.")

    # SETUP new dimnames
    ndns <- dns[[1]]
    ndns[[dm]] <- c(dns[[1]][[dm]], dns[[2]][[dm]])
    
    # CREATE output object, units as in x
    res <- FLQuant(NA, dimnames=ndns, units=units(x))

    # ASSIGN by dm
    for(i in seq(args)) {
      pos <- setNames(dns[[i]][dm], nm=letters[9:14][dm])
      res <- do.call("[<-", c(list(x=res, value=args[[i]]), pos))
    }

    return(res)
  }
) # }}}

# split {{{

#' splits *x* along the *iter* dimension into the groups defined by *f*.
#'
#' Similar to  base::split, but working along the 6th, *iter*, dimension of
#' any singular FLR object. The object is divided into as many objects as
#' unique values in *f*, and returned as an FLlst-derived object, e.g. an
#' FLQuants object when applied to an FLQuant.
#'
#' @param x The object to be split.
#' @param f The vector of group names.
#'
#' @return An object of the corresponding plural class (FLQuants from FLQuant).
#'
#' @name split-methods
#' @rdname split
#'
#' @author Iago Mosqueira (WMR).
#' @keywords methods
#' @md
#' @examples
#' # FROM FLQuant to FLQuants
#' flq <- rlnorm(20, FLQuant(seq(0.1, 0.8, length=10)), 0.2)
#' split(flq, c(rep(1, 5), rep(2,15)))

setMethod("split", signature(x="FLQuant", f="vector"),
  function(x, f) {
  FLQuants(lapply(setNames(nm=unique(f)),
    function(i) {
      iter(x, f == i)
    }
  ))
})

setMethod("split", signature(x="FLQuant", f="missing"),
  function(x) {
  
    f <- seq(dims(x)$iter)
    FLQuants(lapply(setNames(nm=unique(f)),
      function(i) {
        iter(x, f == i)
      }
    ))
})



# }}}

# combine {{{
setMethod('combine', signature(x='FLQuant', y='FLQuant'),
  function(x, y, ...) {

    args <- c(list(x, y), list(...))

    # GET dim(s) & dimnames
    ds <- lapply(args, dim)
    dns <- lapply(args, dimnames)
    
    # CHECK objects share dim[1:5]
    if(length(unique(lapply(ds, "[", 1:5))) > 1)
      stop("Object dimensions [1:5] must match")

    # WARN if dimnames[1:5] differ
    if(length(unique(lapply(dns, "[", 1:5))) > 1)
      warning("dimnames among objects differ, will use those of x")

    # SETUP iter dimnames
    itns <- unname(unlist(lapply(dns, "[", 6)))
    
    # IF dimnames clash, then rename
    if(length(unique(itns)) < length(itns))
      itns <- ac(seq(1, length(itns)))

    # CREATE output object, units as in x
    res <- FLQuant(NA, dimnames=c(dimnames(x)[1:5], list(iter=itns)),
      units=units(x))

    # GET iter limits
    ite <- cumsum(unlist(lapply(ds, "[", 6)))
    its <- ite - unlist(lapply(ds, "[", 6)) + 1

    # ASSIGN by iter
    for(i in seq(length(args)))
      res[,,,,,seq(its[i], ite[i])] <- args[[i]]

    return(res)
  }
) # }}}

# group {{{

#' @rdname group
#' @examples
#' # Add catch-at-age along two age groups, 'juv'eniles and 'adu'lts
#' group(catch.n(ple4), sum, age=c('juv', 'juv', rep('adu', 8)))
#' # An expression can use based on dimnames
#' group(catch.n(ple4), sum, age=age < 3)
#' # Mean by lustrum, by using 'year - year %% 5'
#' group(catch.n(ple4), mean, year = year - year %% 5)

setMethod("group", signature(x="FLQuant", FUN="function"),
  function(x, FUN=sum, ...) {

  # EXTRACT unevaluated args
  args <- match.call(expand.dots = FALSE)$...

  # FIND indices in args
  dm <- na.omit(match(names(args), names(x), nomatch=NA))
  ndm <- names(x)[dm]

  # CHECK dimension aggregating index provided
  if(length(dm) == 0)
    stop("No aggregating index provided")

  # CHECK dimension aggregating index is only 1
  if(length(dm) > 1)
    stop("group can only work over a single dimension, check argument names")

  # EXTRACT indices & FUN args
  indices <- args[[ndm]]
  args[[ndm]] <- NULL
 
  # PARSE
  values <- lapply(dimnames(x)[dm], as.numeric)
  indices <- eval(indices, values)

  # APPLY FUN over indices subset, use extra args
  res <- lapply(unique(indices), function(i) {
    
    # TRIM over dm
    trims <- setNames(list(dimnames(x)[[ndm]][indices %in% i]), nm=ndm)
    z <- do.call(trim, c(list(x=x), trims))
    
    # APPLY FUN on trimmed subset
    y <- do.call(apply, c(list(X=z, MARGIN=seq(1, 6)[-dm], FUN=FUN), args))
    dimnames(y)[[dm]] <- i

    return(y)
  })

  out <- join(FLQuants(res))

  return(out)
}
)

# }}}

# ifelse {{{
setMethod("ifelse", signature(test="FLQuant", yes="ANY", no="ANY"),
  function(test, yes, no) {
    
    uts <- units(test)
    dmns <- dimnames(test)

    test <- as(test, 'logical')
    yes <- c(yes)
    no <- c(no)

    res <- callNextMethod()

    return(FLQuant(res, dimnames=dmns, units=uts))
  }
) 

setMethod("ifelse", signature(test="ANY", yes="ANY", no="FLQuant"),
  function(test, yes, no) {
    
    uts <- units(no)
    dmns <- dimnames(no)

    test <- as(test, 'logical')
    yes <- c(yes)
    no <- c(no)

    res <- callNextMethod()

    return(FLQuant(res, dimnames=dmns, units=uts))
  }
)

setMethod("ifelse", signature(test="ANY", yes="FLQuant", no="ANY"),
  function(test, yes, no) {
    
    uts <- units(yes)
    dmns <- dimnames(yes)

    test <- as(test, 'logical')
    yes <- c(yes)
    no <- c(no)

    res <- callNextMethod()

    return(FLQuant(res, dimnames=dmns, units=uts))
  }
)
# }}}

# tail {{{

#' Returns the first and last parts of an FLQuant.
#'
#' Standard tail and head methods can be applied along any dimension of an
#' FLQuant object.
#'
#' @param x The object to extract from, FLQuant.
#' @param n The number of elements to extract, numeric.
#' @param dim Dimension to extract from, defaults to 2, 'year'.
#'
#' @return An FLQuant with the extracted elements.
#'
#' @rdname tail
#'
#' @author Iago Mosqueira (WMR)
#' @seealso [base::tail]
#' @keywords methods
#' @md
#' @examples
#' x <- FLQuant(1:10)
#' 
#' # Extract the last 3 years
#' tail(x, 3)
#' 
#' # Extract all but the first 3 years
#' tail(x, -3)

setMethod("tail", signature(x="FLQuant"),
  function(x, n=1, dim=2, ...) {

    # dim of length 1
    if(length(dim) > 1)
      stop("tail(FLQuant) can only apply to a single dim(ension)")

    # character dim
    if(is(dim, 'character'))
      dim <- which(dim == names(x))

    # named list of dimension vectors
    idx <- lapply(as.list(dim(x)), seq)
    names(idx) <- c('i','j','k','l','m','n')

    # tail dimension set by dim
    idx[[dim]] <- tail(idx[[dim]], n=n)

    # apply '['
    return(do.call('[', c(list(x=x), idx)))
  }
) # }}}

# head {{{

#' @rdname tail
#' @examples
#'
#' # Extract the first 3 years
#' head(x, 3)
#'
#' # Extract all but the last 3 years
#' head(x, -3)

setMethod("head", signature(x="FLQuant"),
  function(x, n=1, dim=2, ...) {

    # dim of length 1
    if(length(dim) > 1)
      stop("head(FLQuant) can only apply to a single dim(ension)")

    # character dim
    if(is(dim, 'character'))
      dim <- which(dim == names(x))

    # named list of dimension vectors
    idx <- lapply(as.list(dim(x)), seq)
    names(idx) <- c('i','j','k','l','m','n')

    # tail dimension set by dim
    idx[[dim]] <- head(idx[[dim]], n=n)

    # apply '['
    return(do.call('[', c(list(x=x), idx)))
  }
) # }}}

# tS, tS<- {{{
setMethod("tS", signature(object="FLQuant", step="numeric"),
  function(object, step) {

  # dims
  do <- dim(object)[c(2,4)]

  # find pout season and year
  season <- step %% do[2]
  year <- step %/% do[2] + 1

  # correct for those in season 4
  idx <- (step %% do[2]) == 0
  year[idx] <- (step %/% do[2]) [idx]
  season[idx] <- do[2]

  # Are n elements contiguous in years or seasons ?
  if(length(unique(year)) > 1 & length(unique(season)) > 1)
    stop("requested time steps do not generate a consistent object")

  return(object[, unique(year), ,unique(season),,])
  }
)

setMethod("tS<-", signature(object="FLQuant", step="numeric", value="vector"),
  function(object, step, value) {

  # dims
  do <- dim(object)[c(2,4)]

  # find pout season and year
  season <- step %% do[2]
  year <- step %/% do[2] + 1

  # correct for those in season 4
  idx <- (step %% do[2]) == 0
  year[idx] <- step %/% do[2]
  season[idx] <- do[2]

  object[, unique(year), ,unique(season),,]  <- value

  return(object)
  }
)

# }}}

# tsp {{{
setMethod("tsp", signature(x="FLQuant"),
  function(x) {
    dms <- dimnames(x)
    return(c(as.numeric(dms$year[c(1, length(dms$year))]), length(dms$season)))
  }
) # }}}

# $ {{{
#' @rdname Extract
#' @aliases $,FLQuant-method
setMethod("$", signature(x="FLQuant"),           
  function(x, name) {
    return(x[name,])
  }
) # }}}

# catch.n {{{

#' catch.n calculation method
#'
#' Calculate catch.n (catch-at-age/length) from abundances, F and M using the catch equation
#' 
#' The catch-at-age/length, commonly found in the catch.n slot of an
#' `FLStock` object, can be simply calculated from abundances-at-age/length,
#' and natural and fishing mortalities-at-age/length by applying the catch
#' equation
#' \deqn{C = N \cdot F \frac{F}{M+F} \cdot (1 - {\rm e}^(-M-F))}{C = N *F/(M+F) * (1-exp(-M-F))}
#'
#' @aliases catch.n,FLQuant-method
#' @docType methods
#' @author The FLR Team
#' @seealso \linkS4class{FLStock}
#' @keywords methods
#' @examples
#' data(ple4)
#' res <- catch.n(stock.n(ple4), harvest(ple4), m(ple4))
#' catch.n(ple4) / res

setMethod("catch.n", signature(object="FLQuant"),
  function(object, harvest, m) {
    object * (harvest / (harvest + m)) * (1 - exp(-(harvest + m)))
  }) # }}}

# harvest {{{

baranovCatch <- function(n, m, f) {
  return(n * (f / (m + f)) * (1 - exp(-(m + f))))
}

solveBaranov <- function(n, m, c) {

  if(sum(is.na(c(n, m, c))) > 0)
    return(rep(as.numeric(NA), length(n)))

  foo <- function(logf, n, m, c) {
    newc <- baranovCatch(n, m, exp(logf))
    return(sum(c) - sum(newc))
  }

  f <- n

  for(i in seq(length(n)))
    f[i] <- tryCatch(exp(uniroot(foo, interval=log(c(1e-8, 4)),
      extendInt = "yes", n=n[i], m=m[i], c=c[i])$root),
      error = function(e) return(as.numeric(NA)))

  return(f)
}

setMethod("harvest", signature(object="FLQuant", catch="FLQuant"),
  function(object, catch, m, recompute=FALSE) {

    # EMPTY harvest FLQ
    itr <- max(dim(object)[6], dim(catch)[6], dim(m)[6])
    har <- propagate(m, itr)
    har[] <- NA

    object <- propagate(object, itr)
    catch <- propagate(catch, itr)
    m <- propagate(m, itr)

    # dims, ages - last 2, years - last 1
    dm <- dim(har)
    aa <- seq(1, dm[1] - 2)
    yy <- seq(1, dm[2] - 1)
 
    # Baranov functions
    # --- SINGLE year, LOOP over all dims[-year]
    if(dm[2] == 1 | recompute) {
      yy <- seq(1, dm[2])
      for(y in yy) {
        sn <- c(object[, y])
        sc <- c(catch[, y])
        sm <- c(m[, y])
        out <- sn
        har[, y] <- solveBaranov(sn, sm, sc)
      }
    # --- YEARLY
    } else if(dm[4] == 1) {

      # a-1 ages and y-1 years
      n0 <- object[aa, yy]
      n1 <- object[aa + 1, yy + 1]
      har[aa, yy] <- -(log(n1) - log(n0)) - m[aa, yy]

      # LOOP over ages for last year, by unit, area & iter
      for(i in seq(dm[1])) {
        for(k in seq(dm[3])) {
          for(mm in seq(dm[5])) {
            for(it in seq(dm[6])) {
              n <- c(object[i, dm[2], k,, mm, it])
              if(all(is.na(n)))
                har[i, dm[2], k,, mm, it] <- n
              else {
                har[i, dm[2], k,, mm, it] <- solveBaranov(n,
                  m=c(m[i, dm[2], k,, mm, it]), c=c(catch[i, dm[2], k,, mm, it]))
              }
            }
          }
        }
      }

      # LOOP over years for last 2 ages, by unit & area
      for(j in seq(dm[2])) {
        for(k in seq(dm[3])) {
          for(mm in seq(dm[5])) {
            for(it in seq(dm[6])) {
              for(i in c(dm[1]-1, dm[1])) {
                n <- c(object[i, j, k,, mm, it])
                if(all(is.na(n)))
                  har[i, j, k,, mm, it] <- n
                else {
                har[i, j, k,, mm, it] <- solveBaranov(n,
                  m=c(m[i, j, k,, mm, it]), c=c(catch[i, j, k,, mm, it]))
                }
              }
            }
          }
        }
      }

    # SEASONS
    } else {

      # seasons s=1:n-1, log(s+1/s)
      ss <- seq(1, dm[4] - 1)
      n0 <- object[,,,ss]
      n1 <- object[,,,ss + 1]
      har[,,,ss] <- log(n0/n1) - m[,,,ss]

      # last season
      n0 <- object[aa,yy,,dm[4]]
      n1 <- object[aa + 1,yy + 1,,1]
      har[aa,yy,,dm[4]] <- log(n0/n1) - m[aa,yy,,dm[4]]

      # DIV/0 to 0
      har[har < 0] <- 0

      # Q & D, 1999 (Page 325)
      # C_y,a = N_y,a * (F_y,a / (F_y,a + M_y,a)) * (1 - exp((-F_y,a - M_y,a) * tau))
      # NOTE: IGNORING tau (no. years represented by pgroup)
      
      # LOOP over units
      for(u in seq(dm[3])) {
        # LOOP over years and last 2 ages
        for(y in seq(dm[2]-1)) {
          for(a in c(dm[1]-1, dm[1])) {
            for(it in seq(dm[6])) {
              n <- c(object[a,y,u,dm[4],,it])
              if(all(is.na(n))) {
                har[a,y,u,dm[4],,it] <- n
              } else {
              har[a, y, u, dm[4],, it] <- solveBaranov(n,
                m=c(m[a, y, u, dm[4],, it]), c=c(catch[a, y, u, dm[4],, it]))
            }
            }
          }
        }
        # LOOP over ages for last year and season
        for(a in seq(dm[1])) {
          for(it in seq(dm[6])) {
            n <- c(object[a,dm[2],u,dm[4],,it])
            if(all(is.na(n)))
              har[a,dm[2],u,dm[4],,it] <- n
            else {
              har[a, dm[2], u, dm[4],, it] <- solveBaranov(n,
                m=c(m[a,dm[2],u,dm[4],, it]), c=c(catch[a, y, u, dm[4],, it]))
            }
          }
        }
      }
    }

    # har[is.na(har)] <- 0
    har[har < 0] <- 0
    units(har) <- "f"
  
    har[object == 0] <- 0
    har[catch == 0] <- 0

    return(har)
  }
) # }}}

# knit_print.FLQuant{{{
knit_print.FLQuant <- function(object, options, cols=5, inline=FALSE) {

    # dims
    do <- dim(object)

    # More year than cols * 2
    if(do[2] > (cols * 2)) {
      scols <- 1:cols
      ecols <- dimnames(object)[[2]][(do[2]-cols+1):do[2]]

      x1 <- object[,scols]
      x2 <- object[,ecols]

      if(dim(object)[6] != 1)
        cat("iters: ", dim(object)[6],"\n\n")

      if(dim(object)[6] > 1) {
        x1v1 <- apply(x1@.Data, 1:5, median, na.rm=TRUE)
        x1v2 <- apply(x1@.Data, 1:5, mad, na.rm=TRUE)   
        x1v3 <- paste(format(x1v1,digits=5),"(", format(x1v2, digits=3), ")", sep="")
        x2v1 <- apply(x1@.Data, 1:5, median, na.rm=TRUE)
        x2v2 <- apply(x1@.Data, 1:5, mad, na.rm=TRUE)   
        x2v3 <- paste(format(x1v1,digits=5),"(", format(x1v2, digits=3), ")", sep="")
      } else {
        x1v3 <- format(x1,digits=5)
        x2v3 <- format(x2,digits=5)
      }
    
      print(array(x1v3, dim=dim(x1)[1:5], dimnames=dimnames(x1)[1:5]), quote=FALSE)
      cat("      [ ... ", do[2] - cols*2,"years]\n\n")
      print(array(x2v3, dim=dim(x2)[1:2], dimnames=dimnames(x2)[1:2]), quote=FALSE)
    } else {
      if(inline)
        print(c(object))
      else
        print(object)
    }
} # }}}

# filldimnames       {{{
filldimnames <- function(dnames, dim=rep(1,6), iter=1) {
# check only one name for quant in input
if(length(names(dnames)[!names(dnames)%in%c("year","unit","season","area","iter")]) > 1)
stop("more than one vector of names given for the first dimension")
# generate standard names for given dimensions
xnames <- dimnames(FLQuant(dim=dim, iter=iter))
for(i in 1:length(dnames)) {
# non-quant names
if(any(names(dnames)[i]==c("year","unit","season","area","iter")))
xnames[[names(dnames)[i]]] <- dnames[[i]]
# quant
else {
xnames[[1]] <- dnames[[i]]
names(xnames)[1] <- names(dnames)[i]
}
}
return(xnames)
} # }}}

# append {{{

#' Append objects along the year dimension
#'
#' Method to append objects along the *year* dimensions, by extending, combining
#' and substituting sections of them.
#'
#' FLR objects are commonly manipulated along the year dimension, and the append
#' method offers a simple interface for substituting parts of an object with
#' another, or combine them into one, extending them when necessary.
#' The object to be included or added to the first will be placed as defined by
#' the *year* dimnames, unless the *after* input argument specifies otherwise.
#'
#' @param x the object to which the values are to be appended to.
#' @param values to be included in the modified object.
#' @param after a year dimname after with the values are to be appended.
#'
#' @return An object of the same class as x with values appended.
#'
#' @name append-FLCore
#' @rdname append-methods
#' @author The FLR Team
#' @seealso [base::append]
#' @keywords methods
#' @md
#' @examples
#' # append(FLQuant, FLQuant)
#' fq1 <- FLQuant(1, dimnames=list(age=1:3, year=2000:2010))
#' fq2 <- FLQuant(2, dimnames=list(age=1:3, year=2011:2012))
#' fq3 <- FLQuant(2, dimnames=list(age=1:3, year=2014:2016))
#' 
#' # Appends by dimnames$year
#' append(fq1, fq2)
#' # Appends by dimnames$year with gap (2011:2013)
#' append(fq1, fq3)
#' # Appends inside x
#' append(fq1, fq2, after=2009)
#' # Appends after end of x
#' append(fq1, fq2, after=2013)
#'

setMethod("append", signature(x="FLQuant", values="FLQuant"),
  function(x, values, after=dims(values)$minyear-1) {

    # CHECK iters & PROPAGATE x if needed
    if(dim(x)[6] == 1 & dim(values)[6] > 1)
      x <- propagate(x, iter=dim(values)[6])
    
    # EXTEND x if needed
    if(after + dims(values)$year > dims(x)$maxyear)
      x <- window(x, end=after + dims(values)$year)

    x[, ac(seq(after + 1, length=dims(values)$year))] <- values

    return(x)
  }
)

setMethod("append", signature(x="FLQuant", values="numeric"),
  function(x, values, after=dims(x)$maxyear + 1) {

    # CREATE FLQuant from one year of x with last + one year
    y <- expand(x[, 1], year=after)

    # PLACE values
    y[] <- values

    append(x, y)
  }
)
# }}}

# residuals {{{

#' residuals
#' @name residuals-FLQuant
#' @rdname residuals-FLQuant
#'
#' @examples
#' data(ple4)
#' fit <- rlnorm(1, log(catch(ple4)), 0.1)
#' residuals(catch(ple4), fit)
#' residuals(catch(ple4), fit, type="student")
#' rraw(catch(ple4), fit)
#' rlogstandard(catch(ple4), fit)
#' rstandard(catch(ple4), fit)
#' rstudent(catch(ple4), fit)

setMethod("residuals", signature(object="FLQuant"),
  function(object, fit, type="log", ...) {
    
    # MAP glm residuals names with methods
    method <- switch(type[1],
      log="rlog",
      logstandard="rlogstandard",
      standard="rstandard",
      pearson="rstandard",
      student="rstudent",
      raw="rraw")

    do.call(method, list(object, fit, ...))
  }
)

# rraw(FLQ, FLQ)
setGeneric("rraw", function(obs, fit, ...)
		standardGeneric("rraw"))
setMethod("rraw", signature(obs="FLQuant", fit="FLQuant"),
  function(obs, fit) {
    res <- obs - fit
    units(res) <- units(obs)
    return(res)
  }
)

# rlog(FLQ, FLQ)
setGeneric("rlog", function(obs, fit, ...)
		standardGeneric("rlog"))
setMethod("rlog", signature(obs="FLQuant", fit="FLQuant"),
  function(obs, fit) {
    res <- log(obs) - log(fit)
    units(res) <- units(obs)
    return(res)
  }
)

# rlogstandard(FLQ, FLQ)
setGeneric("rlogstandard", function(obs, fit, ...)
		standardGeneric("rlogstandard"))
setMethod("rlogstandard", signature(obs="FLQuant", fit="FLQuant"),
  function(obs, fit, sdlog=sqrt(yearVars(flq))) {
    
    flq <- Reduce("-", lapply(intersect(obs, fit), log))
    
    # DEAL with Inf
    is.na(flq) <- !is.finite(flq)
	  
    res <- flq %/% sdlog
 
    units(res) <- ""

    return(res)
  }
)

# rstandard(FLQ, FLQ)
setGeneric("rstandard", useAsDefault=stats::rstandard)
setMethod("rstandard", signature(model="FLQuant"),
  function(model, fit, sd=c(sqrt(yearVars(flq)))) {

    if(!is(fit, "FLQuant"))
      stop("Both objects must be of class 'FLQuant'")
	
    flq <- Reduce("/", intersect(model, fit))
    
    # DEAL with Inf
    is.na(flq) <- !is.finite(flq)
	  
    res <- flq / sd
    
    units(res) <- ""

    return(res)
  }
)
# rstudent(FLQ, FLQ)
# https://stackoverflow.com/questions/45485144/how-to-compute-studentized-residuals-in-python
setGeneric("rstudent", useAsDefault=stats::rstudent)
setMethod("rstudent", signature(model="FLQuant"),
  function(model, fit, internal=TRUE) {

    if(!is(fit, "FLQuant"))
      stop("Both fit and observation must be of class 'FLQuant'")
	
    mmodel <- mean(model)
    mfit <- mean(fit)
    nn <- length(model)

    diffmsqr <- (model - mmodel)^2
    beta1 <- (model - mmodel) * (fit - mfit)
    beta0 <- mfit - beta1 * model
    fhat <- beta0 + beta1 + model
    residuals <- fit - fhat
    
    hii <- (model - mmodel) * 2 / diffmsqr + (1 / nn)
    vare <- sqrt(sum((fit - fhat) ^ 2) / (nn -2))
    se <- vare * ((1 - hii) ^ 0.5)
    
    if(internal)
      res <- residuals / se
    else
      res <- (residuals / se) * sqrt((nn-2-1) / (nn-2-(residuals/se)^2))

    units(res) <- ""

    return(res)
  }
)

# rpress(FLQ, FLQ)
# rcholesky(FLQ, FLQ)
# https://www.tandfonline.com/doi/abs/10.1198/016214504000000403

# }}}

# fwd(FLQuant) {{{

setMethod("fwd", signature(object="FLQuant", fishery="missing", control="missing"),
  function(object, rec=NA) {

    # TODO ADD z, m, f

    # ADD plusgroup
    dms <- dims(object)
    object[dms$max,] <- quantSums(object[seq(dms$max - 1, dms$max),])
    
    # MOVE other ages
    object[seq(dms$min + 1, dms$max - 1), ] <- object[seq(dms$min, dms$max - 2), ]

    # SET rec to rec
    object[1, ] <- rec

    # CHANGE dimnames
    dimnames(object)$year <- as.numeric(dimnames(object)$year) + 1

    return(object)
  })
# }}}

# iav {{{

#' Compute the inter-annual variability of a time series
#'
#' The inter-annual variability of a time series stored in an FLQuant object,
#' is computed as \eqn{|x_y - x_{y-1}) / x_{y-1}|}. The resulting object will
#' be one year shorter than the input. The first year will be missing as values
#' are assigned to the final year of each pair.
#'
#' @return An object of the same class as object.
#'
#' @name iav
#' @rdname iav
#' @author The FLR Team
#' @keywords methods
#' @md
#' @examples
#' data(ple4)
#' # Compute inter-annual variability in catch
#' iav(catch(ple4))

iav <- function(object) {

  # GET object dims
  dm <- dim(object)

  # CALCULATE annual variation
  res <- abs(object[, -dm[2]] - object[, -1]) / object[, -dm[2]]

  # ASSIGN values to last year
  dimnames(res) <- list(year=dimnames(object)$year[-1])

  return(res)

} # }}}

# aac {{{

#' @rdname aac
#' @examples
#' data(ple4)
#' acc(catch.n(ple4), ages=2:9)

setMethod("acc", signature(object="FLQuant"),
  function(object, ages=seq(dim(object)[1])) {

  res <- apply(object[ages,], c(2, 6), function(i) {
    -coefficients(lm(log(i[i > 0]) ~ as.numeric(dimnames(i)$age)[i > 0]))[2]
  })

  units(res) <- 'z'

  return(res)

})
# }}}

# merge(FLQuant) {{{

#' @examples
#' # merge by year
#' a <- FLQuant(34, dimnames=list(year=2000))
#' b <- FLQuant(32, dimnames=list(year=2001))
#' merge(a, b)
#' # merge by quant
#' a <- FLQuant(54, dimnames=list(age=1))
#' b <- FLQuant(29, dimnames=list(age=2))
#' merge(a, b)

setMethod("merge", signature(x="FLQuant", y="FLQuant"),
  function(x, y) {

    # EXTRACT dimnames
    dnx <- dimnames(x)
    dny <- dimnames(y)

    # FIND different dimension
    dnd <- unlist(Map(function(x, y) identical(x,y), x=dnx, y=dny))
    pos <- which(!dnd)

    # CHECK only one dim to be merged
    if(length(pos) > 1)
      stop("Only one dimension can be different between objects")

    # SELECT merging function
    fun <- switch(pos,
      '1'=qbind,
      '2'=ybind,
      '3'=ubind,
      '4'=sbind,
      '5'=abind,
      '6'=ibind)

    out <- do.call(fun, list(x, y))

    return(out)
  }
)
# }}}

# rho {{{
rho <- function(x) {
  FLPar(rho=c(apply(x, 6, function(i)
    c(acf(c(i), plot=FALSE, na.action=na.pass)[1][[1]]))), units="")
} # }}}

# rwalk {{{

#' Generate a random walk time series from a starting point
#'
#' The last year of an `FLQuant` object is used as atrating point to generate
#' a time series following a random walk with drift:
#' \deqn{z_t = z_{t-1} + \epsilon_t + \delta_t, t=1,2,...}{z[t] = z[t-1] + e[t] + d[t], t=1,2,...}
#' where \eqn{\epsilon}{e} is \eqn{\mathcal{N}(0, \sigma)}{N(0, sd)}
#'
#' The length of the series is set by argument *end*. This is taken as a number of years, if its value is smaller than the final 'year' of 'x0', or as a final year if larger or of class 'character'.
#' @param x0 The initial state of the random walk, 'FLQuant'.
#' @param end The number of years or the final year of the series. numeric.
#' @param sd The standard deviation of the random walk, numeric.
#' @param delta The drift of the random walk.
#'
#' @return An 'FLQuant' object.
#'
#' @author Iago Mosqueira, WMR (2023)
#' @seealso [FLQuant-class] [rnorm]
#' @keywords classes
#' @examples
#' data(ple4)
#' # Generate random walk recruitmrnt with positive drift
#' rwalk(rec(ple4), end=5, sd=0.08, delta=0.05)
#' # Use append() to add the new values at the end
#' append(rec(ple4), rwalk(rec(ple4), end=10, sd=0.04, delta=0))
#' # Use end as number of years
#' rwalk(rec(ple4), end=5)
#' # or as final year
#' rwalk(rec(ple4), end=2020)

rwalk <- function(x0, end=1, sd=0.05, delta=0) {

  # SUBSET last year
  x0 <- x0[, dim(x0)[2]]

  # PARSE end argument
  if(is.numeric(end)) {
    # AS number of extra years
    if(end > dims(x0)$maxyear)
      t <- seq(dims(x0)$maxyear + 1, end)
    # AS final year
    else
      t <- seq(dims(x0)$maxyear + 1, length=end)
  } else if(is.character(end)) {
      t <- seq(dims(x0)$maxyear + 1, as.numeric(end))
  }

  # APPLY random walk by iter
  res <- mapply(function(i) {
    z <- cumsum(rnorm(n=length(t), mean=0, sd=sd))
    t <- seq(t)
    x <- exp(1 * t * delta + z)
    return(i * x)
  }, c(x0))

  return(FLQuant(c(res), dimnames=list(year=t, iter=dimnames(x0)$iter),
    units=units(x0), quant="age"))
}
# }}}

# leslie {{{
.leslie <- function(object, fec) {

  if (length(object) != length(fec))
    stop("Vectors differ in length")
      
  L <- matrix(0, nrow=length(object), ncol=length(object))
  
  diag(L[-1, -length(object)]) <- object[-length(object)]
  
  L[1, ] <- fec
  
  # plusgroup
  L[length(object), length(object)] <- L[length(object), length(object) - 1]

  return(L)
}

#' @examples
#' survivors <- cumprod(rep(0.5, 10))
#' fec <- c(0, rep(1, 9))
#' leslie(survivors, fec)

setMethod("leslie", signature(object="numeric", fec="numeric"),  
  function(object, fec) {
    .leslie(object, fec)
})

#' @examples
#' data(ple4)
#' leslie(stock.n(ple4), mat(ple4))

setMethod("leslie", signature(object="FLQuant", fec="FLQuant"),
  function(object, fec) {

    # FIND largest iters
    its <- max(dim(object)[6], dim(fec)[6])
    yrs <- dim(fec)[2]

    # SET dimnames
    dnms <- dimnames(object)

    # BUILD matrix: age x age x iters
    L <- array(0, c(dim(fec)[1], dim(fec)[1], yrs, its),
      dimnames=c(dnms[1], dnms[1], dnms[2], iter=seq(its)))

    for (i in seq(its))
      for(y in seq(yrs))
        L[,, y, i] <- .leslie(iter(object, i)[,y,drop=TRUE],
          iter(fec, i)[,y,drop=TRUE])
    
    return(drop(L))
  }
)
# }}}

# which_iter {{{

#' @title which_iter
#' @description Returns the positions along the 'iter' dimension that match a logical estatement
#' @param x An input object, class 'FLQuant'
#' @param rule A logical 'rule', either as a 'character' or a logical 'FLQuant'
#' @return As vector of positions along the 'iter' (6th) dimension
#' @details DETAILS
#' @examples
#' # Creates an example object
#' x <- rnorm(50, FLQuant(runif(5, 10, 20)), 20)
#' # Looks for iters with a large value
#' which_iter(x, x > 60)
#' # rule as a character vector
#' which_iter(x, '> 60')
#' # rule has use & and |
#' which_iter(x, '> 50 & < 60')
#' which_iter(x, '< -40 | > 40')
#' @rdname which_iter

which_iter  <- function(x, rule) {
 
  # IF x is character
  if(is(rule, 'character')) {
    
    # PARSE & and |, add x
    rule <- paste('x', gsub('([&|])', '\\1 x', rule))
    
    # EVAL on x
    rule <- eval(parse(text=rule))
  }

  # APPLY rule along iter and find positions of matches
  return(unname(which(apply(rule > 0, 6, sum) > 0)))
}

# }}}
