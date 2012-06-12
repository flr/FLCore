# FLQuant.R - FLQuant class and methods
# FLCore/R/FLQuant.R

# Copyright 2003-2007 FLR Team. Distributed under the GPL 2 or later
# Maintainer: Iago Mosqueira, JRC
# $Id$


# FLQuant(missing){{{
# FLQuant  <- FLQuant()
setMethod("FLQuant", signature(object="missing"),
function(object, dim=rep(1,6), dimnames="missing", quant=NULL, units="NA", iter=1) {

# no dim or dimnames
if (missing(dim) && missing(dimnames)) {
dim <- c(1,1,1,1,1,iter)
dimnames <- list(quant='all', year=1, unit='unique', season='all', area='unique',
iter=1:dim[6])
}

# dim missing
else if (missing(dim)) {
dimnames <- filldimnames(dimnames, iter=iter)
dim <- as.numeric(sapply(dimnames, length))
}

# dimnames missing
else if (missing(dimnames)) {
dim <- c(dim, rep(1,6))[1:6]
if(!missing(iter))
dim[6] <- iter
dimnames <- list(
quant=if(dim[1]==1){"all"}else{1:dim[1]},
year=1:dim[2],
unit=if(dim[3]==1){"unique"}else{1:dim[3]},
season=if(dim[4]==1){"all"}else{1:dim[4]},
area=if(dim[5]==1){"unique"}else{1:dim[5]},
iter=1:dim[6])
}
# both
else {
dim <- c(dim, rep(1,6))[1:6]
if(!missing(iter))
dim[6] <- iter
dimnames <- filldimnames(dimnames, dim=dim, iter=iter)
}
flq <- new("FLQuant", array(as.numeric(NA), dim=dim, dimnames=dimnames), units=units)

if (!is.null(quant))
quant(flq) <- quant

return(flq)
}
)# }}}

# FLQuant(vector){{{
# FLQuant  <- FLQuant(vector)
setMethod("FLQuant", signature(object="vector"),
function(object, dim=rep(1,6), dimnames="missing", quant=NULL, units="NA", iter=1,
    fill.iter=TRUE)
{
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
if(!missing(iter))
dim[6] <- iter
dimnames <- list(
quant=if(dim[1]==1){"all"}else{1:dim[1]},
year=1:dim[2],
unit=if(dim[3]==1){"unique"}else{1:dim[3]},
season=if(dim[4]==1){"all"}else{1:dim[4]},
area=if(dim[5]==1){"unique"}else{1:dim[5]},
iter=1:dim[6])
}
# both
else {
dim <- c(dim, rep(1,6))[1:6]
if(!missing(iter))
dim[6] <- iter
dimnames <- filldimnames(dimnames, dim=dim, iter=iter)
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
)# }}}

# FLQuant(array){{{
# FLQuant <- FLQuant(array)
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
    # TODO TEST
flq <- new("FLQuant", array(as.double(object), dim=dim, dimnames=dimnames),
      units=units)

# Set extra iters to NA, unless array has 6 dimensions
    if(dims(flq)$iter > 1 && !fill.iter)
    flq[,,,,,2:dims(flq)$iter] <- as.numeric(NA)

if (!is.null(quant))
quant(flq) <- quant

return(flq)
}
)# }}}

# FLQuant(matrix){{{
# FLQuant <- FLQuant(matrix)
setMethod("FLQuant", signature(object="matrix"),
function(object, dim="missing", dimnames="missing", ...) {

if(missing(dim))
dim <- c(nrow(object), ncol(object), rep(1,5))[1:6]
if(!missing(dimnames))
return(FLQuant(array(object, dim=dim, dimnames=filldimnames(dimnames, dim=dim)), ...))
if(!is.null(dimnames(object)) && missing(dimnames))
return(FLQuant(array(object, dim=dim), dimnames=filldimnames(dimnames(object),
dim=dim), ...))
return(FLQuant(array(object, dim=dim), ...))
}
)# }}}

# FLQuant(FLQuant){{{
# FLQuant <- FLQuant(FLQuant)
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

## as.FLQuant      {{{
setGeneric("as.FLQuant", function(x, ...)
standardGeneric("as.FLQuant"))# }}}

# as.FLQuant(array){{{
setMethod("as.FLQuant", signature(x="array"),
function(x, ...) {
return(FLQuant(x, ...))
}
)# }}}

# as.FLQuant(matrix){{{
setMethod("as.FLQuant", signature(x="matrix"),
function(x, ...) {
return(FLQuant(x, ...))
}
)# }}}

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
)# }}}

# coerce  {{{
setAs("data.frame", "FLQuant",
  function(from)
  {
    # get data.frame names and compare
names(from) <- tolower(names(from))
    validnames <-c("year","unit","season","area","iter","data")

indices <- match(validnames, names(from))
  indices <- indices[!is.na(indices)]

    # get quant
    qname <- names(from)
qname[indices] <- NA
qname <- qname[!is.na(qname)]

    if (length(qname) > 1)
stop("too many columns in data.frame")
    if(length(qname) == 0)
      qname <- "quant"
    
    # check and fill up missing dimensions
    n <- dim(from)[1]
    # TODO conversion to/from factor messes up dimnames order
    em <- data.frame(quant=rep('all', n), year=rep(1,n), unit=rep('unique',n),
      season=rep('all',n), area=rep('unique',n), iter=rep(1,n), stringsAsFactors=FALSE)
    names(em)[names(em)=="quant"] <- qname
    from[,!names(from)%in%'data'] <- 
    as.data.frame(as.matrix(from[,!names(from)%in%'data']),
      stringsAsFactors=FALSE)
    em[names(from)] <- from

    # create array
    flq <- tapply(em[,"data"], list(em[,qname], em[,"year"], em[,"unit"], em[,"season"],
      em[,"area"], em[,"iter"]), sum)

    # fix dimnames names
    names(dimnames(flq)) <- c(qname, 'year', 'unit', 'season', 'area', 'iter')
    
    # create FLQuant
    flq <- FLQuant(flq)
    
    # units
    if(!is.null(attr(from, 'units')))
      units(flq) <- attr(from, 'units')

    # fill up missing years
    if(length(dimnames(flq)[['year']]) != length(as.character(seq(dims(flq)$minyear,
      dims(flq)$maxyear))))
    {
      res <- FLQuant(dimnames=c(dimnames(flq)[1], list(year=seq(dims(flq)$minyear,
        dims(flq)$maxyear)), dimnames(flq)[3:6]))
      res[,dimnames(flq)[['year']],] <- flq
      flq <- res
    }
return(flq)
  }
) # }}}

# as.FLQuant(data.frame){{{
setMethod("as.FLQuant", signature(x="data.frame"),
function(x, ...)
  {
    # get data.frame names and compare
names(x) <- tolower(names(x))
    validnames <-c("year","unit","season","area","iter","data")

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
    
    # check and fill up missing dimensions
    n <- dim(x)[1]
    # TODO conversion to/x factor messes up dimnames order
    em <- data.frame(quant=rep('all', n), year=rep(1,n), unit=rep('unique',n),
      season=rep('all',n), area=rep('unique',n), iter=rep(1,n), stringsAsFactors=FALSE)
    names(em)[names(em)=="quant"] <- qname
    
    # Not sure why the call to as.matrix was here, but is messes up names from numbers
    #  sometimes, as 1 truns to " 1" is there is 10. Trying now without it. IM, 2/7/09
    #
    #x[,!names(x)%in%'data'] <- as.data.frame(as.matrix(x[,!names(x)%in%'data']),
    #  stringsAsFactors=FALSE)
    x[,!names(x)%in%'data'] <- as.data.frame(x[,!names(x)%in%'data'],
      stringsAsFactors=FALSE)
    em[names(x)] <- x

    # create array
    flq <- tapply(em[,"data"], list(factor(x = em[,qname], levels = unique(em[,qname])),
      factor(x = em[,"year"], levels = unique(em[,"year"])),
      factor(x = em[,"unit"], levels = unique(em[,"unit"])),
      factor(x = em[,"season"], levels = unique(em[,"season"])),
      factor(x = em[,"area"], levels = unique(em[,"area"])),
      factor(x = em[,"iter"], levels = unique(em[,"iter"]))), sum)

    # fix dimnames names
    names(dimnames(flq)) <- c(qname, 'year', 'unit', 'season', 'area', 'iter')
    
    # create FLQuant
    flq <- FLQuant(flq, ...)
    
    # units
    if(!is.null(attr(x, 'units')))
      units(flq) <- attr(x, 'units')

    # fill up missing years
    if(length(dimnames(flq)[['year']]) != length(as.character(seq(dims(flq)$minyear,
      dims(flq)$maxyear))))
    {
      res <- FLQuant(dimnames=c(dimnames(flq)[1], list(year=seq(dims(flq)$minyear,
        dims(flq)$maxyear)), dimnames(flq)[3:6]), ...)
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

## dims       {{{
setMethod("dims", signature(obj="FLQuant"),
# Return a list with different parameters
function(obj, ...){
    names <- names(dimnames(obj))
quant   <-  as.numeric(dim(obj)[names == quant(obj)])
min <- suppressWarnings(as.numeric(dimnames(obj)[[quant(obj)]][1]))
max <- suppressWarnings(as.numeric(dimnames(obj)[[quant(obj)]][length(dimnames(obj)[[quant(obj)]])]))
year<-  as.numeric(dim(obj)[names == "year"])
minyear <-  suppressWarnings(as.numeric(dimnames(obj)$year[1]))
maxyear <-  suppressWarnings(as.numeric(dimnames(obj)$year[dim(obj)[names == "year"]]))
unit<-  dim(obj)[names == "unit"]
 season  <-  dim(obj)[names == "season"]
area<-  dim(obj)[names == "area"]
iter <- dim(obj)[names == "iter"]
list <- list(quant=quant, min=min, max=max, year=year, minyear=minyear,
maxyear=maxyear, unit=unit, season=season, area=area, iter=iter)
names(list)[1] <- quant(obj)
return(list)
}
)   # }}}

## is.FLQuant       {{{
is.FLQuant  <-  function(x)
return(is(x, "FLQuant"))
# }}}

## print {{{
if (!isGeneric("print"))
setGeneric("print", useAsDefault=print)

setMethod("print", signature(x="FLQuant"),
function(x){
show(x)
}
)   # }}}

## plot     {{{
setMethod("plot", signature(x="FLQuant", y="missing"),
function(x, xlab="year", ylab=paste("data (", units(x), ")", sep=""),
type='p', ...)
    {
# get dimensions to condition on (length !=1)
condnames <- names(dimnames(x)[c(1,3:5)][dim(x)[c(1,3:5)]!=1])
cond <- paste(condnames, collapse="+")
if(cond != "") cond <- paste("|", cond)
formula <- formula(paste("data~year", cond))
# set strip to show conditioning dimensions names
strip <- strip.custom(var.name=condnames, strip.names=c(TRUE,TRUE))

    # using do.call to avoid eval of some arguments
    lst <- substitute(list(...))
    lst <- as.list(lst)[-1]
        lst$data <- x
    lst$x <- formula
    lst$xlab <- xlab
    lst$ylab <- ylab
    lst$strip <- strip
    lst$type <- type
    if(dim(x)[6] == 1)
      do.call("xyplot", lst)
      else
        do.call("bwplot", lst)
}
)   # }}}

## lattice plots{{{
# xyplot
setMethod("xyplot", signature("formula", "FLQuant"),
function(x, data, ...){
lst <- substitute(list(...))
lst <- as.list(lst)[-1]
    lst$data <- as.data.frame(data)
lst$x <- x
do.call("xyplot", lst)
})

# bwplot
setMethod("bwplot", signature("formula", "FLQuant"),

function(x, data, ...){
lst <- substitute(list(...))
lst <- as.list(lst)[-1]
    lst$data <- as.data.frame(data)
    lst$data$year <- as.factor(lst$data$year)
lst$x <- x
do.call("bwplot", lst)

})

# dotplot
setMethod("dotplot", signature("formula", "FLQuant"), function(x, data, ...){

lst <- substitute(list(...))
lst <- as.list(lst)[-1]
    lst$data <- as.data.frame(data)
    lst$data$year <- as.factor(lst$data$year)
lst$x <- x
do.call("dotplot", lst)

})

# barchart
setMethod("barchart", signature("formula", "FLQuant"), function(x, data, ...){

lst <- substitute(list(...))
lst <- as.list(lst)[-1]
    lst$data <- as.data.frame(data)
lst$x <- x
do.call("barchart", lst)

})

# stripplot
setMethod("stripplot", signature("formula", "FLQuant"), function(x, data, ...){

lst <- substitute(list(...))
lst <- as.list(lst)[-1]
    lst$data <- as.data.frame(data)
    lst$data$year <- as.factor(lst$data$year)
lst$x <- x
do.call("stripplot", lst)

})

# histogram
setMethod("histogram", signature("formula", "FLQuant"), function(x, data, ...){

lst <- substitute(list(...))
lst <- as.list(lst)[-1]
    lst$data <- as.data.frame(data)
lst$x <- x
do.call("histogram", lst)

})

# bubbles
setMethod("bubbles", signature(x="formula", data ="FLQuant"),
function(x, data, bub.scale=2.5, col=c("blue","red"), ...){
dots <- list(...)
data <- as.data.frame(data)
dots$data <- data
dots$cex <- bub.scale*(abs(data$data)/max(abs(data$data),na.rm=T))+bub.scale*0.1
dots$col <- ifelse(data$data>0, col[1], col[2])
dots$panel <- function(x, y, ..., cex, subscripts){
panel.xyplot(x, y, cex=cex[subscripts], ...)
}
call.list <- c(x=x, dots)
ans <- do.call("xyplot", call.list)
ans
})

setMethod("bubbles", signature(x="formula", data ="data.frame"),
function(x, data, bub.scale=2.5, col=c("blue","red"), ...){
dots <- list(...)
  datanm <- as.character(as.list(x)[[2]])
dots$data <- data
dots$cex <- bub.scale*(abs(data[,datanm])/max(abs(data[,datanm]),na.rm=T))+bub.scale*0.1
dots$col <- ifelse(data[,datanm]>0, col[1], col[2])
dots$panel <- function(x, y, ..., cex, subscripts){
panel.xyplot(x, y, cex=cex[subscripts], ...)
}
call.list <- c(x=x, dots)
ans <- do.call("xyplot", call.list)
ans
})

# }}}

# totals {{{
setMethod('quantTotals', signature(x='FLQuant'),
  function(x, na.rm=TRUE) {
    sums <- x
    for (i in 1:dim(x)[2])
      sums[,i,,,,] <- rowSums(x, dim=1, na.rm=na.rm)
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
setMethod('quantSums', signature(x='FLQuant'), function(x, na.rm=TRUE) {
  res <- colSums(x, na.rm=na.rm)
  dim(res) <- c(1, dim(res))
  return(FLQuant(res, dimnames= c(list(quant='all'),dimnames(x)[2:6]), quant=quant(x),
    units=units(x)))
})

setMethod('yearSums', signature(x='FLQuant'), function(x, na.rm=TRUE) {
return(apply(x, c(1,3:6), sum, na.rm=na.rm))
})

setMethod('unitSums', signature(x='FLQuant'), function(x, na.rm=TRUE) {
return(apply(x, c(1:2,4:6), sum, na.rm=na.rm))
})

setMethod('seasonSums', signature(x='FLQuant'), function(x, na.rm=TRUE) {
return(apply(x, c(1:3,5:6), sum, na.rm=na.rm))
})

setMethod('areaSums', signature(x='FLQuant'), function(x, na.rm=TRUE) {
return(apply(x, c(1:4,6), sum, na.rm=na.rm))
})

setMethod('dimSums', signature(x='FLQuant'), function(x, dim=c(1:2,6), na.rm=TRUE) {
return(apply(x, dim, sum, na.rm=na.rm))
}) # }}}

# means         {{{
setMethod('quantMeans', signature(x='FLQuant'), function(x, na.rm=TRUE) {
res <- colMeans(x, na.rm=na.rm)
dim(res) <- c(1, dim(res))
return(FLQuant(res, dimnames= c(list(quant='all'),dimnames(x)[2:6]), quant=quant(x),
    units=units(x)))
})

setMethod('yearMeans', signature(x='FLQuant'), function(x, na.rm=TRUE) {
return(apply(x, c(1,3:6), mean, na.rm=na.rm))
})

setMethod('unitMeans', signature(x='FLQuant'), function(x, na.rm=TRUE) {
return(apply(x, c(1:2,4:6), mean, na.rm=na.rm))
})

setMethod('seasonMeans', signature(x='FLQuant'), function(x, na.rm=TRUE) {
return(apply(x, c(1:3,6), mean, na.rm=na.rm))
})

setMethod('areaMeans', signature(x='FLQuant'), function(x, na.rm=TRUE) {
return(apply(x, c(1:4,6), mean, na.rm=na.rm))
})

setMethod('iterMeans', signature(x='FLQuant'), function(x, na.rm=TRUE) {
return(apply(x, c(1:5), mean, na.rm=na.rm))
})

setMethod('dimMeans', signature(x='FLQuant'), function(x, dim=c(1:2,6), na.rm=TRUE) {
return(apply(x, dim, mean, na.rm=na.rm))
})  # }}}

# medians {{{
setMethod('iterMedians', signature(x='FLQuant'), function(x, na.rm=TRUE) {
return(apply(x, c(1:5), median, na.rm=na.rm))
})

# }}}

# vars         {{{
setMethod('quantVars', signature(x='FLQuant'), function(x, na.rm=TRUE) {
return(apply(x, 2:6, var, na.rm=na.rm))
})

setMethod('yearVars', signature(x='FLQuant'), function(x, na.rm=TRUE) {
return(apply(x, c(1,3:6), var, na.rm=na.rm))
})

setMethod('unitVars', signature(x='FLQuant'), function(x, na.rm=TRUE) {
return(apply(x, c(1:2,4:6), var, na.rm=na.rm))
})

setMethod('seasonVars', signature(x='FLQuant'), function(x, na.rm=TRUE) {
return(apply(x, c(1:3,5:6), var, na.rm=na.rm))
})

setMethod('areaVars', signature(x='FLQuant'), function(x, na.rm=TRUE) {
return(apply(x, c(1:4,6), var, na.rm=na.rm))
})

setMethod('iterVars', signature(x='FLQuant'), function(x, na.rm=TRUE) {
return(apply(x, c(1:5), var, na.rm=na.rm))
})

setMethod('dimVars', signature(x='FLQuant'), function(x, dim=c(1:2,6), na.rm=TRUE) {
return(apply(x, dim, var, na.rm=na.rm))
})   # }}}

# quantile   {{{
setMethod("quantile", signature(x="FLQuant"),
function(x, probs=seq(0, 1, 0.25), na.rm=FALSE, dim=1:5, ...) {
    res <- FLQuant(NA, dimnames=c(dimnames(x)[-6], list(iter=ac(probs))), units=units(x))
for(i in probs)
res[,,,,,ac(i)] <- apply(x, dim, quantile, i, na.rm=na.rm, ...)
return(res)
}
)   # }}}

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
setGeneric("iter<-", function(object, ..., value)
standardGeneric("iter<-"))
setMethod("iter<-", signature(object="FLQuant", value="FLQuant"),
function(object, iter, value)
{
object[,,,,,iter] <- value
return(object)
}
)   # }}}

# propagate {{{
setMethod("propagate", signature(object="FLQuant"),
  function(object, iter, fill.iter=TRUE)
  {
    FLQuant(object, iter=iter, fill.iter=fill.iter)
  }
) # }}}

# rnorm{{{
setMethod("rnorm", signature(n='numeric', mean="FLQuant", sd="FLQuant"),
  function(n=1, mean, sd) {
    if(dim(mean)[6] > 1 | dim(sd)[6] > 1)
      stop("mean or sd can only have iter=1")
    if(any(dim(mean) != dim(sd)))
      stop("dims of mean and sd must be equal")
    FLQuant(array(rnorm(prod(dim(mean)[-6])*n, rep(iter(mean, 1)[drop=TRUE], n),
      rep(iter(sd, 1)[drop=TRUE], n)), dim=c(dim(mean)[-6], n)),
      dimnames=c(dimnames(mean)[-6], list(iter=seq(n))))
  }
)

setMethod("rnorm", signature(n='numeric', mean="FLQuant", sd="numeric"),
function(n=1, mean, sd)
rnorm(n, mean, FLQuant(sd, dimnames=dimnames(mean)))
)
setMethod("rnorm", signature(n='numeric', mean="numeric", sd="FLQuant"),
function(n=1, mean, sd)
rnorm(n, FLQuant(mean, dimnames=dimnames(sd)), sd)
)
setMethod("rnorm", signature(n='numeric', mean="FLQuant", sd="missing"),
function(n=1, mean)
rnorm(n, mean, 1)
)
setMethod("rnorm", signature(n='numeric', mean="missing", sd="FLQuant"),
function(n=1, sd)
rnorm(n, 0, sd)
)
# }}}

# rlnorm {{{
setMethod("rlnorm", signature(n='numeric', meanlog="FLQuant", sdlog="FLQuant"),
  function(n=1, meanlog, sdlog) {
    if(dim(meanlog)[6] > 1 | dim(sdlog)[6] > 1)
      stop("meanlog or sdlog can only have iter=1")
    if(all(dim(meanlog) != dim(sdlog)))
      stop("dims of meanlog and sdlog must be equal")
    FLQuant(array(rlnorm(prod(dim(meanlog)[-6])*n,
      rep(iter(meanlog, 1)[drop=TRUE], n),
      rep(iter(sdlog, 1)[drop=TRUE],n)), dim=c(dim(meanlog)[-6], n)),
      dimnames=c(dimnames(meanlog)[-6], list(iter=seq(n))), fill.iter=TRUE)
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

# }}}

# rpois{{{
setMethod("rpois", signature(n='numeric', lambda="FLQuant"),
function(n=1, lambda) {
    if(dim(lambda)[6] > 1)
      stop("lambda can only have iter=1")
FLQuant(array(rnorm(prod(dim(lambda)[-6])*n, rep(iter(lambda, 1)[drop=TRUE], n)),
      dim=c(dim(lambda)[-6], n)),
dimnames=c(dimnames(lambda)[-6], list(iter=seq(n))), fill.iter=TRUE)
}
) # }}}

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
setGeneric("setPlusGroup", function(x, plusgroup, ...)
standardGeneric("setPlusGroup"))
setMethod("setPlusGroup", signature(x='FLQuant', plusgroup='numeric'),
function(x, plusgroup, na.rm=FALSE, by='mean') {
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

# sweep {{{
if (!isGeneric("sweep"))
setGeneric("sweep", function (x, MARGIN, STATS, FUN = "-", check.margin = TRUE, ...)
standardGeneric("sweep"))

setMethod('sweep', signature(x='FLQuant'),
  function(x, MARGIN, STATS, FUN, check.margin=TRUE, ...)
  {
    res <- callNextMethod()
    FLQuant(res, units=units(x))
  }
) # }}}

# jacknife  {{{
setMethod('jacknife', signature(object='FLQuant'),
  function(object, na.rm=TRUE) {

    # drop NAs if na.rm=TRUE
    if(na.rm)
      object <- object[!is.na(object)]

    # get dimensions
    dmo <- dim(object)

    # propagate
    res <- propagate(object, prod(dmo) + 1)
  
    # create array with 1 at each location by iter
    idx <- array(c(TRUE, rep(NA, prod(dmo[-6]))), dim=c(dmo[-6], prod(dmo)))
    res[,,,,,-1][idx] <- NA

    return(res)
  }
) # }}}

# jackSummary {{{
setMethod("jackSummary", signature(object="FLQuant"),
  function(object, ...) {
   
   n <- dims(object)$iter - 1
   
   mn <- iter(object,  1)
   u <- iter(object, -1)
   mnU <- apply(u, 1:5, mean)   

   SS <- apply(sweep(u, 1:5, mnU,"-")^2, 1:5, sum)

   bias <- (n - 1) * (mnU - mn)
   se <- sqrt(((n-1)/n)*SS)

   return(list(jack.mean=mn, jack.se=se, jack.bias=bias))
  }
) # }}}

# as.data.frame(FLQuant) {{{
setMethod("as.data.frame", signature(x="FLQuant", row.names="missing",
  optional="missing"),
function(x, cohort=FALSE, drop=FALSE) {
    as.data.frame(x, row.names=NULL, cohort=cohort, drop=drop)
  }
)
setMethod("as.data.frame", signature(x="FLQuant", row.names="ANY",
  optional="missing"),
function(x, row.names, cohort=FALSE, drop=FALSE) {

    res <- callNextMethod(x)
    
    # create cohort column as year - age
    if(cohort) {
      res$cohort  <-  as.numeric(NA)
      if(quant(x) == "age")
        try(res$cohort <- res$year - res$age)
    }

    # drops columns with a single value, i.e. dims of length=1
    if(drop) {
      idx <- names(x)[dim(x) > 1]
      res <- res[, c(idx, 'data')]
    }

    return(res)
  }
) # }}}

# combine {{{
setMethod('combine', signature(x='FLQuant', y='FLQuant'),
  function(x, y) {

    dx <- dim(x)
    dy <- dim(y)

    # dim(x)[1:5] == dim(x)[1:5]
    if(any(dx[-6] != dy[-6]))
      stop("Object dimensions [1:5] must match")

    #
    if(!all.equal(dimnames(x)[1:5], dimnames(y)[1:5]))
      warning("dimnames of x and y differ")

    itx <- dx[6]
    ity <- dy[6]

    res <- FLQuant(NA, dimnames=c(dimnames(x)[1:5], list(iter=seq(itx + ity))),
      units=units(x))
    res[,,,,,1:itx] <- x
    res[,,,,,(itx+1):(itx+ity)] <- y
    return(res)
  }
) # }}}

# NOT EXPORTED
## filldimnames       {{{
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
