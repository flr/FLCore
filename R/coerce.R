# coerce - Various coercion methods for FLCore classes
# FLCore/R/coerce.R

# Copyright 2003-2014 FLR Team. Distributed under the GPL 2 or later
# Maintainer: Iago Mosqueira, JRC
# $Id: coerce.R 1789 2012-12-10 10:34:22Z imosqueira $

# TO data.frame {{{

setAs('FLArray', 'data.frame',
	function(from)
	{
		# to avoid warnings when NA have to be added
		options(warn=-1)
    dnames <- dimnames(from)
        if(!any(is.na(suppressWarnings(as.numeric(dnames[[1]])))))
            quant <- as.numeric(dimnames(from)[[1]])
        else
			      quant <- factor(dnames[[1]], levels=dnames[[1]])
		df <- data.frame(expand.grid(quant=quant,
			year=as.numeric(dnames[[2]]),
			unit=factor(dnames[[3]], levels=dnames[[3]]),
			season=factor(dnames[[4]], levels=dnames[[4]]),
			area=factor(dnames[[5]], levels=dnames[[5]]),
			iter=factor(dnames[[6]], levels=dnames[[6]])),
			data=as.vector(from))
		names(df)[1:2] <- names(dnames)[1:2]
		attributes(df)$units <- units(from)
		options(warn=0)
		return(df)
	}
)

setAs('FLPar', 'data.frame',
  function(from)
  {
	  return(data.frame(expand.grid(dimnames(from)), data=as.vector(from@.Data)))
  }
)
# }}}

# as.data.frame {{{

#' Method as.data.frame
#' 
#' This method converts an \code{FLQuant} or any other FLR object composed of
#' FLQuants into a \link[base]{data.frame}.
#' 
#' For a single \code{\link{FLQuant}}, the \code{data.frame} returned has 7
#' columns: \code{quant}, \code{year}, \code{unit}, \code{season}, \code{area},
#' \code{iter} and \code{data}.  The last column contains the actual values
#' stored in the original object, while the first six contain the corresponding
#' dimensions. The \code{quant}, \code{year} and \code{data} columns are of
#' class \link[base]{numeric}, while the other four columns are of class
#' \link[base]{factor}.
#' 
#' When converting an \code{\linkS4class{FLCohort}} object, the \code{year}
#' column is substituted by \code{cohort}.
#' 
#' The \code{data.frame} returned for complex objects, i.e. those that inherit
#' from class \code{\link{FLComp}}, has an extra column, \code{slot}, that
#' holds the name of the slot in the original object.
#' 
#' The data.frame obtained from a \code{\linkS4class{FLQuants}} object also
#' has an extra column, named \code{qname}, that refers to the name of each
#' \code{FLQuant} object in the list.  This column is named \code{cname} when
#' an \code{\linkS4class{FLCohorts}} object is converted.
#' 
#' Objects of class \code{\linkS4class{FLQuants}} can also be converted into a
#' wide-format table, where data from the list elements are placed in separate
#' colums, using \code{\link{model.frame,FLlst-method}}.
#'
#' @name as.data.frame
#' @aliases as.data.frame-FLCore coerce,FLArray,data.frame-method
#' as.data.frame,FLArray,missing,missing-method
#' as.data.frame,FLQuant,missing,missing-method
#' as.data.frame,FLCohort,missing,missing-method
#' as.data.frame,FLComp,missing,missing-method
#' as.data.frame,FLQuants,missing,missing-method
#' as.data.frame,FLCohorts,missing,missing-method
#' as.data.frame,FLPar,ANY,ANY-method as.data.frame,FLCohort,ANY,ANY-method
#' @docType methods
#' @section Generic function: as.data.frame(x, row.names, optional)
#' @author The FLR Team
#' @seealso \link[base]{as.data.frame}, \link[stats]{model.frame},
#' \link{model.frame,FLlst-method}
#' @keywords methods
#' @examples
#' 
#' data(ple4)
#'
#' # An FLQuant object
#'   fdf <- as.data.frame(catch.n(ple4))
#'   head(fdf)
#'   summary(fdf)
#'
#' # A more complex FLStock object that inherits from class FLComp
#'   sdf <- as.data.frame(ple4)
#'   head(sdf)
#'
#' # An FLQuants object
#'   fqs <- FLQuants(ssb=ssb(ple4), catch=catch(ple4), rec=rec(ple4),
#'     f=fbar(ple4))
#'   sdf2 <- as.data.frame(fqs)
#'   head(sdf2)
#'
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
)

#' @rdname as.data.frame
#' @aliases as.data.frame,FLPar,ANY,ANY-method
setMethod("as.data.frame", signature(x="FLPar"),
	function(x, row.names='col', optional=FALSE, drop=FALSE) {
	  res <- as(x, 'data.frame')
    if(drop) {
      idx <- names(x)[dim(x) > 1]
      res <- res[, c(idx, 'data')]
    }
    return(res)
  }
)
# }}}

# TO FLQuant {{{
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
    em <- data.frame(quant=rep('all', n), year=rep(1,n),
        unit=rep('unique',n), season=rep('all',n),
        area=rep('unique',n), iter=rep(1,n), stringsAsFactors=FALSE)

    names(em)[names(em)=="quant"] <- qname
    from[,!names(from)%in%'data'] <- as.data.frame(as.matrix(from[,
        !names(from)%in%'data']), stringsAsFactors=FALSE)
    em[names(from)] <- from

    # create array
    flq <- tapply(em[,"data"], list(em[,qname], em[,"year"], em[,"unit"],
        em[,"season"], em[,"area"], em[,"iter"]), sum)

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
)
# }}}

# TO FLStock {{{
setAs('FLBiol', 'FLStock',
	function(from)
	{
		FLStock(stock.n=from@n, stock.wt=from@wt, m=from@m,
			name=from@name, desc=from@desc, mat=from@fec,
			m.spwn=from@spwn,harvest.spwn=from@spwn, range=from@range)
	}
)

setAs('data.frame', 'FLStock',
	function(from)
	{
        slots <- as.character(unique(from$slot))
        lst <- vector(length=length(slots), mode='list')
        names(lst) <- slots

        for(i in slots) {
            lst[[i]] <- as.FLQuant(subset(from, slot==i, select=-slot))
        }

        return(do.call('FLStock', lst))
    }
)

setMethod('as.FLStock', signature(object='data.frame'),
    function(object, units=missing) {

        res <- as(object, 'FLStock')

        # units
        if(missing(units))
            warning("units in FLStock slots not specified")
        else
            units(res) <- units

        return(res)
    }
)

# }}}

# TO FLI  {{{
setAs("data.frame", "FLI",
  function(from)
  {
  lst <- list()
  qnames <- as.character(unique(from$slot))
  for (i in qnames)
    lst[[i]] <- as.FLQuant(from[from$slot==i,-1])
  do.call('FLI', lst)
  }
) # }}}

# TO FLIndex {{{
setAs('FLBiol', 'FLIndex',
  function(from)
	{
    dmns<-dimnames(from@n)
    dmns$age<-"all"

		res<-FLIndex(index      =from@n,
                 index.var  =FLQuant(NA, dimnames=dimnames(from@n)),
                 catch.n    =from@n,
                 catch.wt   =from@wt,
                 effort     =FLQuant(1,  dimnames=dmns),
                 sel.pattern=FLQuant(NA, dimnames=dimnames(from@n)),
                 index.q    =FLQuant(1,  dimnames=dimnames(from@n)),
                 range      =from@range,
                 type="number",
			           name=from@name, desc=paste("Coerced from FLBiol:",from@desc))

    units(res@index)   <-units(from@n)
    units(res@catch.n) <-units(from@n)
    units(res@catch.wt)<-units(from@wt)

    res@range<-c(res@range,startf=0.0,endf=0.01)

  return(res)
	}
)

setAs('FLStock', 'FLIndex',
	function(from)
	{
    dmns<-dimnames(from@catch.n)
    dmns$age<-"all"

		res<-FLIndex(index       =from@stock.n,
                 catch.n     =from@catch.n,
                 catch.wt    =from@catch.wt,
                 effort      =FLQuant(1,dimnames=dmns),
                 index.q     =FLQuant(1,dimnames=dmns),
                 index.var   =FLQuant(NA, dimnames=dimnames(from@stock.n)),
                 range       =c(from@range, startf=0, endf=1),
                 type        ="number",
			           name        =from@name,
                 desc        =paste("Coerced from FLStock:",from@desc))

    if(units(harvest(from)) == 'f')
      sel.pattern(res) <- sweep(from@harvest,2:6,fbar(from),"/")

    units(res@index)   <-units(from@stock.n)
    units(res@catch.n) <-units(from@catch.n)
    units(res@catch.wt)<-units(from@catch.wt)

  return(res)
	}
)
# }}}

# TO FLBiol  {{{
setAs('FLStock', 'FLBiol',
	function(from)
	{
		FLBiol(n=from@stock.n, wt=from@stock.wt, m=from@m,
			name=from@name, desc=from@desc, fec=from@mat,
			spwn=from@m.spwn, range=from@range)
	}
)
# }}}

# TO FLPar	{{{
setAs('data.frame', 'FLPar',
  function(from) {

    # iter names from df
		if("iter" %in% colnames(from))
			iters <- from$iter
		# or from rownames, if present
		else
    	iters <- rownames(from, do.NULL=TRUE, prefix="")

    # param named columns
		pnames <- colnames(from)[!colnames(from) %in% c("data", "iter")]

		pnames <- lapply(as.list(as.list(subset(from, select=pnames))), unique)
		pnames <- lapply(pnames, as.character)

	  dmns <- c(pnames, list(iter=unique(iters)))

		return(FLPar(from$data, dimnames=dmns, units="NA"))
  }
)

# }}}

# TO list {{{
setAs("FLPar", "list",
    function(from) {
        lst <- split(from@.Data, 1:nrow(from))
        names(lst) <- dimnames(from)[[1]]
        return(lst)
    }
) # }}}

# TO FLQuants  {{{
setAs('FLComp', 'FLQuants',
	function(from)
  {
		qas <- getSlotNamesClass(from, 'FLArray')

    res <- vector(mode='list', length=length(qas))
    names(res) <- qas

    for (i in qas)
      res[[i]] <- slot(from, i)

    return(FLQuants(res))
  }
)


setAs('data.frame', 'FLQuants',
	function(from)
  {
    qns <- as.character(unique(from$qname))

    res <- vector(mode='list', length=length(qns))
    names(res) <- qns

    for(i in qns)
      res[[i]] <- as.FLQuant(subset(from, qname == i, -qname))

    return(FLQuants(res))
  }
)

# }}}
