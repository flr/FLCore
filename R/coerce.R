# coerce - Various coercion methods for FLCore classes
# FLCore/R/coerce.R

# Copyright 2003-2016 FLR Team. Distributed under the GPL 2 or later
# Maintainer: Iago Mosqueira, EC JRC

#' Convert Objects Between Classes
#'
#' Objects of various **FLCore** classes can be converted into other classes,
#' both basic R ones, like `data.frame`, and others defined in the package. For
#' the specifics of the precise calculations carried out for each pair of
#' classes, see below.
#'
#' @param object Object to be converted.
#' @param Class Name of the class to convert the object to, `character`.
#'
#' @return An object of the requested class.
#'
#' @rdname coerce-methods
#' @name coerce-methods
#' @docType methods
#' @md
#' @author The FLR Team
#' @seealso [base::as], [base::coerce]
#' @keywords methods
NULL

# TO data.frame {{{

#' @rdname coerce-methods
#' @name coerce-methods
#' @section FLArray to data.frame:
#'  The six dimensions of an `FLArray` are converted into seven columns, named
#'  `quant` (or any other name given to the first dimension in the object),
#'  `year`, `unit`, `season`, `area`, `iter`  and `data`. The last one contains
#'  the actual numbers stored in the array. `units` are stored as an attribute
#'  to the `data.frame`. The `year` and `data` columns are of type `numeric`,
#'  while all others are `factor`.
#' @examples
#' # from FLQuant to data.frame
#' as(FLQuant(rnorm(100), dim=c(5, 20)), "data.frame")

setAs('FLArray', 'data.frame',
  function(from)
  {
    # to avoid warnings when NA have to be added
    options(warn=-1)
    dnames <- dimnames(from)

    # CONVERT year/cohort dnames to numeric
    dnames[[2]] <- as.numeric(dnames[[2]])
    
    # TURN quant to numeric, if possible
    if(!any(is.na(suppressWarnings(as.numeric(dnames[[1]])))))
      dnames[[1]] <- as.numeric(dnames[[1]])

    # CONVERT dim[c(3:5)] to factors
    dnames[c(3:5)] <- lapply(dnames[c(3:5)], function(x)
      factor(x, levels=x))

    df <- data.frame(do.call(expand.grid, list(dnames,
      stringsAsFactors = FALSE)), data=c(from), stringsAsFactors = FALSE)

    attributes(df)$units <- units(from)
    options(warn=0)
    return(df)
  }
)

#' @rdname coerce-methods
#' @name coerce-methods
#' @section FLPar to data.frame:
#'  The two or more dimensions of an *FLPar* objects are converted into three or
#'  more columns. For a 2D objects, they are named *params*, *iter* and *data*.
#'  The last one contains the actual numbers stored in the array, in a column
#'  type `numeric`, while all others are `factor`.
#' @examples
#' # from FLPar to data.frame
#' as(FLPar(phi=rnorm(10), rho=rlnorm(10)), "data.frame")

setAs('FLPar', 'data.frame',
  function(from)
  {
    return(data.frame(expand.grid(dimnames(from)), data=as.vector(from@.Data),
      stringsAsFactors = FALSE))
  }
)

setAs('FLPars', 'data.frame',
  function(from)
  {
    dfs <- lapply(from, as, "data.frame")
    dfs <- lapply(names(dfs), function(x) cbind(dfs[[x]], data.frame(qname=x)))
    dfs <- do.call(rbind, dfs)

    dfs$qname <- factor(dfs$qname, levels=names(from))

    return(dfs)
  }
)
# }}}

# TO FLQuant {{{

#' @rdname coerce-methods
#' @section data.frame to FLQuant:
#'  A *data.frame* with the right column names is converted into an *FLQuant*
#'  object with missing values being added. Missing columns are assumed to
#'  contain the default dimnames in *FLQuant*.
#' @examples
#' # from data.frame to FLQuant
#' as(data.frame(age=rep(1:4, each=3), year=2011:2013, data=rnorm(12)), "FLQuant")

setAs("data.frame", "FLQuant",
  function(from) {
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
      dims(flq)$maxyear)))) {
      res <- FLQuant(dimnames=c(dimnames(flq)[1], list(year=seq(dims(flq)$minyear,
        dims(flq)$maxyear)), dimnames(flq)[3:6]))
      res[,dimnames(flq)[['year']],] <- flq
      flq <- res
    }
  return(flq)
  }
) # }}}

# TO FLStock {{{

#' @rdname coerce-methods
#' @aliases coerce,FLBiol,FLStock-method
#' @section *FLBiol* to *FLStock*:
#'  - *n* = *stock.n*
#'  - *wt* = *stock.wt*
#'  - *m* = *m*
#'  - *mat* = *mat()*
#'  - *m.spwm*, *harvest.spwn* = *spwn*
#' @examples
#' # from FLBiol to FLStock
#' data(ple4.biol)
#' fls <- as(ple4.biol, 'FLStock')

setAs('FLBiol', 'FLStock',
  function(from)
  {
    dms <- dimnames(from@n)[1]
    spwn <- do.call(expand, c(list(x=from@spwn, fill=TRUE), dms))
    rang <- c(range(from))
    rang <- c(rang, minfbar=unname(rang["min"]),
      maxfbar=unname(rang["max"]))
    
    res <- FLStock(stock.n=from@n, stock.wt=from@wt, m=from@m,
      name=from@name, desc=from@desc, mat=mat(from),
      m.spwn=spwn, harvest.spwn=spwn, range=rang)
    
    return(res)
  }
)

#' @rdname coerce-methods
#' @aliases coerce,data.frame,FLStock-method
#' @section *data.frame* to *FLStock*:
#'  A *data.frame* with the right column names is converted into an *FLStock*
#'  object with missing values being added.
#' @examples
#' # from data.frame to FLStock
#' df <- data.frame(slot="m", age=rep(1:5, each=5), year=rep(2000:2004, 5),
#'   data=0.2, units="m")
#' fls <- as(df, 'FLStock')

setAs('data.frame', 'FLStock',
  function(from) {
    slots <- as.character(unique(from$slot))
    lst <- vector(length=length(slots), mode='list')
    names(lst) <- slots

    cnms <- colnames(from)

    for(i in slots) {
      lst[[i]] <- as.FLQuant(subset(from, slot==i, select=-slot))
    }
    return(do.call('FLStock', lst))
  }
)

setMethod('as.FLStock', signature(object='data.frame'),
  function(object, units=missing, ...) {

    res <- as(object, 'FLStock')

    # units
    if(!missing(units))
      units(res) <- units

    # args
    args <- list(...)

    if(length(args) > 0)
      for(i in names(args))
        slot(res, i) <- args[[i]]

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
  function(from) {

    mat <- new("predictModel", FLQuants(mat=from@mat), model=~mat)
    fec <- new("predictModel", FLQuants(fec=from@mat %=% 1), model=~fec)

    out <- FLBiol(n=from@stock.n, wt=from@stock.wt, m=from@m,
      spwn=from@m.spwn[1,], mat=mat, fec=fec,
      rec = new('predictModel', FLQuants(rec=from@stock.n[1,]), model=~rec),
      name=from@name, desc=from@desc,
      range=from@range[!names(range(from)) %in% c('minfbar', 'maxfbar')])

    # Empty desc and name slots are a frequent issue with FLasher, i.e. character(0)
    # So check if empty and if fill with something
    if (identical(character(0), name(out))){
        name(out) <- ""
    }
    if (identical(character(0), desc(out))){
        desc(out) <- ""
    }
    return(out)
  }
) # }}}

# TO FLBiolcpp {{{

setAs('FLBiol', 'FLBiolcpp',
  function(from) {
    if("year" %in% names(from@rec@params))
      srparams <- window(as(from@rec@params, "FLQuant"),
        start=dims(from)$minyear, end=dims(from)$maxyear)
    else
      srparams <- as(from@rec@params, "FLQuant")
    
    new("FLBiolcpp",
        name = name(from),
        desc = desc(from),
        range = range(from),
        n = n(from),
        m = m(from),
        wt = wt(from),
        mat = mat(from),
        fec = fec(from),
        spwn = spwn(from),
        srmodel = SRModelName(from@rec@model),
        srparams = srparams)
  }
) # }}}

# TO FLPar  {{{
setAs('data.frame', 'FLPar',
  function(from) {
    
    # long ...
    if(!"data" %in% colnames(from)) {

      do.call('FLPar', c(from))

    # ... or wide
    } else {

      # iter names from df
      if(!"iter" %in% colnames(from))
        from$iter <- "1"

      # param named columns
      pnames <- colnames(from)[!colnames(from) %in% c("data")]

      dmns <- lapply(as.list(as.list(subset(from, select=pnames))), unique)
      dmns <- lapply(dmns, as.character)

      idx <- match(pnames,
        c("params", pnames[!pnames %in% c("params", "iter")], "iter"))

      dmns <- dmns[idx]

      return(FLPar(from$data, dimnames=dmns, units="NA"))
    }
  }
)

setAs('data.frame', 'FLPars',
  function(from) {

  return(do.call(rbind, c(Map(function(x, y) cbind(y, qname=x),
      x=setNames(nm=names(from)), y=lapply(from, as, "data.frame")),
      make.row.names = FALSE)))
  })

# }}}

# TO list {{{
setAs("FLPar", "list",
    function(from) {
        lst <- split(from@.Data, 1:nrow(from))
        names(lst) <- dimnames(from)[[1]]
        return(lst)
    }
) 

setAs("predictModel", "list",
  function(from) {

  res <- list()
  
  mod <- slot(from, 'model')
  fqs <- slot(from, '.Data')
  flp <- slot(from,'params')

  # EXTRACT expression to evaluate
  args <- all.names(mod, functions=FALSE)

  # (1) EXTRACT from FLQuants

  # MATCH names
  idx <- names(fqs) %in% args
  
  # EXTRACT
  if(any(idx)) {
    res <- fqs[idx]
    names(res) <- names(fqs)[idx]
    
    # DROP extracted args
    args <- args[!args %in% names(res)]
  }

  # (2) FLPar
  pars <- as(flp, 'list')
  idx <- names(pars) %in% args
  if(any(idx)) {
    res <- c(res, pars[idx])
    
    # DROP extracted args
    args <- args[!args %in% names(res)]
  }

  # RETURN
  return(res)
  }) 
# }}}

# TO FLQuants  {{{

#' @rdname coerce-methods
#' @aliases coerce,FLStock,FLQuants-method
#' @section FLComp to FLQuant:
#'  A *data.frame* with the right column names is converted into an *FLQuant*
#' @examples
#' # from data.frame to FLQuant

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
      res[[i]] <- as(from[from$qname == i, !names(from) %in% 'qname'], 'FLQuant')

    return(FLQuants(res))
  }
)

# }}}

# TO FLSR {{{

#' @rdname coerce-methods
#' @section FLBiol to FLSR:
#'  Slots `rec` and `ssb` in `FLSR` are populated from `n[1,]` and the result of
#'  `ssb()`, adjusted from recruitment age. Note `ssb(FLBiol)` only corrects for
#'  natural mortality (`m`) to time of spanwing (`spwn`).
#' @examples
#' # from FLBiol to FLSR
#' as(ple4.biol, "FLSR")

setAs('FLBiol', 'FLSR',
  function(from) {

    # rec & ssb
    ssb <- ssb(from)
    rage <- dims(from)$min
    rec <- n(from)[ac(rage),]

    # CORRECT for rage
    rec <- rec[, seq(1 + rage, dim(rec)[2])]
    ssb <- ssb[, seq(1, dim(ssb)[2] - rage)]

    res <- FLSR(name=name(from), desc=desc(from), rec=rec, ssb=ssb,
      model=model(rec(from, FALSE)), params=params(rec(from, FALSE)))

    # PREDICT if possible
    pred <- tryCatch(predict(res))
    if(is(pred, "FLQuant")) {
      fitted(res) <- pred
      residuals(res) <- rec(res) - pred
    }

    return(res)
  } )

setAs('predictModel', 'FLSR',
  function(from) {

    return(FLSR(model=model(from), params=params(from)))

  } )
# }}}

# TO predictModel {{{

#' @rdname coerce-methods
#' @section FLSR to predictModel:
#'  Places the `model` and `params` slots the same-named slots of `predictModel`.
#'  The `residuals` `FLQuant` is stored under that name in the base `FLQuants`
#'  slot.
#' @examples
#' # from FLSR to predictModel
#' data(nsher)
#' as(nsher, 'predictModel')

setAs('FLSR', 'predictModel',
  function(from) {

    return(predictModel(FLQuants(residuals=residuals(from)),
      model=model(from), params=params(from)))

  } )# }}}

# TODO TO FLIndexBiomass
# FLIndex - FLIndexBiomass
# FLStock - FLIndexBiomass
# as.FLIndexBiomass

# TO FLIndexBiomass {{{

setAs('FLIndex', 'FLIndexBiomass',
  function(from) {

  res <- FLIndexBiomass(index=quantSums(index(from) * catch.wt(from)),
    sel.pattern=sel.pattern(from), effort=effort(from),
    index.q=quantMeans(index.q(from)), catch.wt=catch.wt(from),
    name=name(from), desc=desc(from), range=range(from))

  return(res)
  }
)
# }}}
