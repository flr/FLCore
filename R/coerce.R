# coerce - Various coercion methods for FLCore classes
# FLCore/R/coerce.R

# Copyright 2003-2016 FLR Team. Distributed under the GPL 2 or later
# Maintainer: Iago Mosqueira, EC JRC

# TO data.frame {{{
setAs('FLArray', 'data.frame',
  function(from)
  {
    # to avoid warnings when NA have to be added
    options(warn=-1)
    dnames <- dimnames(from)

    # CONVERT year/cohort and iter dnames to numeric
    dnames[[2]] <- as.numeric(dnames[[2]])
    # dnames[["iter"]] <- as.numeric(dnames[["iter"]])
    
    # TURN quant to numeric, if possible
    if(!any(is.na(suppressWarnings(as.numeric(dnames[[1]])))))
      dnames[[1]] <- as.numeric(dnames[[1]])

    # CONVERT dim[c(1,3:6)] to factors
    dnames[c(3:6)] <- lapply(dnames[c(3:6)], as.factor)

    df <- data.frame(do.call(expand.grid, list(dnames, stringsAsFactors = FALSE)),
      data=c(from))

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
      name=from@name, desc=from@desc, mat=mat(from),
      m.spwn=from@spwn, harvest.spwn=from@spwn, range=from@range)
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
  function(from) {
    out <- FLBiol(n=from@stock.n, wt=from@stock.wt, m=from@m, spwn=from@m.spwn[1,],
      mat=new("predictModel", FLQuants(mat=from@mat), model=~mat),
      fec=new('predictModel', FLQuants(fec=from@mat), model=~fec),
      rec = new('predictModel', FLQuants(rec=from@stock.n[1,]), model=~rec),
      name=from@name, desc=from@desc, range=from@range)
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
    # process modelPredict slots
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
        srparams = as(from@rec@params, "FLQuant"))
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
