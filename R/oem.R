# oem.R - DESC
# FLCore/R/oem.R

# Copyright European Union, 2017
# Author: Iago Mosqueira (EC JRC) <iago.mosqueira@ec.europa.eu>

# Observation

# survey {{{

#' A method to generate observations of abundance at age.
#'
#' @param object The object on which to draw the observation
#'
#' @return An FLQuant for the index of abundance
#'
#' @rdname survey
#' @author The FLR Team
#' @seealso \link{FLComp}
#' @keywords classes

setGeneric("survey", function(object, index, ...) standardGeneric("survey"))

#' @rdname survey
#' @examples
#' data(ple4)
#' data(ple4.index)
#' # CONSTRUCT a survey from stock and index
#' survey(ple4, ple4.index)

setMethod("survey",   signature(object="FLStock", index="FLIndex"),
  function(object, index, sel=sel.pattern(index),
    ages = dimnames(index)$age,
    timing = mean(range(index, c("startf", "endf"))),
    index.q = index@index.q, stability=1) {

    # COMPUTE index
    abnd <- index(object, sel=sel, ages=ages, timing=timing)

    # APPLY Q
    index(index) <- index.q %*% abnd ^ stability

    return(index)

  }
)

#' @rdname survey
#' @examples
#' # Create FLIndexBiomass
#' ple4.biom <- as(ple4.index, "FLIndexBiomass")
#' survey(ple4, ple4.biom)

# TODO: ADD catch.n

setMethod("survey", signature(object="FLStock", index="FLIndexBiomass"),
  function(object, index, sel=sel.pattern(index),
    ages=ac(seq(range(index, c('min')), range(index, c('max')))),
    timing = mean(range(index, c("startf", "endf"))),
    catch.wt = index@catch.wt,
    index.q = index@index.q, stability = 1) {

    # CHECK timing
    if(is.na(timing))
      stop("Index timing not set and missing from range c('startf', 'endf')")

    # CHECK catch.wt
    if(any(is.na(catch.wt)))
      warning("NAs found in catch.wt(index), maybe call using 'catch.wt=stock.wt(stock)'")

    # COMPUTE index
    abnd <- index(object, sel=sel, ages=ages, timing=timing)

    # ADD propatage
    catch.n(index)[ages,] <- abnd

    # APPLY Q on biomass
    index(index) <- c(index.q * unitSums(quantSums(abnd *
      catch.wt[ages, dimnames(index)$year])) ^ stability)

    return(index)

  }
)

#' @rdname survey
#' @examples
#' data(ple4)
#' survey(ple4)
#' survey(ple4, biomass=TRUE)

setMethod("survey",   signature(object="FLStock", index="missing"),
  function(object, sel=catch.sel(object), ages=dimnames(sel)$age,
    timing = 0.5, index.q=1, biomass=FALSE, stability=1) {

    # COMPUTE index
    abnd <- index(object, sel=sel, ages=ages, timing=timing)

    # SELECT output class
    if(biomass)
      ind <- FLIndexBiomass(
        index=quantSums((abnd * stock.wt(object)[ages,]) ^ stability * index.q),
        index.q=quantSums(abnd) %=% index.q, sel.pattern=sel[ages,],
        range=c(min=as.numeric(ages[1]), max=as.numeric(ages[length(ages)]),
        startf=timing, endf=timing))
    else
      ind <- FLIndex(index=abnd ^ stability * index.q,
        catch.wt=stock.wt(object)[ages,], index.q=abnd %=% index.q,
        sel.pattern=sel[ages,], range=c(startf=timing, endf=timing),
        type="number")

    return(ind)
  }
)

#' @rdname survey

setMethod("survey", signature(object="FLStock", index="FLIndices"),
  function(object, index, ...) {

  res <- lapply(index, function(x) survey(object=object, index=x, ...))

  return(res)
  }
)

# }}}

# index(FLStock) {{{

#' @examples
#' data(ple4)
#' index(ple4, timing=0.9)
#' index(ple4, timing=0)

setMethod("index",   signature(object="FLStock"),
  function(object, sel=catch.sel(object), ages=dimnames(sel)$age,
    timing = 0.5) {

    # timing MUST BE 0 - 1
    timing <- pmax(pmin(timing, 1.0), 0.0)

    # GET index years & ages
    yrs <- dimnames(sel)$year

    # CORRECT abundances for timing
    stock.n <- stock.n(object) *
      exp(-harvest(object) * timing - m(object) * timing)
    
    # APPLY survey selectivity
    res <- stock.n[ages, yrs] %*% expand(sel, year=yrs)[ages, yrs]
    
    # SET units as stock.n
    units(res) <- units(stock.n)

    # SELECT ages
    res <- res[ages,]
  
    # IF NA, for low N, set small value
    res[is.na(res)] <- 1e-16

    return(res)

  }
)
# }}}

# cpue {{{

#' cpue, a method to generate an observation of a CPUE index of abundance
#'
#' The observation of stock abundance by CPUE series from commercial fleets is an
#' important step in the generation of management advice that needs to replicated
#' on an Operating Model during any simulation exercise. This method gemnerates
#' an observation of biomass or numbers-at-age from an FLstock being used as OM.
#'
#' @param object The object from which to generate the observation.
#' @param sel The selectivity of the survey, defaults to be 1 for all ages.
#' @param effort Units of index to use to mimic effort series in the fishery, "f" or "hr"
#' @param mass Is the index to be in weight at age?
#'
#' @return An FLQuant for the index of abundance, age-disaggregated
#'
#' @name cpue
#' @rdname cpue
#' @aliases cpue cpue-methods
#'
#' @author Laurie Kell & Iago Mosqueira, FLR Team.
#' @seealso \link{FLComp}
#' @keywords methods
#' @examples
#'
#' data(ple4)
#' 
#' cpue(ple4)
#' # Am aggregated biomass CPUE
#' quantSums(cpue(ple4))
#'
#' \dontrun{
#' plot(FLQuants(om=stock(ple4), cpue=quantSums(cpue(ple4)),
#'   hr=quantSums(cpue(ple4, effort="hr"))))
#' }

setGeneric("cpue", function(object, index, ...) standardGeneric("cpue"))

#' @rdname cpue
#' @aliases cpue,FLStock-method

setMethod('cpue', signature(object='FLStock', index="missing"),
  function(object, sel.pattern=harvest(object),
    effort = units(harvest(object)), biomass = TRUE) {
    
    # EFFORT from F or HR
    if (effort[1] == "hr")
      E <- catch(object) / stock(object)
    else if (effort[1] == "f") 
      E <- fbar(object)
    else 
      E <- fbar(object) %=% effort
    
    cpue <- (catch.n(object) %*% sel.pattern) %/% E

    if (biomass)
      cpue <- quantSums(cpue * catch.wt(object))

  return(cpue)
  }
) # }}}

# hyperstability {{{

hyperstability <- function(object, omega=1, ref=yearMeans(object)) {
  return(ref %*% ((object %/% ref) ^ omega))
} # }}}

# computeQ(FLIndices, FLStock, FLQuants) {{{

setMethod("computeQ", signature=c(indices="FLIndices", stock="FLStock",
  fit="FLQuants"), function(indices, stock, fit) {

  # SET iterMedians for stock
  yrs <- dimnames(stock)$year

  # LOOP over indices
  res <- Map(function(x, y) {

    # GET mean index timing and dimnames
    t <- mean(range(x)[c("startf", "endf")])
    dms <- dimnames(x)[1:2]
 
    # FLIndex
    if(is(x, "FLIndex")) {

      # CORRECT abundances for Z
      nay <- stock.n(stock) * exp(-z(stock) * t)
      nay <- nay[dms[[1]], dms[[2]]]
      
      # COMPUTE Q
      iq <- y / nay

    # FLIndexBiomass
    } else if(is(x, "FLIndexBiomass")) {
      
      # CORRECT abundances for Z
      nay <- stock.n(stock) * exp(-z(stock) * t)
      nay <- nay[dms[[1]], dms[[2]]]
      
      # SET sel.pattern if missing
      if(all(is.na(sel.pattern(x)))) {
        sel.pattern(x) <- 0
        sel.pattern(x)[seq(range(x, 'min'), range(x, 'max')),] <- 1
      }

      # COMPUTE vb
      biy <- quantSums(nay * stock.wt(stock)[dms[[1]], dms[[2]]] *
        sel.pattern(x))

      iq <- y / biy
    }

    return(iq)

    }, x=indices, y=fit)

  return(FLQuants(res))
  }
)

#' @examples
#' computeQ(ple4.index, ple4, rlnorm(1, log(index(ple4.index)), 0.1))

setMethod("computeQ", signature=c(indices="FLIndex", stock="FLStock",
  fit="FLQuant"), function(indices, stock, fit) {
    
    res <- computeQ(indices=FLIndices(A=indices), stock=stock, 
      fit=FLQuants(A=fit))

    return(res[[1]])

  }
)
# }}}

# Noise

# bias {{{

bias <- function(object, bias=0.02){
  return(FLQuant(cumprod(1 + rep(c(bias), dim(object)[2])), dimnames=dimnames(object)))
}

biased <- function(object, bias=0.02){
  return(object * bias(object, bias=bias))
} # }}}

# rnoise, rlnoise {{{

#' @title Random noise with different frequencies
#' 
#' @description A noise generator
#' 
#' @param n number of iterations
#' @param len an \code{FLQuant}
#' @param sd standard error for simulated series
#' @param b autocorrelation parameter a real number in [0,1] 
#' @param burn gets rid of 1st values i series
#' @param trunc get rid of values > abs(trunc)
#' @param what returns time series for year, cohort or age"
#' @param ... any
#' @aliases rnoise rnoise-method rnoise,numeric,FLQuant-method rnoise,numeric,missing-method
#' @aliases rlnoise rlnoise-method rlnoise,numeric,FLQuant-method rlnoise,numeric,missing-method
#' 
#' @docType methods
#' @rdname rnoise
#'
#' @return A \code{FLQuant} with autocorrelation equal to B.
#' 
#' @references Ranta and Kaitala 2001 Proc. R. Soc.
#' vt = b * vt-1 + s * sqrt(1 - b^2)
#' s is a normally distributed random variable with mean = 0
#' b is the autocorrelation parameter
#' @examples
#' \dontrun{
#' flq <- FLQuant(1:100, quant="age")
#' white <- rnoise(100,flq,sd=.3,b=0)
#' plot(white)
#' acf(white)
#' 
#' red <- rnoise(100,flq,sd=.3,b=0.7)
#' plot(red)
#' acf(red)
#' 
#' res <- rnoise(100,flq,sd=.3,b=0)
#' 
#' ggplot() +
#'   geom_point(aes(year,age,size=data),
#'     data=subset(as.data.frame(res), data>0)) +
#' geom_point(aes(year,age,size=-data),
#'             data=subset(as.data.frame(res),data<=0),colour="red")+
#' scale_size_area(max_size=4, guide="none")+
#' facet_wrap(~iter)
#' 
#' data(ple4)
#' res <- rnoise(4,m(ple4),burn=10,b=0.9,what="cohort")
#' ggplot()+
#' geom_point(aes(year,age,size= data),
#'           data=subset(as.data.frame(res),data>0))+
#' geom_point(aes(year,age,size=-data),
#'           data=subset(as.data.frame(res),data<=0),colour="red")+
#' scale_size_area(max_size=4, guide="none")+
#' facet_wrap(~iter)
#' 
#' }

setMethod("rnoise", signature(n='numeric', len="FLQuant"),
  function(n=n, len=len, sd=1, b=0, burn=0, trunc=0,
    what=c("year","cohort","age"), seed=NA) {
 
    # CHECK and ADJUST len dims
    if(!dim(len)[6] %in% c(1, n))
      stop("len must have 1 or n iters")

    if(dim(len)[6] == 1)
      res <- propagate(len, n)
    else
      res <- len

    # APPLY by dim
    switch(what[1],
      "cohort"={
        object <- as(len, "FLCohort")
        res <- apply(object, c(2:6), function(x)
          t(noiseFn(len=length(x), sd=sd, b=b, burn=burn, trunc=trunc, seed=seed)))
        res <- array(res, unlist(lapply(dimnames(object), length)),
          dimnames=dimnames(object))
        res <- as(FLCohort(res), "FLQuant")
      },
      "year" = {
        leng <- prod(dim(len)[-6])
        # MATRIX with n rows and recycled sd and d
        res[] <- apply(matrix(c(rep(sd, length=n), rep(b, length=n)), ncol=2, nrow=n), 1,
          function(x) noiseFn(len=leng, sd=x[1], b=x[2], burn=burn, trunc=trunc,
            seed=seed))
      },
      "age" = {
        res <- apply(len, c(2:6),
          function(x) noiseFn(len=length(x), sd=sd, b=b, burn=burn, trunc=trunc, 
            seed=seed))
        res <- as.FLQuant(res, dimnames=dimnames(len))
      }
    )
    return(len + res)
  }
)

#' @rdname rnoise
setMethod("rnoise", signature(n='numeric', len="missing"),
  function(n=n, sd=1, b=0, burn=0, trunc=0, seed=NA) {
    return(noiseFn(len=n, sd=sd, b=b, burn=burn, trunc=trunc, seed=seed))
  }
)

#' @rdname rnoise
setMethod("rlnoise", signature(n='numeric', len="FLQuant"),
  function(n=n, len=len, sd=1, b=0, burn=0, trunc=0,
    what=c("year", "cohort", "age"), seed=NA) {
    return(exp(rnoise(n=n, len=len, sd=sd, b=b, burn=burn, trunc=trunc,
      what=what[1], seed=seed)))
  }
) # }}}

# noiseFn {{{
noiseFn <- function(len, sd=1, b=0, burn=0, trunc=0, seed=NA) {

  # set.seed by call if given
  if(!is.na(seed))
    set.seed(seed)

  # CHECK burn >= 0
  if (burn < 0)
    stop("burn must be >=0")
  
  # SET burn + 1
  burn <- burn + 1

  # OUTPUT vector, will hack off first values at the end
  x <- rep(0, len + burn)

  # Ranta and Kaitala 2001 Proc. R. Soc.
  s <- rnorm(len + burn, mean=0, sd=sd)

  for(i in (1:(len+burn-1))){
    x[i+1] <- b * x[i] + s[i] * sqrt(1 - b^2)
    if(trunc > 0){
      if (x[i+1] > (1 - trunc))  x[i+1] <- ( 1 - trunc)
      if (x[i+1] < (-1 + trunc)) x[i+1] <- (-1 + trunc)}
  }
  
  if (burn <= 0)
    return(x)
  
  x <- x[-(seq(burn))]
  
  return(x)
}# }}}

# Diagnostics

# mase {{{

#' Compute mean absolute scaled error (MASE)
#'
#' Franses, PH. "A note on the Mean Absolute Scaled Error". International Journal of Forecasting. 32 (1): 20–22. doi:10.1016/j.ijforecast.2015.03.008.
#'
#' @param ref Reference or naive prediction.
#' @param preds Predicitions to compare to reference.
#' @param order Are predictions in 'inverse' (default) or 'ahead' order.
#' @param ... Extra arguments.
#'
#' @return A numeric vector of the same length as 'preds'.

setGeneric("mase", function(ref, preds, ...) standardGeneric('mase'))

#' @rdname mase

setMethod("mase", signature(ref="FLQuant", preds="FLQuants"),
  function(ref, preds, order=c("inverse", "ahead")) {

    # SET dims
    fy <- dims(ref)$maxyear
    nyears <- length(preds)

    # REVERSE if ahead
    if(match.arg(order) == "ahead")
      preds <- preds[rev(seq(length(preds)))]

    # ADD names if missing
    if(is.null(names(preds)))
      names(preds) <- seq(fy - 1, fy - nyears)
    
    # \sum_{t=T-h+1}^{T} |\hat{y}_t - y_t|
    num  <- abs(log(mapply(function(x, y) x[, y], preds,
      y=ac(seq(fy, fy - nyears + 1)), SIMPLIFY=TRUE)) -
      log(ref[, ac(seq(fy, fy - nyears + 1))]))

    # \sum_{t=T-h+1}^{T} |y_t - y_{t-1}|
    den <- abs(log(ref[, ac(seq(fy, fy - nyears + 1))]) -
      log(ref[, ac(seq(fy - 1, fy - nyears))]))

    mase <- (1 / nyears * sum(num)) / (1 / nyears * sum(den))

    return(mase)
  }
)

#' @rdname mase
#' @param wt Mean weights-at-age to use with indices.

setMethod("mase", signature(ref="FLIndices", preds="list"),
  function(ref, preds, order="inverse", wt="missing") {

    # CHECK classes in list
    if(!all(unlist(lapply(preds, is, "FLIndices"))))
      stop("All elements in 'preds' list must be of class 'FLIndices'.")

    if(!all(unlist(lapply(preds, length)) == length(ref)))
      stop("'FLIndices' in 'preds' must have the same length as 'ref'.")

    # TODO CHECK names and warn if first matches last year of ref

    indices <- c(list(ref), preds)

    # TODO PARSE wt and add to indices

    res <- unlist(lapply(setNames(nm=names(indices[[1]])), function(i) {
      # COMPUTE index in biomass
      flqs <- lapply(indices, function(x) {
        if(is(x, "FLIndexBiomass"))
          index(x[[i]])
        else
          quantSums(index(x[[i]]) * catch.wt(x[[i]]))
      })
      mase(flqs[[1]], FLQuants(flqs[-1]), order=order)
    }))

    return(res)
  }
)
# }}}

# ar1rlnorm {{{

#' Generates a time series of possible bias-corrected lognormal autocorrelated random values
#'
#' Thorston, 2020.
#'
#' @param rho Autocorrelation coefficient.
#' @param years Vector of year names.
#' @param iters Number of iterations.
#' @param meanlog Mean of the series in log space.
#' @param sdlog Marginal standard deviation in log space.
#' @param bias.correct Should bias-correction be applied? Defaults to TRUE.
#'
#' @return An FLQuant object
#'
#' @author Iago Mosqueira (WMR), Henning Winker (JRC).
#' @seealso \link{rlnorm}
#' @keywords classes
#' @references  Thorson, J. T. Predicting recruitment density dependence and intrinsic growth rate for all fishes worldwide using a data-integrated life-history model. Fish Fish. 2020; 21: 237– 251. https://doi-org.ezproxy.library.wur.nl/10.1111/faf.12427
#' @examples
#' devs <- ar1rlnorm(rho=0.6, years=2000:2030, iter=500, meanlog=0, sdlog=1)
#' plot(devs)

ar1rlnorm <- function(rho, years, iters=1, meanlog=0, sdlog=1,
  bias.correct=TRUE, ...) {

  # DIMs
	n <- length(years)

  # BIAS correction
  logbias <- 0

  if(bias.correct)
    logbias <- 0.5 * c(sdlog) ^ 2

  rhosq <- c(rho) ^ 2

  #
  res <- matrix(rnorm(n * iters, mean=meanlog, sd=sdlog), nrow=n, ncol=iters)

	res <- apply(res, 2, function(x) {
		for(i in 2:n)
			x[i] <- rho * x[i-1] + sqrt(1 - rhosq) * x[i]
		return(exp(x - logbias))
	})

	return(FLQuant(array(res, dim=c(1,n,1,1,1,iters)),
		dimnames=list(year=years, iter=seq(1, iters), ...)))
}
# }}}

# rlnormar1 {{{

rlnormar1 <- function(n=NULL, meanlog=0, sdlog=1, rho=0, years,
  bias.correct=TRUE) {

  # SET iters
  if(is.null(n))
    n <- max(c(length(meanlog), length(sdlog), length(rho)))

  # NUMBER of years
  nyrs <- length(years)

  # REPEAT inputs to correct size
  rho <- rep(c(rho), length=n)
  meanlog <- rep(c(meanlog), length=n)
  sdlog <- rep(c(sdlog), length=n)

  res <- matrix(rnorm(n * nyrs, mean=meanlog, sd=sdlog),
    nrow=length(years), ncol=n)

  # BIAS correction
  logbias <- 0

  if(bias.correct)
    logbias <- 0.5 * sdlog ^ 2

  # RHOSQ
  rhosq <- rho ^ 2

  # FILL along years
  for(y in seq(nyrs)[-1])
    res[y, ] <- rho * res[y - 1, ] + sqrt(1 - rhosq) * res[y, ]

  # APPLY bias correction
  res <- exp(res - logbias)

  out <- FLQuant(c(res), dimnames=list(year=years, iter=seq(n)))

	return(out)
}

# }}}

# ar1deviances {{{
ar1deviances <- function(x, year) {

  rho <- rho(window(x, end=year))
  sdlog <- sqrt(yearVars(window(x, end=year)))

  x[, ac(seq(year + 1, dims(x)$maxyear))] <- rlnormar1(meanlog=0,
    sdlog=sdlog, rho=rho, years=ac(seq(year + 1, dims(x)$maxyear)))

  return(x)
}
# }}}

# runstest {{{

#' Computes Runs Test p-values
#'
#' @param fit The result of a model fit.
#' @param obs The observations used in the fit.
#' @param combine Should ages be combined by addition, defaults to TRUE.
#' @param ... Extra arguments.
#'
#' @return A list with elements 'p.values' and 'pass'.
    
setGeneric("runstest", function(fit, obs, ...)
  standardGeneric("runstest"))

#' @rdname runstest
#' @examples
#' data(nsher)
#' # Compute 'runstest' for FLSR fit
#' runstest(fit=fitted(nsher), obs=rec(nsher))
#' # Example runstest by age
#' data(ple4)
#' runstest(catch.n(ple4), landings.n(ple4), combine=FALSE)
#' runstest(fit=FLQuants(D=residuals(catch(ple4), discards(ple4)),
#'   L=residuals(catch(ple4), landings(ple4))))
#' runstest(fit=residuals(fitted(nsher), rec(nsher)))
#' runstest(FLQuants(residuals(fitted(nsher), rec(nsher))))
#' # Returns value per iter
#' runstest(fit=rnorm(25, residuals(fitted(nsher), rec(nsher)), 0.2))

setMethod("runstest", signature(fit="FLQuants", obs="missing"),
  function(fit, combine=TRUE) {

    # COMBINE
    if(combine) {
      fit <- lapply(fit, quantSums)
    }

    # APPLY over all iters in each element
    if(dim(fit[[1]])[6] > 1) {
      # By element
      return(lapply(fit, function(x) {
        cbind(iter=seq(length(x)), do.call(rbind,
          c(lapply(divide(x, 6), runstest), make.row.names=FALSE)))
      }))
    }
    
    # sigma3, by index
    if(combine) {
      s3s <- lapply(fit, sigma3)
    } else {
      s3s <- lapply(fit, function(x) {
        cbind(do.call(rbind, lapply(divide(x, 1),
          function(y) unlist(sigma3(y)))),
          data.frame(age=an(dimnames(x)$age)))
      })
    }
    # MERGE
    s3dat <- do.call(rbind, c(Map(function(x, y)
      cbind(x, qname=y), lapply(s3s, as.data.frame), names(s3s)),
      make.row.names = FALSE))
    
    # p.value >= 0.05 -> TRUE, green
    s3dat$pass <- s3dat$p.value >= 0.05

    # DROP qname if not used
    #if(length(unique(s3dat$qname)) == 1)
    #   s3dat$qname <- NULL

    return(s3dat)
  }
)

#' @rdname runstest

setMethod("runstest", signature(fit="FLQuants", obs="FLQuants"),
  function(fit, obs, combine=TRUE) {
    
    # COMBINE
    if(combine) {
      fit <- lapply(fit, quantSums)
      obs <- lapply(obs, quantSums)
    }
    
    # RESIDUALS
    res <- FLQuants(Map(residuals, obs, fit, type="log"))
    
    return(runstest(res, combine=combine))
  }
)

#' @rdname runstest

setMethod("runstest", signature(fit="FLQuant", obs="FLQuant"),
  function(fit, obs, combine=TRUE) {
    
    fit <- FLQuants(A=fit)
    obs <- FLQuants(A=obs)
    
    return(runstest(fit, obs, combine=combine))
  }
)

#' @rdname runstest
#' @examples
#' runstest(ssb(nsher))
#' runstest(rnorm(1, FLQuant(1, dimnames=list(year=1973:2021))))

setMethod("runstest", signature(fit="FLQuant", obs="missing"),
  function(fit, combine=TRUE) {
    return(runstest(FLQuants(A=fit), combine=combine))
  }
)

#' @rdname runstest
#' @examples
#' runstest(rep(0.1, 10), cumsum(rnorm(10, 0.1, 0.01)))

setMethod("runstest", signature(fit="numeric", obs="numeric"),
  function(fit, obs, combine=TRUE) {
    return(runstest(fit=FLQuants(FLQuant(fit)),
      obs=FLQuants(FLQuant(obs)), combine=combine))
  }
)

#' @rdname runstest
#' @examples
#' runstest(rnorm(10, 0, 0.1))

setMethod("runstest", signature(fit="numeric", obs="missing"),
  function(fit, obs, combine=TRUE) {
    return(runstest(fit=FLQuants(FLQuant(fit)), combine=combine))
  }
)

#' @rdname runstest

# }}}

# sigma3 (FLQuant) {{{

#' Compute the 3-sigma limits and the corresponding p-value
#'
#' @param x An object of class FLQuant.
#' @param mixing Alternative hypothesis to be tested. One of "two.sided", "less" (default) or "greater".
#'
#' @return A list with elements 'lcl', 'ucl' and 'p.value'.
#'
#' @examples
#' data(ple4)
#' sigma3(catch.n(ple4))
#' data(nsher)
#' sigma3(residuals(nsher))

sigma3 <- function(x, mixing="less", type="residual") {

  # COMPUTE average moving rate

  mr <- abs(diff(x - 0))
  amr <- mean(mr, na.rm=TRUE)
  
  # COMPUTE upper limit for moving ranges

  ulmr <- 3.267 * amr

  # REMOVE moving ranges greater than ulmr and recalculate amr, Nelson 1982

  mr  <- c(mr)[c(mr) < ulmr]
  amr <- mean(mr, na.rm = TRUE)

  # Calculate standard deviation, Montgomery, 6.33

  stdev <- amr / 1.128

  # Calculate control limits
  lcl <- -3 * stdev
  ucl <- 3 * stdev

  # SET alternative
  alternative <- c("two.sided", "left.sided")[which(c("two.sided", "less") %in% mixing)]

  if(nlevels(factor(sign(x))) > 1) {

    # Make the runs test non-parametric
    y <- ifelse(x < 0, -1, 1)
    
    #
    runstest <- .runs.test(c(y), threshold = 0, alternative = alternative)
    
    if(is.na(runstest$p.value)) {
      p.value <- 0.001 
    } else {
      pvalue <- round(runstest$p.value, 3)
    }

  } else {
    pvalue <- 0.001
  }
 
  return(list(lcl = lcl, ucl = ucl, p.value = pvalue))
}

#

.runs.test <- function(x, alternative="two.sided", threshold=median(x), pvalue="normal", plot=FALSE){

  # Performs the Runs Test for Randomness.
  #
  # Args:
  #   x: a numeric vector containing the data.
  #   alternative hypothesis, must be one of "two.sided" (default), "left.sided" or "right.sided"
  #   threshold: 
  #
  # Returns:
  #   statistic: the (normalized) value of the statistic test.
  #   n: the sample size, after the remotion of consecutive duplicate values.
  #   p.value: the asymptotic p-value.
  #
druns <- function(x, n1, n2, log = FALSE){
  stopifnot(is.numeric(x))
  x <- ifelse(x == round(x),x,1)
  r0 <- ifelse(x %% 2==0, 2*choose(n1-1, round(x/2)-1)*choose(n2-1, round(x/2)-1), 
             choose(n1-1, round((x-1)/2))*choose(n2-1, round((x-3)/2))+choose(n1-1, round((x-3)/2))*choose(n2-1, round((x-1)/2)))  
  r0<-r0/choose(n1+n2, n1)
# if TRUE, probabilities p are given as log(p).  
ifelse(log,return(log(r0)),return(r0))  
}

  dname <- deparse(substitute(x))
  if (alternative == "t"){alternative <- "two.sided"} 
  if (alternative == "l"){alternative <- "left.sided"}
  if (alternative == "r"){alternative <- "right.sided"}    
  if (alternative != "two.sided" & alternative != "left.sided" & alternative != "right.sided")
    {stop("must give a valid alternative")}
  # Remove NAs
  x <- na.omit(x)
  stopifnot(is.numeric(x))
  # Remove values equal to the level
  x <- x[x!=threshold]
  s <- sign(x-threshold)
  n1 <- length(s[s>0]) 
  n2 <- length(s[s<0])
  runs <- rle(s)
  r1 <- length(runs$lengths[runs$values==1])
  r2 <- length(runs$lengths[runs$values==-1])  
  n <- n1+n2
  mu <- 1 + 2*n1*n2/(n1+n2)
  vr <- 2*n1*n2*(2*n1*n2-n1-n2)/(n^2*(n-1))
  rr <- r1+r2
  #
  #
  # Computes the p-value
  pv <- 0
  if (pvalue == "exact"){    
    if (alternative=="two.sided"){
      pv1<-sum(druns(1:rr,n1,n2))
      pv2<-sum(druns(rr:(n1+n2),n1,n2))
      pv <- 2*min(pv1,pv2)
    }
    if (alternative=="left.sided"){pv<-sum(druns(2:rr,n1,n2))}
    if (alternative=="right.sided") {pv<-sum(druns(rr:(n1+n2),n1,n2))}    
  }
  if (pvalue=="normal"){
    pv0 <- pnorm((rr - mu) / sqrt(vr))
    if (alternative=="two.sided"){pv <- 2*min(pv0,1-pv0)}
    if (alternative=="left.sided"){pv <- pv0}
    if (alternative=="right.sided") {pv <- 1-pv0}
  }  
  if (alternative=="two.sided"){alternative<-"nonrandomness"}
  if (alternative=="left.sided"){alternative<-"trend"}
  if (alternative=="right.sided") {alternative<-"first-order negative autocorrelation"}
  #
  rval <- list(statistic = c(statistic=(rr - mu) / sqrt(vr)), p.value = pv, runs=rr, mu=mu, var=vr,  
               method = "Runs Test", data.name = dname, parameter=c(runs=rr, n1=n1,n2=n2,n=n), alternative=alternative)  
  class(rval) <- "htest"
  return(rval)
}

# }}}

# roc {{{

#' Receiver Operating Characteristic (ROC)
#'
#' A receiver operating characteristic (ROC) curve shows the ability of a
#' binary classifier. Here it is applied to compare two sets of values,
#' stored as two FLQuant objects. The first is the result of aplying a logical
#' comparison of a given state against a reference value, so it contains a 
#' binary (0, 1) label. The second, the score, contains an alternative metric
#' that attempts to measure the absolute value of the first.
#' The examples below compare an observation of stock status, SSB being less 
#' than a reference point, and an alternative metric, here the catch curve 
#' estimates of total mortality.
#'
#' @examples
#' data(ple4)
#' # OM 'reality' on stock status (fbar)
#' state <- fbar(ple4)[, ac(1960:2017)]
#' # Model estimates of F using catch curves
#' ind <- acc(catch.n(ple4)[, ac(1960:2017)])
#' # Compute TSS, returns data.frame
#' roc(state >= 0.22, ind)
#' # Needs ggplotFL
#' \dontrun{
#' ggplot(roc(state >= 0.22, ind, direction='>='), aes(x=FPR, y=TPR)) +
#'   geom_line() +
#'   geom_abline(slope=1, intercept=0, colour="red", linetype=2)
#' }

roc <- function(label, ind, direction=c(">=", "<=")) {

  # CHECK label is logical or pseudo-logical (0, 1)
  if(!all(c(label) %in% c(0, 1)))
    stop("label can only contain 0 and 1 for FALSE, TRUE")
 
  # CONVERT label to pseudo-logical (0/1)
  if(!is.numeric(label))
    label[] <- ifelse(c(label), 1, 0)

  # PROPAGATE if needed
  its <- max(dim(label)[6], dim(ind)[6])
  label <- propagate(label, its)
  ind <- propagate(ind, its)

  # CHECK dims match
  if(!all.equal(dim(label), dim(ind)))
    stop("dimensions of label and ind must match")

  # CHECK label not all TRUE/FALSE
  if(all(label == 1) | all(label == 0))
    stop("label cannot be all TRUE or FALSE")

  # SET comparison direction
  direction <- switch(match.arg(direction), ">=" =  FALSE, "<=" = TRUE)

  # ORDER by descending ind
  idx <- rev(order(c(ind), decreasing=direction))
  label <- c(label)[idx]
 
  # CALCULATE TRUE and FALSE positives
  tp <- cumsum(label)
  fp <- cumsum(!label)
  
  # CALCULATE TRUE and FALSE negatives
  tn <- sum(!label) - fp  
  fn <- sum(label) - tp  

  # CALCULATE TRUE and FALSE positive rates
  tpr <- cumsum(label) / sum(label) 
  fpr <- cumsum(!label) / sum(!label)
  
  # COMPUTE True Skill Score
  tss  <- (tp / (tp + fn) - fp / (fp + tn))

  # CONSTRUCT output data.frame
  out <- model.frame(FLQuants(ind=ind), drop=TRUE)[idx,]

  res <- cbind(out, label=label, TP=tp, TN=tn,
    FP=fp, FN=fn, TPR=tpr, FPR=fpr, TSS=tss)

  return(res)
}
# }}}

# auc {{{

#' Area under the curve
#' The area under the ROC (auc, Area under the Curve), is calculated from
#' the true and false positive rates (TPR and FPR). The two columns
#' returned by the `roc()` function with those names can be passed on to this 
#' function.
#' @rdname roc
#' @examples
#' # Computes auc using the output of roc()
#' with(roc(state >= 0.22, ind), auc(TPR=TPR, FPR=FPR))
#' auc(roc(state >= 0.22, ind))

auc <- function(x=NULL, TPR=x$TPR, FPR=x$FPR){

  dFPR <- c(diff(FPR), 0)
  dTPR <- c(diff(TPR), 0)
  
  sum(TPR * dFPR) + sum(dTPR * dFPR)/2
}
# }}}
