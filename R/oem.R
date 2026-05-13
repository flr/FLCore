# oem.R - DESC
# FLCore/R/oem.R

# Copyright European Union, 2017
# Author: Iago Mosqueira (EC JRC) <iago.mosqueira@ec.europa.eu>

# Observation

# survey {{{

#' Generate simulated survey observations of abundance
#'
#' Creates a simulated observation of abundance-at-age or biomass index from
#' an \code{FLStock} object representing the operating model (OM), replicating
#' the sampling process of a scientific survey. The result is returned as an
#' \code{FLIndex} or \code{FLIndexBiomass} object with the \code{index} slot
#' populated.
#'
#' @param object An \code{FLStock} operating model from which to draw the survey.
#' @param index An \code{FLIndex}, \code{FLIndexBiomass}, or \code{FLIndices}
#'   defining the survey design; if missing, a generic survey is constructed.
#' @param sel Selectivity-at-age pattern to apply; defaults to
#'   \code{sel.pattern(index)}.
#' @param ages Character vector of ages to include; defaults to all ages in
#'   \code{index}.
#' @param timing Fraction of the year at which the survey takes place (0--1);
#'   defaults to the midpoint of the \code{startf}/\code{endf} range entries.
#' @param index.q Catchability of the survey; defaults to \code{index@index.q}.
#' @param stability Hyperstability/hyperdepletion exponent applied to abundance;
#'   1 (default) gives a linear response.
#' @param biomass Logical; if \code{TRUE} and \code{index} is missing, returns
#'   an \code{FLIndexBiomass} object; otherwise an \code{FLIndex}.
#' @param catch.wt Mean weight-at-age used to convert numbers to biomass for
#'   \code{FLIndexBiomass} surveys; defaults to \code{index@catch.wt}.
#' @param ... Additional arguments passed to methods.
#'
#' @return An \code{FLIndex} or \code{FLIndexBiomass} object with the
#'   \code{index} slot populated by the simulated survey observations, or a
#'   list of such objects when \code{index} is of class \code{FLIndices}.
#'
#' @name survey
#' @rdname survey
#' @aliases survey survey-methods
#' @docType methods
#' @section Generic function: survey(object, index, ...)
#'
#' @author The FLR Team
#' @seealso \linkS4class{FLIndex}, \linkS4class{FLIndexBiomass},
#'   \linkS4class{FLStock}, \link{cpue}, \link{index}
#' @keywords methods

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
    # catch.n(index) <- expand(abnd, age=dimnames(index)$age, fill=FALSE)

    # COMPUTE biomass index
    index(index) <- c(unitSums(quantSums(abnd *
      catch.wt[ages, dimnames(index)$year] %*% index.q)) ^ stability)

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

#' Compute an abundance index from an FLStock
#'
#' Computes an abundance-at-age index from an \code{FLStock} object at a
#' given survey timing, correcting stock numbers for the combined effect of
#' fishing and natural mortality up to that point in the year.
#'
#' @param object An \code{FLStock} object from which to compute the index.
#' @param sel Selectivity-at-age pattern to apply; defaults to
#'   \code{catch.sel(object)}.
#' @param ages Character vector of ages to include; defaults to all ages in
#'   \code{sel}.
#' @param timing Fraction of the year at which the index is computed (0--1);
#'   defaults to 0.5 (mid-year).
#'
#' @return An \code{FLQuant} of abundance-at-age corrected for mortality up
#'   to \code{timing} and filtered to the selected ages.
#'
#' @rdname index
#' @author The FLR Team
#' @seealso \linkS4class{FLStock}, \link{survey}
#' @keywords methods
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

#' Compute a CPUE index of abundance from an FLStock
#'
#' The observation of stock abundance by CPUE series from commercial fleets is an
#' important step in the generation of management advice that needs to be replicated
#' on an Operating Model during any simulation exercise. This method generates
#' an observation of biomass or numbers-at-age from an \code{FLStock} being used as OM.
#'
#' @param object An \code{FLStock} from which to generate the CPUE observation.
#' @param index An optional index object (currently unused for the
#'   \code{FLStock, missing} method).
#' @param sel.pattern Selectivity pattern to apply; defaults to
#'   \code{harvest(object)}.
#' @param effort Units of effort used to mimic the effort series in the
#'   fishery: \code{"f"} (default, fishing mortality), \code{"hr"} (harvest
#'   rate), or a numeric value.
#' @param biomass Logical; if \code{TRUE} (default) the index is summed across
#'   ages and weighted by catch weight-at-age to give a biomass CPUE.
#' @param ... Additional arguments passed to methods.
#'
#' @return An \code{FLQuant} containing the CPUE index: age-disaggregated
#'   if \code{biomass = FALSE}, or aggregated biomass CPUE if
#'   \code{biomass = TRUE}.
#'
#' @name cpue
#' @rdname cpue
#' @aliases cpue cpue-methods
#' @docType methods
#' @section Generic function: cpue(object, index, ...)
#'
#' @author The FLR Team
#' @seealso \link{survey}, \linkS4class{FLStock}
#' @keywords methods
#' @examples
#'
#' data(ple4)
#' 
#' cpue(ple4)
#' # An aggregated biomass CPUE
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

#' Apply hyperstability or hyperdepletion to an abundance index
#'
#' Modifies an abundance index to exhibit hyperstability (apparent abundance
#' remains high even as true abundance declines) or hyperdepletion (apparent
#' abundance drops faster than true abundance) relative to a reference level.
#' The transformation applied is
#' \eqn{I^* = I_{\rm ref} \cdot (I / I_{\rm ref})^\omega}, where
#' \eqn{\omega < 1} produces hyperstability and \eqn{\omega > 1} produces
#' hyperdepletion.
#'
#' @param object An \code{FLQuant} abundance index to transform.
#' @param omega Numeric scalar; exponent controlling the degree of
#'   hyperstability (\eqn{\omega < 1}) or hyperdepletion (\eqn{\omega > 1}).
#'   Defaults to 1 (no transformation).
#' @param ref Reference level of the index; defaults to the year means of
#'   \code{object}.
#'
#' @return An \code{FLQuant} of the same dimensions as \code{object} with the
#'   hyperstability/hyperdepletion transformation applied.
#'
#' @author The FLR Team
#' @seealso \link{survey}, \link{cpue}
#' @keywords utilities

hyperstability <- function(object, omega=1, ref=yearMeans(object)) {
  return(ref %*% ((object %/% ref) ^ omega))
} # }}}

# computeQ(FLIndices, FLStock, FLQuants) {{{

#' Compute catchability from model fit and observations
#'
#' Estimates the catchability coefficient (\eqn{Q}) for one or more survey
#' indices given an \code{FLStock} operating model and the fitted index values
#' from a stock assessment. The calculation accounts for survey timing,
#' correcting stock numbers for total mortality (\eqn{Z}) at the time of the
#' survey.
#'
#' @param indices An \code{FLIndices} or \code{FLI} (the virtual base class
#'   shared by \code{FLIndex} and \code{FLIndexBiomass}) object containing the
#'   survey index definitions (timing, selectivity pattern, etc.).
#' @param stock An \code{FLStock} operating model providing stock numbers and
#'   weight-at-age.
#' @param fit An \code{FLQuants} or \code{FLQuant} of fitted index values from
#'   the stock assessment model.
#' @param ... Additional arguments (unused).
#'
#' @return An \code{FLQuants} or \code{FLQuant} of estimated catchability
#'   coefficients, one per index (or age for biomass indices).
#'
#' @name computeQ
#' @rdname computeQ
#' @aliases computeQ computeQ-methods
#' @docType methods
#' @section Generic function: computeQ(indices, stock, fit, ...)
#'
#' @author The FLR Team
#' @seealso \link{survey}, \linkS4class{FLIndex}, \linkS4class{FLStock}
#' @keywords methods
#' @examples
#' # Load datasets
#' data(ple4)
#' data(ple4.indices)
#' # Compute for each index and pseudo-noisy fit
#' pseudofit <- lapply(ple4.indices, function(x) index(x) * 0.8)
#' computeQ(ple4.indices, ple4, pseudofit)

setMethod("computeQ", signature=c(indices="FLIndices", stock="FLStock",
  fit="FLQuants"), function(indices, stock, fit) {

  # SET iterMedians for stock
  yrs <- dimnames(stock)$year

  # LOOP over indices
  res <- Map(function(x, y) {

    # GET mean index timing and dimnames
    t <- mean(range(x)[c("startf", "endf")])

    # DEFAULTS to 0.5
    if(is.na(t)) t <- 0.5

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

      iq <- y %/% biy
    }

    return(iq)

    }, x=indices, y=fit)

  return(FLQuants(res))
  }
)

#' @rdname computeQ
#' @examples
#' #  Compute for single index and pseudo-noisy fit
#' computeQ(ple4.indices[[1]], ple4, rlnorm(1, log(index(ple4.indices[[1]])), 0.1))

setMethod("computeQ", signature=c(indices="FLI", stock="FLStock",
  fit="FLQuant"), function(indices, stock, fit) {
    
    res <- computeQ(indices=FLIndices(A=indices), stock=stock, 
      fit=FLQuants(A=fit))

    return(res[[1]])

  }
)
# }}}

# Noise

# bias {{{

#' Compute a cumulative multiplicative bias trend
#'
#' Computes a cumulative multiplicative bias factor that increases (or
#' decreases) systematically over the years in an \code{FLQuant}. Starting
#' from 1, each subsequent year is multiplied by \code{1 + bias}, producing
#' a trend that can represent, e.g., a gradually increasing observation bias.
#'
#' @param object An \code{FLQuant} whose year dimension and dimnames are used.
#' @param bias Numeric; per-year fractional bias; defaults to 0.02 (2\% per
#'   year).
#'
#' @return An \code{FLQuant} with the same dimnames as \code{object}
#'   containing the cumulative bias multipliers.
#'
#' @author The FLR Team
#' @seealso \link{biased}
#' @keywords utilities

bias <- function(object, bias=0.02){
  return(FLQuant(cumprod(1 + rep(c(bias), dim(object)[2])), dimnames=dimnames(object)))
}

#' Apply a systematic bias trend to an abundance index
#'
#' Multiplies an \code{FLQuant} by a cumulative bias trend generated by
#' \link{bias}, so that values drift away from truth over time.
#'
#' @param object An \code{FLQuant} to be biased.
#' @param bias Numeric; per-year fractional bias passed to \link{bias};
#'   defaults to 0.02.
#'
#' @return An \code{FLQuant} of the same dimensions as \code{object} with the
#'   cumulative bias applied.
#'
#' @author The FLR Team
#' @seealso \link{bias}
#' @keywords utilities

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
#' @author The FLR Team
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

#' Generate an autocorrelated noise series
#'
#' Internal helper that generates a numeric vector of autocorrelated noise
#' following the AR(1) process described by Ranta and Kaitala (2001):
#' \eqn{v_t = b \cdot v_{t-1} + s_t \sqrt{1 - b^2}}, where
#' \eqn{s_t \sim N(0, \sigma^2)}.
#'
#' @param len Integer; length of the output vector (after burn-in removal).
#' @param sd Numeric; standard deviation of the innovations; defaults to 1.
#' @param b Numeric; autocorrelation parameter in \eqn{[-1, 1]}; 0 gives
#'   white noise; defaults to 0.
#' @param burn Integer; number of initial values to discard as burn-in;
#'   defaults to 0.
#' @param trunc Numeric; if > 0, values outside
#'   \eqn{(-(1 - \mathrm{trunc}), 1 - \mathrm{trunc})} are truncated;
#'   defaults to 0 (no truncation).
#' @param seed Integer or \code{NA}; random seed passed to \code{set.seed};
#'   if \code{NA} (default) the seed is not set.
#'
#' @return A numeric vector of length \code{len} containing the simulated
#'   autocorrelated deviates.
#'
#' @author The FLR Team
#' @references Ranta, E. and Kaitala, V. (2001). Travelling waves in vole
#'   population dynamics. \emph{Proceedings of the Royal Society of London.
#'   Series B: Biological Sciences}, 268(1474), 1595--1600.
#' @seealso \link{rnoise}, \link{rlnoise}
#' @keywords internal

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
#' Computes the Mean Absolute Scaled Error (MASE) between a reference
#' (naive) prediction and one or more alternative predictions.
#' MASE is scale-independent and robust to outliers, making it useful for
#' comparing forecast accuracy across different indices or time series.
#'
#' @references Franses, P.H. (2016). A note on the Mean Absolute Scaled
#'   Error. \emph{International Journal of Forecasting}, 32(1):20--22.
#'   \doi{10.1016/j.ijforecast.2015.03.008}.
#'
#' @param ref Reference or naive prediction, an \code{FLQuant} or
#'   \code{FLIndices} time series.
#' @param preds Predictions to compare to the reference; an \code{FLQuants}
#'   or a list of \code{FLIndices}.
#' @param order Character; whether predictions are in \code{"inverse"}
#'   (default, most recent first) or \code{"ahead"} order.
#' @param wt Mean weights-at-age to use when converting index numbers to
#'   biomass (only for the \code{FLIndices, list} method).
#' @param ... Additional arguments passed to methods.
#'
#' @return A numeric scalar (or named numeric vector for the
#'   \code{FLIndices, list} method) giving the MASE value(s).
#'
#' @name mase
#' @rdname mase
#' @aliases mase mase-methods
#' @docType methods
#' @section Generic function: mase(ref, preds, ...)
#'
#' @author The FLR Team
#' @seealso \link{runstest}
#' @keywords methods

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

#' Generate an AR(1) lognormal FLQuant time-series (deprecated)
#'
#' @description
#' Deprecated. Please use \link{rlnormar1} instead.
#'
#' @param rho Numeric; AR(1) autocorrelation parameter in \eqn{[-1, 1]}.
#' @param years Integer or character vector of years for the time dimension.
#' @param iters Integer; number of iterations to generate; defaults to 1.
#' @param meanlog Numeric; mean of the normal (log) distribution; defaults to
#'   0.
#' @param sdlog Numeric; standard deviation of the normal (log) distribution;
#'   defaults to 1.
#' @param bias.correct Logical; if \code{TRUE} apply log-normal bias
#'   correction; defaults to \code{FALSE}.
#' @param ... Additional arguments (unused).
#'
#' @return An \code{FLQuant} with dimensions
#'   \code{1 x length(years) x 1 x 1 x 1 x iters}.
#'
#' @author The FLR Team
#' @seealso \link{rlnormar1}
#' @keywords utilities

ar1rlnorm <- function(rho, years, iters=1, meanlog=0, sdlog=1,
  bias.correct=FALSE, ...) {

  .Deprecated(rlnormar1, package='FLCore',
    msg=message("This function will soon be deprecated. Please use 'rlnormar1'"))

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

#' Generate an AR(1) lognormal FLQuant time-series
#'
#' Simulate one or more iterated log-normal AR(1) series across specified
#' years and return the results as an FLQuant.
#'
#' The function first draws independent normal samples with given
#' `meanlog` and `sdlog`, constructs an AR(1) process with correlation
#' parameter `rho`, and finally exponentiates the series. If
#' `bias.correct = TRUE` the log-normal bias (0.5 * sdlog^2) is removed
#' on the log-scale so that the resulting series have the requested
#' `meanlog` on the log-scale.
#'
#' @param n Integer. Number of iterations to generate. If `NULL` (default)
#'   `n` is set to the maximum length of `meanlog`, `sdlog` and `rho`.
#' @param meanlog Numeric. Mean(s) of the normal (log) distribution for the
#'   innovations. Recycled to length `n` if necessary.
#' @param sdlog Numeric. Standard deviation(s) of the normal (log)
#'   distribution for the innovations. Recycled to length `n` if necessary.
#' @param rho Numeric. AR(1) autocorrelation parameter(s) in [-1, 1].
#'   Recycled to length `n` if necessary.
#' @param years Integer or character vector. Years (time dimension) for the
#'   returned FLQuant (length determines number of time steps).
#' @param bias.correct Logical. If TRUE (default FALSE) subtract
#'   0.5 * sdlog^2 from the log-series before exponentiation to correct
#'   for the lognormal bias.
#'
#' @return An object of class FLQuant with dimensions year x iter containing
#'   the simulated log-normal AR(1) series.
#'
#' @details The simulation proceeds as follows:
#'   1. Draw independent normal innovations: eps_{t,i} ~ N(meanlog_i, sdlog_i^2).
#'   2. Build the AR(1) process: x_{t,i} = rho_i * x_{t-1,i} + sqrt(1 - rho_i^2) * eps_{t,i},
#'      with x_{1,i} = eps_{1,i}.
#'   3. Optionally apply bias correction on the log scale, then exponentiate:
#'      y_{t,i} = exp(x_{t,i} - 0.5 * sdlog_i^2)  (if bias.correct = TRUE).
#'
#' @author The FLR Team
#'
#' @examples
#' # 6 years, 5 iterations, rho = 0.5, default meanlog/sdlog
#' rlnormar1(n = 5, rho = 0.5, years = 2000:2005)
#'
#' # varying sdlog per iteration
#' rlnormar1(n = 3, sdlog = c(0.5, 1, 1.5), rho = 0.3, years = 1990:1994)
#'
#' @seealso \link{rlnorm}
#' @keywords statistics
#' @references  Thorson, J. T. Predicting recruitment density dependence and intrinsic growth rate for all fishes worldwide using a data-integrated life-history model. Fish Fish. 2020; 21: 237– 251. https://doi-org.ezproxy.library.wur.nl/10.1111/faf.12427


rlnormar1 <- function(n=NULL, meanlog=0, sdlog=1, rho=0, years,
  bias.correct=FALSE) {

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
    nrow=length(years), ncol=n, byrow=TRUE)

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

#' Extend AR(1) log-normal deviances beyond a given year
#'
#' Replaces the values of an \code{FLQuant} beyond a specified year with
#' simulated AR(1) log-normal deviates, using the autocorrelation and
#' standard deviation estimated from the historical period up to and
#' including that year.
#'
#' @param x An \code{FLQuant} of log-normal deviances with historical data
#'   up to at least \code{year}.
#' @param year Integer or character; the last year of the historical period.
#'   Values in \code{x} after this year will be replaced by simulations.
#'
#' @return An \code{FLQuant} of the same dimensions as \code{x}, with years
#'   after \code{year} replaced by AR(1) log-normal simulations estimated
#'   from the historical data.
#'
#' @author The FLR Team
#' @seealso \link{rlnormar1}
#' @keywords utilities

ar1deviances <- function(x, year) {

  rho <- rho(window(x, end=year))
  sdlog <- sqrt(yearVars(window(x, end=year)))

  x[, ac(seq(year + 1, dims(x)$maxyear))] <- rlnormar1(meanlog=0,
    sdlog=sdlog, rho=rho, years=ac(seq(year + 1, dims(x)$maxyear)))

  return(x)
}
# }}}

# runstest {{{

#' Compute runs test p-values for residual randomness
#'
#' Applies the non-parametric runs test to assess whether residuals from a
#' stock assessment model fit are randomly distributed around zero (i.e. no
#' systematic trend or autocorrelation). A p-value < 0.05 indicates
#' significant non-randomness. The function accepts residuals directly, or
#' computes log-residuals from a pair of fit and observations.
#'
#' @param fit Residuals or model-fitted values; an \code{FLQuant},
#'   \code{FLQuants}, or \code{numeric} vector.  When \code{obs} is also
#'   provided, \code{fit} and \code{obs} are used to compute log-residuals.
#' @param obs Observations corresponding to \code{fit}; an \code{FLQuant},
#'   \code{FLQuants}, or \code{numeric} vector.  If missing, \code{fit} is
#'   treated directly as residuals.
#' @param combine Logical; if \code{TRUE} (default) ages are summed before
#'   the test is applied.
#' @param ... Additional arguments passed to methods.
#'
#' @return A \code{data.frame} with columns \code{lcl}, \code{ucl},
#'   \code{p.value}, \code{pass} (TRUE if p-value >= 0.05), and \code{qname},
#'   one row per index or iteration.
#'
#' @name runstest
#' @rdname runstest
#' @aliases runstest runstest-methods
#' @docType methods
#' @section Generic function: runstest(fit, obs, ...)
#'
#' @author The FLR Team
#' @seealso \link{sigma3}
#' @keywords methods
    
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

# }}}

# sigma3 (FLQuant) {{{

#' Compute 3-sigma control limits and a runs test p-value
#'
#' Computes the 3-sigma control limits (lower and upper) for an
#' \code{FLQuant} time series using the average moving range method
#' (Montgomery, 2009), and performs a non-parametric runs test to assess
#' whether the series shows a systematic trend.
#'
#' @param x An \code{FLQuant} time series (typically residuals).
#' @param mixing Character; alternative hypothesis for the runs test.  One
#'   of \code{"two.sided"}, \code{"less"} (default, tests for trend), or
#'   \code{"greater"}.
#' @param type Character; reserved for future use; currently ignored.
#'
#' @return A named list with elements:
#'   \describe{
#'     \item{lcl}{Lower 3-sigma control limit (negative).}
#'     \item{ucl}{Upper 3-sigma control limit (positive).}
#'     \item{p.value}{p-value from the runs test; values >= 0.05 suggest
#'       acceptable randomness.}
#'   }
#'
#' @name sigma3
#' @rdname sigma3
#' @author The FLR Team
#' @seealso \link{runstest}, \link{.runs.test}
#' @keywords utilities
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

#' @title Runs test for randomness
#'
#' @description Internal implementation of the runs test for randomness.
#'   Performs the runs test against a threshold value, returning a list of
#'   class \code{"htest"} with the test statistic, p-value, and related
#'   diagnostics. This function is called internally by \link{sigma3}.
#'
#' @param x A numeric vector containing the data.
#' @param alternative Character; the alternative hypothesis.  One of
#'   \code{"two.sided"} (default), \code{"left.sided"}, or
#'   \code{"right.sided"} (abbreviations \code{"t"}, \code{"l"},
#'   \code{"r"} are accepted).
#' @param threshold Numeric; the threshold used to dichotomise \code{x}
#'   into above/below; defaults to \code{median(x)}.
#' @param pvalue Character; method to compute p-values; \code{"normal"}
#'   (default, normal approximation) or \code{"exact"} (exact distribution).
#' @param plot Logical; reserved for future use; currently ignored.
#'
#' @return A list of class \code{"htest"} with elements:
#'   \describe{
#'     \item{statistic}{Normalised test statistic.}
#'     \item{p.value}{Asymptotic (or exact) p-value.}
#'     \item{runs}{Total number of runs.}
#'     \item{mu}{Expected number of runs under the null.}
#'     \item{var}{Variance of the number of runs under the null.}
#'     \item{method}{Character description of the test.}
#'     \item{data.name}{Name of the data object.}
#'     \item{parameter}{Named vector with runs, n1, n2, n.}
#'     \item{alternative}{Description of the alternative hypothesis.}
#'   }
#'
#' @author The FLR Team
#' @seealso \link{sigma3}, \link{runstest}
#' @keywords internal

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

#' Receiver Operating Characteristic table with True Skill Statistic (TSS)
#'
#' A receiver operating characteristic (ROC) curve shows the ability of a
#' binary classifier. Here it is applied to compare two sets of values,
#' stored as two FLQuant objects. The first is the result of applying a logical
#' comparison of a given state against a reference value, so it contains a
#' binary (0, 1) label. The second, the score, contains an alternative metric
#' that attempts to measure the absolute value of the first.
#' The examples below compare an observation of stock status, SSB being less
#' than a reference point, and an alternative metric, here the catch curve
#' estimates of total mortality.
#'
#' @param label Logical, integer (0/1), or \code{FLQuant} giving the true
#'   class for each observation (1 = positive, 0 = negative). Non-logical
#'   values are coerced to 0/1. Labels must not be all 0 or all 1.
#' @param ind Numeric vector or \code{FLQuant} of indicator / score values
#'   used to rank observations.
#' @param direction Character scalar, one of \code{">="} (default) or
#'   \code{"<="}. If \code{">="}, larger \code{ind} values are treated as
#'   more evidence for the positive class; if \code{"<="}, smaller
#'   \code{ind} values are treated as more evidence for the positive class.
#'
#' @details
#' When \code{label} and \code{ind} are \code{FLQuant} objects the function
#' will propagate them along the 6th dimension if needed. The function checks
#' that \code{label} contains only 0/1 and that both arguments have matching
#' dimensions. Observations are ordered according to \code{ind} (respecting
#' \code{direction}) and cumulative counts and rates are computed.
#'
#' @return A \code{data.frame} sorted by the chosen threshold order containing
#'   the columns:
#'   \describe{
#'     \item{ind}{indicator / score values}
#'     \item{label}{coerced 0/1 label}
#'     \item{TP, TN, FP, FN}{cumulative true/false positive/negative counts}
#'     \item{TPR, FPR}{true positive rate and false positive rate}
#'     \item{TSS}{True Skill Statistic, computed as TPR - FPR
#'       (i.e. tp/(tp+fn) - fp/(fp+tn))}
#'   }
#'
#' @name roc
#' @rdname roc
#' @author The FLR Team
#' @seealso \link{auc}
#' @keywords utilities
#' @examples
#' data(ple4)
#' # OM 'reality' on stock status (fbar)
#' state <- fbar(ple4)[, ac(1960:2017)]
#' # Model estimates of F using catch curves
#' ind <- acc(catch.n(ple4)[, ac(1960:2017)])
#' # Compute TSS, returns data.frame
#' roc(state >= 0.22, ind)
#' # Needs ggplot2
#' \dontrun{
#' ggplot(roc(state >= 0.22, ind, direction='>='), aes(x=FPR, y=TPR)) +
#'   geom_line() +
#'   geom_abline(slope=1, intercept=0, colour="red", linetype=2)
#' }

roc <- function(label, ind, direction=c(">=", "<=")) {

  # CONVERT label to pseudo-logical (0/1)
  if(is.logical(label))
    label[] <- ifelse(c(label), 1, 0)

  # CHECK label is logical or pseudo-logical (0, 1)
  if(!all(c(label) %in% c(0, 1)))
    stop("label can only contain 0 and 1 for FALSE, TRUE")
 
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
  direction <- switch(match.arg(direction), ">=" =  TRUE, "<=" = FALSE)

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

#' Area under the ROC curve
#'
#' Computes the area under the receiver operating characteristic curve (AUC)
#' using the trapezoidal rule applied to the true positive rate (TPR) and
#' false positive rate (FPR). AUC ranges from 0 to 1; a value of 0.5
#' indicates no discriminating ability (equivalent to random guessing),
#' while a value of 1 indicates perfect discrimination.
#'
#' @param x A \code{data.frame} returned by \link{roc}, from which
#'   \code{TPR} and \code{FPR} are extracted if not supplied directly.
#'   Defaults to \code{NULL}.
#' @param TPR Numeric vector of true positive rates, typically
#'   \code{x$TPR}.
#' @param FPR Numeric vector of false positive rates, typically
#'   \code{x$FPR}.
#'
#' @return A single numeric value: the area under the ROC curve.
#'
#' @rdname roc
#' @author The FLR Team
#' @seealso \link{roc}
#' @keywords utilities
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
