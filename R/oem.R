# oem.R - DESC
# FLCore/R/oem.R

# Copyright European Union, 2017
# Author: Iago Mosqueira (EC JRC) <iago.mosqueira@ec.europa.eu>
#
# Distributed under the terms of the European Union Public Licence (EUPL) V.1.1.

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
  function(object, sel.pattern=harvest(object), effort = units(harvest(object)),
    mass = TRUE) {
    
    # EFFORT from F or HR
    if (effort[1] == "hr")
      E <- catch(object) / stock(object)
    else if (effort[1] == "f") 
      E <- fbar(object)
    else 
      E <- fbar(object) %=% effort
    
    cpue <- (catch.n(object) %*% sel.pattern) %/% E

    if (mass)
      cpue <- cpue * catch.wt(object)

  return(cpue)
  }
) # }}}

# survey {{{

#' survey, a method to generate an observation of abundance at age
#'
#' Description: Lorem ipsum dolor sit amet, consectetur adipiscing elit. Pellentesque eleifend
#' odio ac rutrum luctus. Aenean placerat porttitor commodo. Pellentesque eget porta
#' libero. Pellentesque molestie mi sed orci feugiat, non mollis enim tristique. 
#'
#' Details: Aliquam sagittis feugiat felis eget consequat. Praesent eleifend dolor massa, 
#' vitae faucibus justo lacinia a. Cras sed erat et magna pharetra bibendum quis in 
#' mi. Sed sodales mollis arcu, sit amet venenatis lorem fringilla vel. Vivamus vitae 
#' ipsum sem. Donec malesuada purus at libero bibendum accumsan. Donec ipsum sapien, 
#' feugiat blandit arcu in, dapibus dictum felis. 
#'
#' @param object The object on which to draw the observation
#'
#' @return An FLQuant for the index of abundance
#'
#' @name cpue
#' @rdname cpue
#' @aliases cpue cpue-methods
#'
#' @author The FLR Team
#' @seealso \link{FLComp}
#' @keywords classes
#' @examples
#'
#' \dontrun{
#' plot(FLQuants(om=stock(ple4), survey=quantSums(survey(ple4) * stock.wt(ple4)),
#'  cpue=quantSums(cpue(ple4)), hr=quantSums(cpue(ple4, effort="hr"))))
#' }

setGeneric("survey", function(object, index, ...) standardGeneric("survey"))

#' @rdname cpue
#' @aliases cpue-FLStock-method

setMethod("survey",   signature(object="FLStock", index="FLIndex"),
  function(object, index, sel=sel.pattern(index), mass = FALSE,
    timing = mean(range(index, c("startf", "endf"))),
    index.q = index@index.q) {

    # GET abundance
    abnd <- survey(object, sel=sel, timing=timing, mass=mass)

    # APPLY Q
    res <- abnd %*% index.q

    return(res)

  }
)

setMethod("survey", signature(object="FLStock", index="FLIndexBiomass"),
  function(object, index, sel=sel.pattern(index),
    ages=ac(seq(range(index, c('min')), range(index, c('max')))),
    timing = mean(range(index, c("startf", "endf"))),
    index.q = index@index.q) {

    # CHECK for ages
    
    # GET abundance
    abnd <- survey(object[ages, ], sel=sel[ages, ], timing=timing, mass=TRUE)

    # APPLY Q
    res <- quantSums(abnd) %*% index.q

    return(res)

  }
)

setMethod("survey",   signature(object="FLStock", index="missing"),
  function(object, sel=stock.n(object) %=% 1, ages=dimnames(object)$age,
    timing = 0.5, mass = FALSE) {
    
    # timing MUST BE 0 - 1
    timing <- pmax(pmin(timing, 1.0), 0.0)

    # GET index years
    yrs <- dimnames(sel)$year

    # CORRECT abundances for timing
    stock.n <- stock.n(object) *
      exp(-(harvest(object) * timing - m(object) * timing))
    
    # APPLY survey selectivity
    res <- stock.n[, yrs] %*% sel

    # SET units as stock.n
    units(res) <- units(stock.n)

    # SELECT ages
    res <- res[ages,]
  
    if (mass)
      res <- res * stock.wt(object)[, yrs]

    return(res)
  }
) # }}}

# hyperstability {{{

hyperstability <- function(object, omega=1, ref=yearMeans(object)) {
  return(ref %*% ((object %/% ref) ^ omega))
} # }}}

# bias {{{

bias <- function(object, bias=0.02){
  return(FLQuant(cumprod(1 + rep(c(bias), dim(object)[2])), dimnames=dimnames(object)))
}

biased <- function(object, bias=0.02){
  return(object) * bias(object, bias=bias)
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
    if(order[1] == "ahead")
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

ar1rlnorm <- function(rho, years, iters=1, mean=0, margSD=0.6) {

  #
	n <- length(years)
	rhosq <- rho ^ 2
	
  #
  res <- matrix(rnorm(n*iters, mean=mean, sd=margSD), nrow=n, ncol=iters)

	res <- apply(res, 2, function(x) {
		for(i in 2:n)
			x[i] <- sqrt(rhosq) * x[i-1] + sqrt(1-rhosq) * x[i]
		return(exp(x))
	})

	return(FLQuant(array(res, dim=c(1,n,1,1,1,iters)),
		dimnames=list(year=years, iter=seq(1, iters))))
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
#' runstest(fitted(nsher), rec(nsher))

setMethod("runstest", signature(fit="FLQuants", obs="missing"),
  function(fit, combine=TRUE) {

    # COMBINE
    if(combine) {
      fit <- lapply(fit, quantSums)
    }
    
    # RESIDUALS
    res <- fit
  
    # sigma3, by index
    if(combine) {
      s3s <- lapply(res, sigma3)
      # or index and age
    } else {
      s3s <- lapply(res, function(x) {
        rbindlist(lapply(divide(x, 1), sigma3), idcol="age")
      })
    }
 
    # MERGE
    s3dat <- do.call(rbind, c(Map(function(x, y)
      cbind(x, qname=y), lapply(s3s, as.data.frame), names(s3s)),
      make.row.names = FALSE))
    
    # p.value >= 0.05 -> TRUE, green
    s3dat$pass <- s3dat$p.value >= 0.05

    return(data.table(s3dat))
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
  # Plot the data if requested by the user
  if (plot){
    plot((1:n)[s>0],x[s>0], xlim=c(1,n), ylim=c(min(x),max(x)), xlab="", ylab=dname)
    points((1:n)[s<0],x[s<0], col="red")
    abline(h=threshold, col=gray(.4))
    for (i in 1:(n-1)){
      if (s[i]*s[i+1]<0){abline(v=i+0.5, lty=2)}
    }
  }
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
