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

setGeneric("cpue", function(object, ...) standardGeneric("cpue"))

#' @rdname cpue
#' @aliases cpue,FLStock-method

setMethod('cpue',   signature(object='FLStock'),
  function(object, sel.pattern=harvest(object), effort = units(harvest(object)),
    mass = TRUE) {
    
    # EFFORT from F or HR
    if (effort[1] == "hr")
      E <- catch(object) / stock(object)
    else  
      E <- fbar(object)
    
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

setGeneric("survey", function(object, ...) standardGeneric("survey"))

#' @rdname cpue
#' @aliases cpue-FLStock-method

setMethod("survey",   signature(object="FLStock"),
  function(object, sel=stock.n(object) %=% 1, timing = 0.5, mass = FALSE) {
  
    # timing MUST BE 0 - 1
    timing <- pmax(pmin(timing, 1.0), 0.0)

    # CORRECT abundances for timing
    stock.n <- stock.n(object) * exp(-(harvest(object) * timing - m(object) * timing))
 
    # APPLY survey selectivity
    survey <- stock.n %*% sel

    # SET units as stock.n
    units(survey) <- units(stock.n)
  
    if (mass)
      survey <- survey * stock.wt(object)

    return(survey)
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

setMethod("rnoise", signature(n='numeric', len="missing"),
  function(n=n, sd=1, b=0, burn=0, trunc=0, seed=NA) {
    return(noiseFn(len=n, sd=sd, b=b, burn=burn, trunc=trunc, seed=seed))
  }
)

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
