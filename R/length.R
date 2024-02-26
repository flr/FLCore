# length.R - DESC
# FLCore/R/length.R

# Copyright (c) WUR, 2022.
# Author: Iago MOSQUEIRA (WMR) <iago.mosqueira@wur.nl>

# growth models {{{

#' Growth models
#'
#' @rdname growth-models
#' @examples
#' data(ple4)

#' vonbert
#'
#' @rdname growth-models
#' @examples
#' vonbert(linf=35, k=0.352, t0=-0.26, age=1:14)

vonbert <- function(linf, k, t0, age) {
  linf * (1.0 - exp((-k * (age - t0))))
}

#' ivonbert
#'
#' @rdname growth-models
#' @examples
#' ivonbert(35, 0.352, -0.26, 1:34)

ivonbert <- function(linf, k, t0, len) {
  pmax(0, log(1 - (len / linf)) / (-k) + t0)
}

#' gompertz
#'
#' @rdname growth-models
#' @examples
#' gompertz(linf=179.13, k=0.4088, a=1.7268, age=1:12)

gompertz <- function(linf, a, k, age) {
  linf * exp(-a * exp(log(k) * age))
}

#' richards
#'
#' @rdname growth-models
#' @examples
#' richards(linf=178.63, k=0.424, b=-7.185, m=2880.4, age=1:12)

richards <- function(linf, k, b, m, age) {
  linf / exp(log(1 + exp(-k * age + b)) * m)
}

# }}}

# invALK {{{

invALK <- function(params, model=vonbert, age, cv=0.1, lmax=1.2, bin=1,
  max=ceiling(linf * lmax), reflen=NULL) {

    linf <- c(params['linf'])

    # FOR each age
    bins <- seq(0, max, bin)

    # METHOD
    if(isS4(model))
      len <- do.call(model, list(age=age,params=params))
    else {
      lparams <- as(FLPar(params), "list")
      len <- do.call(model, c(list(age=age),
        lparams[names(lparams) %in% names(formals(model))]))
    }

    if(is.null(reflen)) {
      sd <- abs(len * cv)
    } else {
      sd <- reflen * cv
    }

    probs <- Map(function(x, y) {
      p <- c(pnorm(1, x, y),
        dnorm(bins[-c(1, length(bins))], x, y),
        pnorm(bins[length(bins)], x, y, lower.tail=FALSE))
      return(p / sum(p))
    }, x=len, y=sd)

    res <- do.call(rbind, probs)

    alk <- FLPar(array(res, dim=c(length(age), length(bins), 1)),
      dimnames=list(age=age, len=bins, iter=1), units="")

    return(alk)
} 
# }}}

# lenSamples {{{

#' @examples
#' data(ple4)
#' ialk <- invALK(params=c(linf = 60, k = 2.29e-01, t0 = -1.37e+00),
#'   model=vonbert, age=1:10, lmax=1.2)
#' lenSamples(catch.n(ple4), invALK=ialk, n=250)


lenSamples <- function(object, invALK, n=300) {
  
  # PROPAGATE invALK to match object
  invALK <- propagate(invALK, dim(object)[6])

  # DIMS
  dmo <- dim(object)
  dno <- dimnames(object)
  dmi <- dim(invALK)
  dni <- dimnames(invALK)

  # RESULTS
  res <- array(NA, dim=c(dmi[2], dmo[2], 1, 1, 1, dmo[6]))
  ob <- unname(object)

  # LOOP over iters and years
  for(i in seq(dmo[6])) {
    for(y in seq(dmo[2])) {
      # GET proportions at length from invALK
      res[,y,,,,i] <- c(ob[,y,,,,i] %*% invALK[,,i, drop=TRUE])
      # SAMPLE from multinom
      res[,y,,,,i] <- apply(rmultinom(n, 1, prob=c(res[,y,,,,i])), 1, sum)
    }
  }
  
  out <- FLQuant(res, dimnames=list(len=dni$len, year=dno$year, iter=dno$iter))

  return(out)
}
# }}}

# mlc {{{

mlc <- function(samples) {
  return(quantSums(as.numeric(dimnames(samples)$len) * samples)
    / quantSums(samples))
} # }}}

#' @rdname length-based-indicators
#' @references
#' - Kell, L.T., Minto, C., Gerritsen, H.D. 2022. Evaluation of the skill of length-based indicators to identify stock status and trends. ICES Journal of Marine Science. doiu: 10.1093/icesjms/fsac043.
#' - ICES. 2015. Report of the Fifth Workshop on the Development of Quantitative Assessment Methodologies based on Life-history Traits, Exploitation Characteristics and other Relevant Parameters for Data-limited Stocks (WKLIFE V), 5–9 October 2015, Lisbon, Portugal. ICES CM 2015/ACOM:56. 157 pp.
#' - ICES. 2020. Tenth Workshop on the Development of Quantitative Assessment Methodologies based on LIFE-history traits, exploitation characteristics, and other relevant parameters for data-limited stocks (WKLIFE X). ICES Scientific Reports. 2:98. 72 pp. http://doi.org/10.17895/ices.pub.5985
#' @examples
#' data(ple4)
#' indicators.len(ple4, indicators=c('lbar', 'lmaxy'),
#'   params=FLPar(linf=132, k=0.080, t0=-0.35), metric='catch.n',
#'   lenwt=FLPar(a=0.01030, b=2.975))
#' indicators.len(ple4, indicators=c('pmega'),
#'   params=FLPar(linf=60, k=2.29e-01, t0=-1.37), metric='catch.n')
#' data(ple4.index)
#' indicators.len(ple4.index, indicators=c('lbar', 'lmean'),
#'   params=FLPar(linf=132, k=0.080, t0=-0.35), metric='index')
#' #
#' ialk <- invALK(params=FLPar(linf = 60, k = 2.29e-01, t0 = -1.37e+00),
#'   model=vonbert, age=1:10, lmax=1.2)
#' samps <- lenSamples(catch.n(ple4), invALK=ialk, n=250)

# TODO: LOOP over metric

indicators.len <- function (object, indicators="lbar", model=vonbert, params,
  cv=0.1, lmax=1.25, bin=1, n=500, metric=catch.n, ...) {

  # MERGE params and ...
  # TODO: DEAL with params as vector
  params <- c(as(params, 'list'), list(...))

  # COMPUTE inverse ALK (cv, lmax, bin)
  ialk <- invALK(FLPar(params[names(params) %in% names(formals(model))]),
    age=seq(dims(object)$min, dims(object)$max), cv=cv, lmax=lmax,
    bin=bin, model=model)

  # GENERATE length samples from metric
  input <- do.call(metric, list(object))

  samps <- lenSamples(input, ialk, n=n)

  # OBTAIN names from functions
  nms <- unlist(lapply(indicators, function(x)
      if(is(x, "function"))
        find.original.name(x)
      else
        x
    ))

  # SORT OUT names
  if(is.null(names(indicators)))
    names(indicators) <- nms
  else
    names(indicators)[names(indicators) == 0] <- nms[names(indicators) == 0]
  
  # COMPUTE indicator(s)
  ind <- lapply(setNames(indicators, nm=nms), function(x) {

    # SUBSET indicator arguments in params
    if(is.character(x)) {
      pars <- params[names(params) %in% names(formals(get(x)))]
    } else if(is.function(x)) {
      pars <- params[names(params) %in% names(formals(x))]
    }
    
    return(do.call(x, args=c(list(samps), pars)))
  })

  return(ind)
}

# lenquantile {{{

#' Calculate quantile(s) of length distribution
#'
#' @rdname length-based-indicators
#' @examples
#' lenquantile(samps, 0.50)

lenquantile <- function(x, quantile=0.50) {
 
  # OUTPUT object, one len
  res <- x[1,,,,,]

  # LOOP over dims 2:6
  for(i2 in seq(dim(x)[2])) {
    for(i3 in seq(dim(x)[3])) {
      for(i4 in seq(dim(x)[4])) {
        for(i5 in seq(dim(x)[5])) {
          for(i6 in seq(dim(x)[6])) {

            # SUBSET vector for all lens
            a <- x[,i2,i3,i4,i5,i6]

            # RESCALE to max(a) | 1e4 to avoid huge vectors
            ma <- max(a)
            a <- (a / ma) * min(ma, 1e4)
            
            # REPEAT lengths
            b <- as.numeric(rep(dimnames(a)$len, times=floor(a)))
            # COMPUTE quantile
            res[, i2, i3, i4, i5, i6] <- quantile(b, quantile)
          }
        }
      }
    }
  }

  dimnames(res)$len <- 'all'

  return(res)
}

# }}}

# lmax5 {{{

#' lmax5, Mean length of largest 5%
#'
#' @rdname length-based-indicators
#' @examples
#' lmax5(samps)

lmax5 <- function(x) {

  # CREATE copy of x with lens
  lens <- x %=% as.numeric(dimnames(x)$len)

  # COMPUTE 95% quantile
  q95 <- lenquantile(x, 0.95)

  # REMOVE values of len < q95
  x[lens < expand(q95, len=dimnames(x)$len)] <- 0

  res <- quantSums(x * lens) / quantSums(x)
  units(res) <- "cm"

  return(res)
}
# }}}

# l95 {{{

#' l95, 95% percentile
#'
#' @rdname length-based-indicators
#' @examples
#' l95(samps)

l95 <- function(x) {

  # CREATE copy of x with lens
  lens <- x %=% as.numeric(dimnames(x)$len)

  # COMPUTE 95% quantile
  res <- lenquantile(x, 0.95)

  units(res) <- "cm"

  return(res)
}
# }}}

# l25 {{{

#' l25, 25% percentile
#'
#' @rdname length-based-indicators
#' @examples
#' l25(samps)

l25 <- function(x) {

  # CREATE copy of x with lens
  lens <- x %=% as.numeric(dimnames(x)$len)

  # COMPUTE 95% quantile
  res <- lenquantile(x, 0.25)

  units(res) <- "cm"

  return(res)
}

# }}}

# lc50 {{{

#' lc50, Length at 50% of modal abundance
#'
#' @rdname length-based-indicators
#' @examples
#' lc50(samps)

lc50 <- function(x) {

  # CREATE copy of x with lens
  lens <- x %=% as.numeric(dimnames(x)$len)

  # GET position of length with max yield
  posmode <- apply(x, 2:6, function(i) which(i == max(i))[1])

  res <- lens[posmode] / 2
  units(res) <- "cm"

  return(res)
}

# }}}

# lmode {{{

#' lmode, modal length
#'
#' @rdname length-based-indicators
#' @examples
#' lmode(samps)

lmode <- function(x) {

  # CREATE copy of x with lens
  lens <- x %=% as.numeric(dimnames(x)$len)

  # GET position of length with max yield
  posmode <- apply(x, 2:6, function(i) which(i == max(i))[1])

  res <- lens[posmode]
  units(res) <- "cm"

  return(res)

}
# }}}

# lbar {{{

#' lbar, mean length of individuals
#'
#' @rdname length-based-indicators
#' @examples
#' lbar(samps)

lbar <- function(x) {

  # CREATE copy of x with lens
  lens <- x %=% as.numeric(dimnames(x)$len)

  res <- quantSums(x * lens) / quantSums(x)
  units(res) <- "cm"

  return(res)
}
# }}}

# lmean {{{

#' lmean, mean length of individuals > lmode
#'
#' @rdname length-based-indicators
#' @examples
#' lmean(samps)
#' # Linf(ple4) = 60
#' lmean(samps) / (0.75 * lc50(samps) + 0.25 * 60) #

lmean <- function(x) {

  # CREATE copy of x with lens
  lens <- x %=% as.numeric(dimnames(x)$len)

  lmod <- lmode(x)

  # REMOVE values if len <= lmode
  x[lens <= expand(lmode(x), len=dimnames(x)$len)] <- 0

  res <- quantSums(x * lens) / quantSums(x)
  units(res) <- "cm"

  return(res)
}
# }}}

# lmaxy {{{

#' lmaxy, length class with maximum biomass in catch
#'
#' @rdname length-based-indicators
#' @examples
#' lenwt <- FLPar(a=0.01030, b=2.975)
#' lmaxy(samps, lenwt)

lmaxy <- function(x, lenwt) {

  # GET watage from length-weight
  wt <- c(lenwt$a) * as.numeric(dimnames(x)$len) ^ c(lenwt$b)
 
  # COMPUTE biomass per length bin
  biom <- x * (x %=% wt)

  # CREATE copy of x with lens
  lens <- x %=% as.numeric(dimnames(x)$len)

  # GET length with max yield
  maxlen <- biom == expand(apply(biom, 2:6,  max),
    len=dimnames(x)$len)

  # FIND len that matches maxlen
  res <- FLQuant(c(lens)[c(maxlen)], dimnames=c(list(len='all'),
    dimnames(x)[-1]))

  units(res) <- "cm"

  return(res)
}
# }}}

# pmega {{{

#' pmega, Proportion of individuals above L opt + 10%
#'
#' @rdname length-based-indicators
#' @examples
#' pmega(samps, linf=60)

pmega <- function(x, linf, lopt=linf * 2/3) {

  # CREATE copy of x with lens
  lens <- x %=% as.numeric(dimnames(x)$len)

  # EXTRACT numbers with len > lopt
  lar <- x
  lar[lens < lopt * 1.10, ] <- 0

  # PROPORTION > lopt
  res <- quantSums(lar) / quantSums(x)

  return(res)
}
# }}}

# bheqz {{{

#' bheqz, Beverton & Holt 
#'
#' z = (k * (linf - lmean)) / (lmean - lc)
#' lmean = sum(naa * len) / sum(naa)
#' lc, length at first capture
#' @rdname length-based-indicators
#' @examples
#' linf <- 60
#' k <- 2.29e-01
#' t0 <- -1.37e+00
#' bheqz(samps, linf = 60, k = 2.29e-01, t0 = -1.37e+00)

bheqz <- function(x, linf, k, t0, lc=lc50(x)) {

  # CREATE copy of x with lens
  lens <- x %=% as.numeric(dimnames(x)$len)

  # COMPUTE (weighted) mean length in catch
  meanl <- quantSums(lens * x) / quantSums(x)

  # z = (k * (linf - meanl)) / (meanl - lc)
  res <- (k * (linf - meanl)) / (meanl - lc)

  units(res) <- "z"

  return(res)
}
# }}}

# TODO: lopt (Froese, 2008, Minimizing the impact ...) {{{

#' @examples
#' lopt(linf=35, m=0.1, k=0.352)

lopt <- function(linf, m, k) {
  return(linf * (3 / (3 + m/k)))
}
# }}}
