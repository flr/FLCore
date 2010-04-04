# SRmodels - Stock-recruitment models
# FLCore/R/SRmodels

# Copyright 2003-2007 FLR Team. Distributed under the GPL 2 or later
# Maintainer: Laurie Kell, Cefas
# $Id$

# Reference:
# Notes:

# models

# ricker  {{{
ricker <- function()
{
  logl <- function(a, b, rec, ssb)
      loglAR1(log(rec), log(a*ssb*exp(-b*ssb)))

  initial <- structure(function(rec, ssb) {
		# The function to provide initial values
    res  <-coefficients(lm(c(log(rec/ssb))~c(ssb)))
    return(FLPar(a=max(exp(res[1])), b=-max(res[2])))
	},
  # lower and upper limits for optim()
	lower=rep(1e-10, 2),
	upper=rep(Inf, 2)
	)
	model  <- rec~a*ssb*exp(-b*ssb)
	return(list(logl=logl, model=model, initial=initial))
} # }}}

# bevholt {{{
bevholt <- function()
  {
  ## log likelihood, assuming normal log.
  logl <- function(a, b, rec, ssb)
      loglAR1(log(rec), log(a*ssb/(b+ssb)))

  ## initial parameter values
  initial <- structure(function(rec, ssb) {
    a <- max(quantile(c(rec), 0.75, na.rm = TRUE))
    b <- max(quantile(c(rec)/c(ssb), 0.9, na.rm = TRUE))
    return(FLPar(a = a, b = a/b))
	},

  ## bounds
  lower=rep(10e-8, 2),
	upper=rep(Inf, 2))

  ## model to be fitted
  model  <- rec~a*ssb/(b+ssb)
  
	return(list(logl=logl, model=model, initial=initial))
} # }}}

# segreg  {{{
segreg <- function()
{
	logl <- function(a, b, rec, ssb)
    loglAR1(log(rec), log(ifelse(ssb<=b,a*ssb,a*b)))

  model <- rec ~ FLQuant(ifelse(ssb<=b,a*ssb,a*b))

  initial <- structure(function(rec, ssb)
  {
    return(FLPar(a=median(c(rec/ssb)), b=median(c(ssb))))
  },
    lower=rep(0, 1e-7),
    upper=rep(Inf, 2))

	return(list(logl=logl, model=model, initial=initial))
} # }}}

# geomean {{{
geomean<-function() 
    {
    logl <- function(a, rec)
      loglAR1(log(rec), log(FLQuant(rep(a, length(rec)))))
    
    initial <- structure(function(rec) {
        return(FLPar(a = exp(mean(log(rec), na.rm=TRUE))))
        }, 
        lower = c(1e-08), upper = rep(Inf))
    
    model <- rec ~ a + ssb/ssb - 1
    
    return(list(logl = logl, model = model, initial = initial))
    } # }}}

# shepherd  {{{
shepherd <- function()
{
  logl <- function(a,b,c,rec,ssb)
      loglAR1(log(rec), log(a*ssb/(1+(ssb/b)^c)))

  initial <- structure(function(rec,ssb){
    c <- 1
    x <- ssb^c
		y <- ssb/rec

    res <- coefficients(lm(c(y)~c(x)))

    a <- max(1/res[1])
    b <- max(b=1/((res[2]*a)^(1/c)))

    return(FLPar(a=a,b=b,c=c))},
    
    lower = c(1e-08, 1e-08, 1),
    upper = c(1e02,  1e+08,10))

  model <- rec ~ a * ssb/(1 + (ssb/b)^c)

  return(list(logl = logl, model = model, initial = initial))
} # }}}

# cushing {{{
cushing<-function()
{
  logl <- function(a, b, rec, ssb)
    loglAR1(log(rec), log(a*ssb^b))

  initial <- structure(function(rec, ssb)
  {
    a <- mean(rec/ssb)
    b <- 1.0
    return(FLPar(a=a,b=b))
  },
  lower=c(0, 0.0001),
	upper=c(Inf, 1))

  model  <- rec~a*ssb^b

	return(list(logl=logl, model=model, initial=initial))
}  # }}}

# rickerSV  {{{
rickerSV <- function()
{
  logl <- function(s, v, spr0, rec, ssb)
  { 
    pars <- abPars('ricker', s=s, v=v, spr0=spr0)
    loglAR1(log(rec), log(pars['a']*ssb*exp(-pars['b']*ssb)))
  }

  initial <- structure(function(rec, ssb)
  {
    s <- 0.75
    spr0 <- quantile(c(ssb/rec), prob = 0.9, na.rm = TRUE, names=FALSE)
    v <-mean(as.vector(ssb), na.rm = TRUE)*2
    return(FLPar(s=s, v=v, spr0=spr0))
	},
  ## bounds
  lower=c(rep(1e-8, 3)),
	upper=c(10, Inf, Inf))

	model  <- rec~abPars('ricker', s=s, v=v, spr0=spr0)['a']*ssb*exp(-abPars('ricker', s=s, v=v, spr0=spr0)['b']*ssb)

	return(list(logl=logl, model=model, initial=initial))
} # }}}

# bevholtSV {{{
bevholtSV <- function()
  {
  logl <- function(s, v, spr0, rec, ssb)
  {
    pars <- abPars('bevholt', s=s, v=v, spr0=spr0)
    loglAR1(log(rec), log(pars['a']*ssb/(pars['b']+ssb)))
  }

  ## initial parameter values
  initial <- structure(function(rec, ssb)
  {
    s <- 0.75
    spr0 <- quantile(c(ssb/rec), prob = 0.9, na.rm = TRUE, names=FALSE)
    v <-mean(as.vector(ssb), na.rm = TRUE)*2
    return(FLPar(s=s, v=v, spr0=spr0))
	},
  ## bounds
  lower=c(0.2, rep(10e-8, 2)),
	upper=c(0.999, Inf, Inf))

  ## model to be fitted
  model  <- rec~abPars('bevholt', s=s, v=v, spr0=spr0)['a']*ssb /
    (abPars('bevholt', s=s, v=v, spr0=spr0)['b']+ssb)
  
	return(list(logl=logl, model=model, initial=initial))
} # }}}

# shepherdSV {{{
shepherdSV <- function()
  {
  logl <- function(s, v, spr0, c, rec, ssb)
  {
    pars <- abPars('shepherd', s=s, v=v, spr0=spr0, c=c)
    loglAR1(log(rec), log(pars['a']*ssb/(1+(ssb/pars['b'])^c)))
  }

  ## initial parameter values
  initial <- structure(function(rec, ssb)
  {
    s <- 0.75
    spr0 <- quantile(c(ssb/rec), prob = 0.9, na.rm = TRUE, names=FALSE)
    v <-mean(as.vector(ssb), na.rm = TRUE)*2
    return(FLPar(s=s, v=v, spr0=spr0, c=1))
	},
  ## bounds
  lower=c(0.2, rep(10e-8, 2), 1),
	upper=c(0.999, Inf, Inf, 10))

  ## model to be fitted
  model  <- rec~abPars('shepherd', s=s, v=v, spr0=spr0, c=c)['a']*ssb /
    (1 + (ssb / abPars('shepherd', s=s, v=v, spr0=spr0, c=c)['b']) ^ c)
  
	return(list(logl=logl, model=model, initial=initial))
} # }}}

# bevholtAR1 {{{
bevholtAR1 <- function()
  {
  ## log likelihood, assuming normal log.
  logl <- function(a, b, rho, rec, ssb)
      loglAR1(log(rec), log(a*ssb/(b+ssb)), rho=rho)

  ## initial parameter values
  initial <- structure(function(rec, ssb) {
    a <- max(quantile(c(rec), 0.75, na.rm = TRUE))
    b <- max(quantile(c(rec)/c(ssb), 0.9, na.rm = TRUE))
    return(FLPar(a = a, b = a/b, rho=0))
	},

  ## bounds
  lower=c(rep(10e-8, 2), -1),
	upper=c(rep(Inf, 2), 1))

  ## model to be fitted
  model  <- rec~a*ssb/(b+ssb)
  
	return(list(logl=logl, model=model, initial=initial))
} # }}}

# methods

# spr0  {{{
## calcs spawner per recruit at F=0.0   
setMethod('spr0', signature(ssb='FLQuant', rec='FLQuant', fbar='FLQuant'),
   function(ssb, rec, fbar)
  {
    if  (any(dim(ssb)[3:5]>1)) "stop multiple units, seasons, areas not allowed yet"
    if  (any(dim(rec)[3:5]>1)) "stop multiple units, seasons, areas not allowed yet"
    if  (any(dim(fbar)[3:5]>1)) "stop multiple units, seasons, areas not allowed yet"

    # years: corrects length if mismatch
    minyear <- max(unlist(lapply(list(fbar=fbar, ssb=ssb, rec=rec),
      function(x) min(as.numeric(dimnames(x)$year)))))
    maxyear <- min(unlist(lapply(list(fbar=fbar, ssb=ssb, rec=rec),
      function(x) max(as.numeric(dimnames(x)$year)))))

    # ssb & f
    ssb  <- ssb[ 1, as.character(seq(minyear, maxyear)), drop=TRUE]
    rec  <- rec[ 1, as.character(seq(minyear, maxyear)), drop=TRUE]
    fbar <- fbar[1, as.character(seq(minyear, maxyear)), drop=TRUE]

    # spr0
    spr0 <- lm(c(ssb/rec)~c(fbar))$coefficients[1]
    names(spr0) <- "spr0"

    return(spr0)
  }
)

setMethod('spr0', signature(ssb='FLStock', rec='missing', fbar='missing'),
  function(ssb)
  {
    # rec
    sr <- as.FLSR(ssb)

    # spr0
    spr0(ssb=ssb(ssb), rec=rec(sr), fbar=fbar(ssb))
  }
)

setMethod('spr0', signature(ssb='FLSR', rec='missing', fbar='FLQuant'),
  function(ssb, fbar)
  {
    # spr0
    spr0(ssb=ssb(ssb), rec=rec(ssb), fbar=fbar)
  }
)
# }}}

# rSq {{{
setMethod('rSq', signature(obs='FLQuant',hat='FLQuant'),
  function(obs, hat=rep(0,length(obs)))
  {
    ## calculates R squared
    mn   <-mean(obs)
    mnHat<-mean(hat)
    SStot<-sum((obs-mn)^2)
    SSreg<-sum((hat-mnHat)^2)
    SSerr<-sum((obs-hat)^2)

    res  <-1-SSerr/SStot

    return(res)
  }
) # }}}

# loglAR1 {{{
setMethod('loglAR1', signature(obs='FLQuant', hat='FLQuant'),
  function(obs, hat, rho=0)
  {
    # calculates likelihood for AR(1) process
    n   <- dim(obs)[2]
    rsdl<-(obs[,-1] - rho*obs[,-n] - hat[,-1] + rho*hat[,-n])
    s2  <- sum(rsdl^2, na.rm=T)
    s1  <-s2

    if (!is.na(rsdl[,1]))
      s1 <- s1+(1-rho^2)*rsdl[,1]^2

    sigma2   <- sigma(obs, hat)^2
    n        <- length(obs[!is.na(obs)])
    sigma2.a <- (1-rho^2)*sigma2
    res      <- (log(1/(2*pi))-n*log(sigma2.a)+log(1-rho^2)-s1/(2*sigma2.a))/2

    if (!is.finite(res))
      res <- -1e100

    return(res)
  }
) # }}}

# SRModelName {{{
SRModelName <- function(model)
{
  return(switch(gsub(" ", "", as.character(as.list(model)[3])),
      "a*ssb*exp(-b*ssb)" = "ricker",
      "a*ssb/(b+ssb)" = "bevholt",
      "a*ssb/(1+(ssb/b)^c)" = "shepherd",
      "a*ssb^b" = "cushing",
      "FLQuant(ifelse(ssb<=b,a*ssb,a*b))" = "segreg",
      "FLQuant(a,dimnames=dimnames(rec))" = "mean",
      "a" = "mean",
      'abPars("bevholt",s=s,v=v,spr0=spr0)["a"]*ssb/(abPars("bevholt",s=s,v=v,spr0=spr0)["b"]+ssb)' = "bevholtSV",
      'abPars("ricker",s=s,v=v,spr0=spr0)["a"]*ssb*exp(-abPars("ricker",s=s,v=v,spr0=spr0)["b"]*ssb)' = "rickerSV",
      NULL))
} # }}}

# SRNameCode {{{
SRNameCode <- function(name)
{
  code <- switch(name,
    "mean" = 1,
    "bevholt" = 2,
    "ricker" = 3,
    "segreg" = 4,
    "shepherd" = 5,
    "cushing" = 6,
    "dersch" = 7,
    "pellat" = 8,
    "bevholtD" = 21,
    "bevholtSV" = 22,
    "rickerD" = 31,
    "rickerSV" = 32,
    "shepherdD" = 51,
    "shepherdSV" = 52,
    NA)

  if(is.na(code))
    stop("model name has not been recognized")

  return(as.integer(code))
} # }}}

# spr2v {{{
spr2v <- function(model, spr, a=NULL, b=NULL, c=NULL, d=NULL)
{
  # SSB as function of ssb/rec
  return(switch(model,
    "bevholt"  = a*(spr)-b,
    "ricker"   = log(a*spr)/b,
    "cushing"  = (1/(a*spr))^(1/(b-1)),
    "shepherd" = b*(a*spr-1)^(1/c),
    "segreg"   = ifelse(ssb <= b, a/(spr), 0),
    "mean"     = a/(spr),
    "dersh"    = ssb*a*(1-b*c*ssb)^c,
    "pellat"   = 1/(a/ssb-a/ssb*(ssb/b)^c),
    NULL))
} # }}}

# srr2s {{{
srr2s <- function(model, ssb=NULL, spr=NULL, a=NULL, b=NULL, c=1, d=NULL)
{
  #recruits as function of ssb or ssb/rec
  if (is.null(ssb) & !is.null(spr))
    ssb <- spr2v(model, spr, a, b, c, d)

  eval(as.list(do.call(model, list())$model)[[3]], envir=list(ssb=ssb, spr0=spr, a=a, b=b, c=c, d=d))
} # }}}

# abPars {{{
abPars <- function(model, s=NULL, v, spr0, c=NULL, d=NULL)
{
  # converts a & b parameterisation into steepness & virgin biomass (s & v)
  switch(model,
    "bevholt" ={a=(v+(v-s*v)/(5*s-1))/spr0; b=(v-s*v)/(5*s-1)},
    "bevholtSV" ={a=(v+(v-s*v)/(5*s-1))/spr0; b=(v-s*v)/(5*s-1)},
    "ricker"  ={b=log(5*s)/(v*0.8); a=exp(v*b)/spr0},
    "rickerSV"  ={b=log(5*s)/(v*0.8); a=exp(v*b)/spr0},
    "cushing" ={b=log(s)/log(0.2); a=(v^(1-b))/(spr0)},
    "cushingSV" ={b=log(s)/log(0.2); a=(v^(1-b))/(spr0)},
    "shepherd"={b=v*(((0.2-s)/(s*0.2^c-0.2))^-(1/c)); a=((v/b)^c+1)/spr0},
    "shepherdSV"={b=v*(((0.2-s)/(s*0.2^c-0.2))^-(1/c)); a=((v/b)^c+1)/spr0},
    "mean"    ={a=v/spr0;b=NULL},
    "meanSV"    ={a=v/spr0;b=NULL},
    "segreg"  ={a=5*s/spr0; b=v/(a*spr0)},
    "segregSV"  ={a=5*s/spr0; b=v/(a*spr0)},
    {stop("model name not recognized")})

  res <- c(a=a, b=b)
  return(res[!is.null(res)])
} # }}}

# svPars {{{
svPars <- function(model, spr0, a, b=NULL, c=NULL, d=NULL)
{
  v <- spr2v(model, spr0, a, b, c, d)
  s <- srr2s(model, ssb=v*.2, a=a, b=b, c=c, d=d) / srr2s(model, ssb=v, a=a,
      b=b, c=c, d=d)
  return(c(s=s, v=v, spr0=spr0))
}
# }}}
