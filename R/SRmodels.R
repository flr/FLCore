# SRmodels - Stock-recruitment models
# FLCore/R/SRmodels

# Copyright 2003-2007 FLR Team. Distributed under the GPL 2 or later
# Maintainer: Laurie Kell, Cefas
# $Id$

# Reference:
# Notes:

# SRModelName {{{
SRModelName<-function(formula)
{
  srmodels <- list('ricker', 'ricker.d', 'ricker.c.a', 'ricker.c.b', 'ricker.sv',
  'ricker.ar1', 'bevholt', 'bevholt.ar1','bevholt.d', 'bevholt.c.a', 'bevholt.c.b',   
  'bevholt.sv', 'bevholt.ndc', 'shepherd', 'shepherd.ar1', 'shepherd.d', 'geomean', 'segreg')
  srformulae <- lapply(srmodels, function(x) do.call(x, list())$model)
  names(srformulae) <- srmodels
  for(i in srmodels)
    if(formula == srformulae[[i]])
      return(i)
  return(FALSE)
} # }}}

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

# sv2ab & ab2sv {{{
# calc steepness & virgin biomass from alpha & beta, given SSB per R at F=0
sv2ab <- function(steepness, vbiomass, spr0, model)
{
   bh.sv2ab<-function(steepness,vbiomass,spr0)
      {
      a <- vbiomass*4*steepness/(spr0*(5*steepness-1.0))
      b  <- a*spr0*(1.0/steepness - 1.0)/4.0

			res       <-c(a,b)
			names(res)<-c("a","b")
			return(res)
			}

   rk.sv2ab<-function(steepness,vbiomass,spr0){
      b  <- log(5.0*steepness)/(vbiomass*0.8);
      a <- exp(b*vbiomass)/spr0;

			res   <-c(a,b)
			names(res)<-c("a","b")
			return(res)
			}

   if (model=="bevholt") return(bh.sv2ab(steepness,vbiomass,spr0))
   if (model=="ricker")  return(rk.sv2ab(steepness,vbiomass,spr0))
}

#Get a & b from Steepness & virgin biomass
ab2sv<-function(a,b,spr0,model)
   {
   bh.ab2sv<-function(a,b,spr0){
			steepness <- a*spr0/(4*b+a*spr0)
			vbiomass  <- (spr0*a*(5*steepness-1))/(4*steepness)

			res       <-c(steepness,vbiomass)
			names(res)<-c("steepness","vbiomass")
			return(res)
      }

   rk.ab2sv<-function(a,b,spr0){
			vbiomass <- log(spr0 * a)/b
			steepness<- 0.2*exp(b*(vbiomass)*0.8);

			res       <-c(steepness,vbiomass)
			names(res)<-c("steepness","vbiomass")
			return(res)
      }
  
   if (model=="bevholt") return(bh.ab2sv(a,b,spr0))
   if (model=="ricker")  return(rk.ab2sv(a,b,spr0))
}
# }}}

# ab {{{
setMethod('ab', signature(object='FLSR'),
  function(object, plusgroup=dims(object)$max, ...){

  if (!(SRModelName(model(object)) %in% c("bevholt.sv","ricker.sv")))
  return(object)

  dmns<-dimnames(params(object))
  dmns$params<-c("a","b")

  if (SRModelName(model(object)) == "bevholt.sv")
     model<-"bevholt"
  else if (SRModelName(model(object)) == "ricker.sv")
     model<-"ricker"

  par<-FLPar(sv2ab(params(object)["s",],params(object)["v",],params(object)["spr0",],model=model),dimnames=dmns)

  if (SRModelName(model(object)) == "bevholt.sv")
     model(object)<-bevholt()
  else if (SRModelName(model(object)) == "ricker.sv")
     model(object)<-ricker()

  params(object)<-par

  return(object)
  })  # }}}

# Ricker  {{{
ricker <- function()
{
	logl <- function(a, b, sigma2, rec, ssb)
	# The actual minus log-likelihood
	sum(dnorm(log(rec), log(a*ssb*exp(-b*ssb)), sqrt(sigma2), TRUE), na.rm=TRUE)

  initial <- structure(function(rec, ssb)
		# The function to provide initial values
		{
			x <- ssb
			y <- log(rec/ssb)
			sx <- sum(x, na.rm=TRUE)
			sy <- sum(y, na.rm=TRUE)
			sxx <- sum(x*x, na.rm=TRUE)
			sxy <- sum(x*y, na.rm=TRUE)
			s2x <- sx*sx
			sxsy <- sx*sy
	
			b <- -(length(ssb)*sxy-sxsy)/(length(ssb)*sxx-s2x)
      b <- b + b/10
			a <- exp(sum(y, na.rm=TRUE)/length(ssb) + b*(sum(x, na.rm=TRUE)/length(ssb)))
      a <- a + a/10
			return(list(a=a, b=b, sigma2=var(log(rec) - log(a*ssb*exp(-b*ssb)), na.rm=TRUE)))
		},
		# lower and upper limits for optim()
		lower=rep(1e-10, 3),
		upper=rep(Inf, 3)
	)
	model  <- rec~a*ssb*exp(-b*ssb)
	return(list(logl=logl, model=model, initial=initial))
} # }}}

# bevholt {{{
bevholt <- function()
  {
  ## log likelihood, assuming normal log.
  logl <- function(a, b, sigma2, rec, ssb)
		sum(dnorm(log(rec), log(a*ssb/(b+ssb)), sqrt(sigma2), TRUE), na.rm=TRUE)

  ## initial parameter values
  initial <- structure(function(rec, ssb)
		{
			a <- max(rec, na.rm=TRUE) + 0.1 * (max(rec, na.rm=TRUE) - min(rec, na.rm=TRUE))
			b <- 0.5 * min(ssb, na.rm=TRUE)
			sigma2 <- var(log(rec /( a * ssb / (b + ssb))), y= NULL, na.rm = TRUE) 	
			return(list(a=a, b=b, sigma2=sigma2))
		},

  ## bounds
  lower=c(0, 0.0001, 0.0001),
	upper=rep(Inf, 3))

  ## model to be fitted
  model  <- rec~a*ssb/(b+ssb)
  
	return(list(logl=logl, model=model, initial=initial))
} # }}}

# Depensatory Ricker {{{
ricker.d<-function () 
    {
    logl <- function(a, b, c, sigma2, rec, ssb)
      sum(dnorm(log(rec), log(a * (ssb^c) * exp(-b * ssb)), sqrt(sigma2), TRUE),
        na.rm=TRUE)
    
    initial <- structure(function(rec, ssb) {
        x <- ssb
        y <- log(rec/ssb)
        sx <- sum(x, na.rm=TRUE)
        sy <- sum(y, na.rm=TRUE)
        sxx <- sum(x * x)
        sxy <- sum(x * y)
        s2x <- sx * sx
        sxsy <- sx * sy
        b <- -(length(ssb) * sxy - sxsy)/(length(ssb) * sxx - 
            s2x)
        b <- b + b/10
        a <- exp(sum(y, na.rm=TRUE)/length(ssb) + b * (sum(x, na.rm=TRUE)/length(ssb)))
        a <- a + a/10
        c <- 1
        return(list(a = a, b = b, c=c, sigma2 = var(log(rec) - log(a * 
            (ssb^c) * exp(-b * ssb)))))
    }, lower = c(1e-08, 1e-08,1e-08, 1e-08), upper = rep(Inf, 4))
    
    model <- rec ~ a * (ssb^c) * exp(-b * ssb)
    
    return(list(logl = logl, model = model, initial = initial))
    }   # }}}

# Ricker with covariate  {{{
ricker.c.a<-function () 
  {
    logl <- function(a, b, c, sigma2, rec, ssb, covar)
      sum(dnorm(log(rec), log(a*(1-c*covar) * ssb * exp(-b * ssb)), sqrt(sigma2),
        TRUE), na.rm=TRUE)
    initial <- structure(function(rec, ssb, covar) {
        x <- ssb
        y <- log(rec/ssb)
        sx <- sum(x, na.rm=TRUE)
        sy <- sum(y, na.rm=TRUE)
        sxx <- sum(x * x)
        sxy <- sum(x * y)
        s2x <- sx * sx
        sxsy <- sx * sy
        b <- -(length(ssb) * sxy - sxsy)/(length(ssb) * sxx - 
            s2x)
        b <- b + b/10
        a <- exp(sum(y, na.rm=TRUE)/length(ssb) + b * (sum(x, na.rm=TRUE)/length(ssb)))
        a <- a + a/10
        c <- 0
        return(list(a = a, b = b, c=c, sigma2 = var(log(rec) - log(a*(1-c*covar) * 
            ssb * exp(-b * ssb)))))
    }, lower = c(1e-08, 1e-08, 1e-08, 1e-08), upper = rep(Inf, 4))
    model <- rec ~ a*(1-c*covar) * ssb * exp(-b * ssb)
    return(list(logl = logl, model = model, initial = initial))
  }

ricker.c.b<-function () 
  {
    logl <- function(a, b, c, sigma2, rec, ssb, covar)
      sum(dnorm(log(rec), log(a * ssb * exp(-b*(1-c*covar) * ssb)), sqrt(sigma2),
        TRUE), na.rm=TRUE)
    initial <- structure(function(rec, ssb, covar) {
        x <- ssb
        y <- log(rec/ssb)
        sx <- sum(x, na.rm=TRUE)
        sy <- sum(y, na.rm=TRUE)
        sxx <- sum(x * x)
        sxy <- sum(x * y)
        s2x <- sx * sx
        sxsy <- sx * sy
        b <- -(length(ssb) * sxy - sxsy)/(length(ssb) * sxx - 
            s2x)
        b <- b + b/10
        a <- exp(sum(y, na.rm=TRUE)/length(ssb) + b * (sum(x, na.rm=TRUE)/length(ssb)))
        a <- a + a/10
        c <- 0
        return(list(a = a, b = b, c=c, sigma2 = var(log(rec) - log(a * 
            ssb * exp(-b*(1-c*covar) * ssb)))))
    }, lower = c(1e-08, 1e-08, 1e-08, 1e-08), upper = rep(Inf, 4))
    model <- rec ~ a * ssb * exp(-b*(1-c*covar) * ssb)
    return(list(logl = logl, model = model, initial = initial))
  } # }}}

# Ricker parameterised for steepness & virgin biomass {{{
Ricker.SV <- function (steepness, vbiomass, spr0, ssb) 
{
  b <- log(5 * steepness) / (vbiomass * 0.8)
  a <- exp(b * vbiomass) / spr0
  return(a * ssb * exp(-b* ssb))      
}

ricker.sv <- function()
{
  logl <- function(steepness, vbiomass, spr0, sigma2, rec, ssb)
    sum(dnorm(log(rec), log(Ricker.SV(steepness, vbiomass, spr0, ssb)), sqrt(sigma2),
    TRUE), na.rm=TRUE)

  initial <- structure(function(rec, ssb) {
    return(list(steepness = .5, vbiomass = mean(as.vector(ssb))*2, spr0=spr0,
      sigma2 = 0.3))
  }, lower = c(.1, 1e-08, 1e-08, 1e-08), upper = c(5, rep(Inf, 3)))

  model <- rec ~ Ricker.SV(steepness, vbiomass, spr0, ssb)

  return(list(logl = logl, model = model, initial = initial))
} # }}}

# Bevholt {{{
bevholt.d<-function() 
    {
    logl <- function(a, b, c, sigma2, rec, ssb)
      sum(dnorm(log(rec), log(a * ssb^c/(b + ssb^c)), sqrt(sigma2), TRUE), na.rm=TRUE)
    
    initial <- structure(
      function(rec, ssb) 
        {
        a <- max(rec, na.rm=TRUE) + 0.1 * (max(rec, na.rm=TRUE) - min(rec, na.rm=TRUE))
        b <- 0.5 * min(ssb, na.rm=TRUE)
        c <- 1.0
        sigma2 <- var(log(rec/(a * ssb^c/(b + ssb^c))), y = NULL, 
            na.rm = TRUE)
        return(list(a = a, b = b, c = c, sigma2 = sigma2))
        }, 
      
      lower = c(0, 1e-08, 1e-08, 1e-08), upper = rep(Inf, 4)
      )
    
    model <- rec ~ a * ssb^c/(b + ssb^c)
    
    return(list(logl = logl, model = model, initial = initial))
    } # }}}

# Bevholt NDC {{{
bevholt.ndc<-function() 
    {
    logl <- function(a, b, c, sigma2, rec, ssb)
      sum(dnorm(log(rec), log(a * ssb*(1+c)/(b + ssb*(1+c))), sqrt(sigma2), TRUE),
        na.rm=TRUE)
    
    initial <- structure(
      function(rec, ssb) 
        {
        a <- max(rec, na.rm=TRUE) + 0.1 * (max(rec, na.rm=TRUE) - min(rec, na.rm=TRUE))
        b <- 0.5 * min(ssb, na.rm=TRUE)
        c <- 0.0
        sigma2 <- var(log(rec/(a * ssb*(1+c)/(b + ssb*(1+c)))), y = NULL, 
            na.rm = TRUE)
        return(list(a = a, b = b, c = c, sigma2 = sigma2))
        }, 
      
      lower = c(1e-08, 1e-08, -0.5, 1e-08), upper = c(Inf,Inf,0.5,Inf)
      )
    
    model <- rec ~ a * ssb*(1+c)/(b + ssb*(1+c))
    
    return(list(logl = logl, model = model, initial = initial))
    } # }}}

# Bevholt covariates {{{
bevholt.c.b<-function() 
    {
    logl <- function(a, b, c, sigma2, rec, ssb, covar)
      sum(dnorm(log(rec), log(a*ssb/(b*(1-c*covar) + ssb)), sqrt(sigma2), TRUE),
        na.rm=TRUE)
    
    initial <- structure(
      function(rec, ssb, covar) 
        {
        a <- max(rec, na.rm=TRUE) + 0.1 * (max(rec, na.rm=TRUE) - min(rec, na.rm=TRUE))
        b <- 0.5 * min(ssb)
        c <- 0.0
        sigma2 <- var(log(rec/(a * (1-c*covar)*ssb/(b + ssb))), y = NULL, 
            na.rm = TRUE)
        return(list(a = a, b = b, c = c, sigma2 = sigma2))
        }, 
      
      lower = c(1e-08, 1e-08, -10, 1e-08), upper = rep(Inf, 4)
      )
    
    model <- rec ~ a*ssb/(b*(1-c*covar) + ssb)
    
    return(list(logl = logl, model = model, initial = initial))
    }

bevholt.c.a<-function() 
    {
    logl <- function(a, b, c, sigma2, rec, ssb, covar)
      sum(dnorm(log(rec), log(a*(1-c*covar) * ssb/(b + ssb)), sqrt(sigma2), TRUE),
        na.rm=TRUE)
    
    initial <- structure(
      function(rec, ssb, covar) 
        {
        a <- max(rec, na.rm=TRUE) + 0.1 * (max(rec, na.rm=TRUE) - min(rec, na.rm=TRUE))
        b <- 0.5 * min(ssb, na.rm=TRUE)
        c <- 0.0
        sigma2 <- var(log(rec/(a*(1-c*covar) * ssb/(b + ssb))), y = NULL, 
            na.rm = TRUE)
        return(list(a = a, b = b, c = c, sigma2 = sigma2))
        }, 
      
      lower = c(1e-08, 1e-08, -1, 1e-08), upper = c(Inf,Inf,1,Inf)
      )
    
    model <- rec ~ a*(1-c*covar) * ssb/(b + ssb)
    
    return(list(logl = logl, model = model, initial = initial))
    } # }}}
    
# bevholt parameterised for steepness & virgin biomass  {{{
Bevholt.SV<-function(s,v,spr0,ssb)
    {
    param<-sv2ab(s,v,spr0,"bevholt")

    return(param["a"] * ssb/(param["b"] + ssb))
    }
    
bevholt.sv<-function()
    {
    logl <- function(s, v, spr0, sigma2, rec, ssb)
       sum(dnorm(log(rec), log(Bevholt.SV(s,v,spr0,ssb)), sqrt(sigma2), TRUE),
        na.rm=TRUE)

    initial <- structure(function(rec, ssb) {
        return(list(s = .75, v = mean(as.vector(ssb), na.rm=TRUE)*2, spr0=1,
          sigma2 = 0.3))
    }, lower = c(.21,rep(1e-08, 3)), upper = c(1,rep(Inf, 3)))

    model <- rec ~ Bevholt.SV(s,v,spr0,ssb)

    return(list(logl = logl, model = model, initial = initial))
    } # }}}

# Geomean {{{
geomean<-function() 
    {
    logl <- function(a, sigma2, rec)
      sum(dnorm(log(rec), log(rep(a, length(rec))), sqrt(sigma2), TRUE), na.rm=TRUE)
    
    initial <- structure(function(rec) {
        a     <- exp(mean(log(rec), na.rm=TRUE))
        sigma2 <- var(log(rec/a), na.rm = TRUE)
        return(list(a = a, sigma2 = sigma2))
        }, 
        lower = c(1e-08, 1e-08), upper = rep(Inf, 2))
    
    model <- rec ~ a
    
    return(list(logl = logl, model = model, initial = initial))
    } # }}}

# logl.ar1  {{{
logl.ar1<-function(rho,sigma2,obs,hat)
  {
  n        <-length(obs)
  s2       <-sum((obs[,-1] - rho*obs[,-n] - hat[,-1] + rho*hat[,-n])^2)
  s1       <-(1-rho^2)*(obs[,1]-hat[,1])^2 + s2
  sigma2.a <-(1-rho^2)*sigma2
  res      <-(log(1/(2*pi))-n*log(sigma2.a)+log(1-rho^2)-s1/(2*sigma2.a))/2

  return(res)
  } # }}}

# bevholt AR1  {{{
bevholt.ar1 <- function()
  {
  ## log likelihood, assuming normal log.
  logl <- function(a, b, rho, sigma2, rec, ssb)
     {
     hat<-a*+ssb/(b+ssb)

     return(logl.ar1(rho,sigma2,log(rec),log(hat)))
     }

  ## initial parameter values
  initial <- structure(function(rec, ssb)
	  	{
			a <- max(rec, na.rm=TRUE) + 0.1 * (max(rec, na.rm=TRUE) - min(rec, na.rm=TRUE))
			b <- 0.5 * min(ssb, na.rm=TRUE)
      rho<-0.0
			sigma2 <- var(log(rec /( a * ssb / (b + ssb))), y= NULL, na.rm = TRUE)
			return(list(a=a, b=b, rho=rho, sigma2=sigma2))
  		},

  ## bounds
  lower=c(0, 1e-8, -0.5, 1e-8),
	upper=c(Inf,Inf,0.5,Inf))

  ## model to be fitted
  model  <- rec~a*ssb/(b+ssb)

	return(list(logl=logl, model=model, initial=initial))
} #}}}

# Ricker AR1  {{{
ricker.ar1<-function()
    {
    logl <- function(a, b, rho, sigma2, rec, ssb) 
       {
       hat<-a*log(ssb)*exp(-b*log(ssb))
           
       return(logl.ar1(rho,sigma2,log(rec),log(hat)))
       }
    
    initial <- structure(function(rec, ssb) {
        x <- ssb
        y <- log(rec/ssb)
        sx <- sum(x, na.rm=TRUE)
        sy <- sum(y, na.rm=TRUE)
        sxx <- sum(x * x)
        sxy <- sum(x * y)
        s2x <- sx * sx
        sxsy <- sx * sy
        b <- -(length(ssb) * sxy - sxsy)/(length(ssb) * sxx - 
            s2x)
        b <- b + b/10
        a <- exp(sum(y, na.rm=TRUE)/length(ssb) + b * (sum(x, na.rm=TRUE)/length(ssb)))
        a <- a + a/10
        rho<-0
    
        return(list(a = a, b = b, rho=rho, sigma2 = var(log(rec) - log(a * 
            ssb * exp(-b * ssb)))))
        }, 
        lower = c(1e-08, 1e-08, -1.0, 1e-08), upper = c(Inf,Inf,1.0,Inf))
    
    model <- rec ~ a * ssb * exp(-b * ssb)
    
    return(list(logl = logl, model = model, initial = initial))
    } # }}}

# segreg  {{{
segreg <- function()
{
	logl <- function(a, b, sigma2, rec, ssb)
	  # The actual minus log-likelihood
	  sum(dnorm(log(rec), log(FLQuant(ifelse(ssb <= b, a*ssb, a*b))), sqrt(sigma2), TRUE), TRUE)

  model <- rec ~ FLQuant(ifelse((ssb*a) <= (b/a), a * ssb, a * b))

  initial <- structure(function(rec, ssb)
  {
    a <- mean(rec/ssb)
    b <- mean(ssb)
    sigma2 <- var(log(rec/ifelse(ssb <= b, a*ssb, a*b)), y=NULL, na.rm=TRUE)
    return(list(a=a, b=b, sigma2=sigma2))
  },
    lower=rep(0.0001, 3),
    upper=rep(Inf, 3))

	return(list(logl=logl, model=model, initial=initial))
} # }}}

# Sheperd {{{
shepherd<-function()
    {
    logl <- function(a, b, c, sigma2, rec, ssb)
      sum(dnorm(log(rec), log(a * ssb/(1 + (ssb/b)^c)), sqrt(sigma2), TRUE), na.rm=TRUE)

    initial <- structure(
      function(rec, ssb)
        {
        a <- mean(rec/ssb,na.rm=T)
        b <- mean(ssb,na.rm=T)
        c <- 1.0
        sigma2 <- var(log(rec/(a * ssb/(1 + (ssb/b)^c))), y = NULL,
            na.rm = TRUE)
        return(list(a = a, b = b, c = c, sigma2 = sigma2))
        },

      lower = c(0, 1e-08, 1, 1e-08), upper = c(Inf,Inf,4,Inf)
      )

    model <- rec ~ a * ssb/(1 + (ssb/b)^c)

    return(list(logl = logl, model = model, initial = initial))
    } # }}}

# Sheperd {{{
shepherd.d<-function()
    {
    logl <- function(a, b, c, sigma2, rec, ssb)
      sum(dnorm(log(rec), log(a * ssb^2/(1 + (ssb/b)^c)), sqrt(sigma2), TRUE), na.rm=TRUE)

    initial <- structure(
      function(rec, ssb)
        {
        a <- mean(rec/ssb,na.rm=T)
        b <- mean(ssb,na.rm=T)
        c <- 1.0
        sigma2 <- var(log(rec/(a * ssb^2/(1 + (ssb/b)^c))), y = NULL,
            na.rm = TRUE)
        return(list(a = a, b = b, c = c, sigma2 = sigma2))
        },

      lower = c(0, 1e-08, 1, 1e-08), upper = c(Inf,Inf,4,Inf)
      )

    model <- rec ~ a * ssb^2/(1 + (ssb/b)^c)

    return(list(logl = logl, model = model, initial = initial))
    } # }}}

# Sheperd {{{
shepherd.ndc<-function()
    {
    logl <- function(a, b, c, d, sigma2, rec, ssb)
      sum(dnorm(log(rec), log(a * (ssb-d)/(1 + ((ssb-d)/b)^c)), sqrt(sigma2), TRUE), na.rm=TRUE)

    initial <- structure(
      function(rec, ssb)
        {
        a <- mean(rec/ssb,na.rm=T)
        b <- mean(ssb,na.rm=T)
        c <- 1.0
        sigma2 <- var(log(rec/(a * ssb/(1 + (ssb/b)^c))), y = NULL,
            na.rm = TRUE)
        return(list(a = a, b = b, c = c, d=0, sigma2 = sigma2))
        },

      lower = c(0, 1e-08, 1, -1.5, 1e-08), upper = c(Inf,Inf,4,0.5,Inf)
      )

    model <- rec ~ a * (ssb-d)/(1 + ((ssb-d)/b)^c)

    return(list(logl = logl, model = model, initial = initial))
    } # }}}

# Sheperd.ar1 {{{
shepherd.ar1<-function()
    {
    logl <- function(a,b,c,rho, sigma2, rec, ssb)
       {
       hat<-a * ssb/(1 + (ssb/b)^c)

       return(logl.ar1(rho,sigma2,log(rec),log(hat)))
       }

    initial <- structure(
      function(rec, ssb)
        {
        a <- mean(rec/ssb,na.rm=T)
        b <- mean(ssb,na.rm=T)
        c <- 1.0
        rho<-0.0

        sigma2 <- var(log(rec/(a * ssb/(1 + (ssb/b)^c))), y = NULL,
            na.rm = TRUE)
        return(list(a = a, b = b, c = c, rho=rho, sigma2 = sigma2))
        },

      lower = c(0, 1e-08, 1, -0.9, 1e-08), upper = c(Inf,Inf,4,0.9,Inf)
      )

    model <- rec ~ a * ssb/(1 + (ssb/b)^c)

    return(list(logl = logl, model = model, initial = initial))
    } # }}}

# Sheperd.d.ar1 {{{
shepherd.d.ar1<-function()
    {
    logl <- function(a,b,c,rho, sigma2, rec, ssb)
       {
       hat<-a * ssb^2/(1 + (ssb/b)^c)

       return(logl.ar1(rho,sigma2,log(rec),log(hat)))
       }

    initial <- structure(
      function(rec, ssb)
        {
        a <- mean(rec/ssb,na.rm=T)
        b <- mean(ssb,na.rm=T)
        c <- 1.0
        rho<-0.0

        sigma2 <- var(log(rec/(a * ssb^2/(1 + (ssb/b)^c))), y = NULL,
            na.rm = TRUE)
        return(list(a = a, b = b, c = c, rho=rho, sigma2 = sigma2))
        },

      lower = c(0, 1e-08, 1, -0.9, 1e-08), upper = c(Inf,Inf,4,0.9,Inf)
      )

    model <- rec ~ a * ssb^2/(1 + (ssb/b)^c)

    return(list(logl = logl, model = model, initial = initial))
    } # }}}

# Sheperd.ar1 {{{
shepherd.ndc.ar1<-function()
    {
    logl <- function(a,b,c,d, rho, sigma2, rec, ssb)
       {
       hat<-a * (ssb-d)/(1 + ((ssb-d)/b)^c)
       return(logl.ar1(rho,sigma2,log(rec),log(hat)))
       }

    initial <- structure(
      function(rec, ssb)
        {
        a <- mean(rec/ssb,na.rm=T)
        b <- mean(ssb,na.rm=T)
        c <- 1.0
        sigma2 <- var(log(rec/(a * ssb/(1 + (ssb/b)^c))), y = NULL,
            na.rm = TRUE)
        return(list(a = a, b = b, c = c, d=0, rho=0, sigma2 = sigma2))
        },

      lower = c(0, 1e-08, 1, -1.5, -0.9, 1e-08), upper = c(Inf,Inf,4,0.5,0.9,Inf)
      )

    model <- rec ~ a * (ssb-d)/(1 + ((ssb-d)/b)^c)

    return(list(logl = logl, model = model, initial = initial))
    } # }}}
