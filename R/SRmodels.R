# SRmodels - Stock-recruitment models
# FLCore/R/SRmodels

# Copyright 2003-2016 FLR Team. Distributed under the GPL 2 or later
# Maintainer: Iago Mosqueira, EC JRC

#' Stock-Recruitment models
#' 
#' A range of stock-recruitment (SR) models commonly used in fisheries science
#' are provided in FLCore.
#' 
#' Each method is defined as a function returning a list with one or more
#' elements as follows:
#' - model: Formula for the model, using the slot names \emph{rec} and \emph{ssb}
#'   to refer to the usual inputs
#' - logl: Function to calculate the loglikelihood of the given model when
#'   estimated through MLE (See \code{\link{fmle}}) 
#' - initial: Function to provide initial values for all parameters to the
#'   minimization algorithms called by \code{\link{fmle}} or
#'   \code{\link[stats]{nls}}. If required, this function also has two attributes,
#'   \code{\link{lower}} and \code{\link{upper}}, that give lower and upper limits
#'   for the parameter values, respectively. This is used by some of the methods
#'   defined in \code{\link[stats]{optim}}, like \code{"L-BFGS-B"}.
#'
#' The \emph{model<-} method for \code{\linkS4class{FLModel}} can then be called
#' with \emph{value} being a list as described above, the name of the function
#' returning such a list, or the function itself. See the examples below.
#' 
#' Several functions to fit commonly-used SR models are available. They all use
#' maximum likelihood to estimate the parameters through the method
#' \code{\link{loglAR1}}.
#'
#' \itemize{
#' \item ricker: Ricker stock-recruitment model fit: \deqn{R = a S e^{-b
#' S}}{R = a*S*exp(-b*S)} \emph{a} is related to productivity (recruits per
#' stock unit at small stock size) and \emph{b} to density dependence.
#' (\emph{a, b} > 0).
#'
#' \item bevholt: Beverton-Holt stock-recruitment model
#' fit: \deqn{R = \frac{a S}{b + S}}{R = a*S / (b + S)} \emph{a} is the
#' maximum recruitment (asymptotically) and \emph{b} is the stock level needed
#' to produce the half of maximum recruitment \eqn{\frac{a}{2}}{a/2}.
#' (\emph{a, b} > 0).
#' 
#' \item segreg: Segmented regression stock-recruitment model fit:
#' \deqn{R = \mathbf{ifelse}(S \leq b, a S, a b)}{ R = ifelse(S <= b, a*S, a*b)}
#' \emph{a} is the slope of the recruitment for stock levels below \emph{b}, and
#' \eqn{a b}{a*b} is the mean recruitment for stock levels above \emph{b}.
#' (\emph{a, b} > 0).
#'
#' \item geomean: Constant recruitment model fit, equal to the
#' historical geometric mean recruitment.  \deqn{(R_1 R_2 \ldots R_n)^{1/n} =
#' e^{\mathbf{mean}(\log(R_1),\ldots , }}{R = (R_1*R_2*...*R_n)^(1/n) =
#' exp(mean(log(R_1) + ... + log(R_n)))}\deqn{ \log(R_n))}{R =
#' (R_1*R_2*...*R_n)^(1/n) = exp(mean(log(R_1) + ... + log(R_n)))}
#' 
#' \item shepherd: Shepherd stock-recruitment model fit: \deqn{R =
#' \frac{a S}{1+(\frac{S}{b})^c}}{ R = a * S/(1 + (S/b)^c)} \emph{a} represents
#' density-independent survival (similar to \emph{a} in the Ricker stock-recruit
#' model), \emph{b} the stock size above which density-dependent processes
#' predominate over density-independent ones (also referred to as the threshold
#' stock size), and \emph{c} the degree of compensation.
#'
#' \item cushing: Cushing stock-recruitment model fit: \deqn{R = a S
#' e^{b}}{R = a*S*exp(b)} This model has been used less often, and is limited
#' by the fact that it is unbounded for \emph{b}>=1 as \emph{S} increases.
#' (\emph{a, b} > 0). }
#'
#' Stock recruitment models parameterized for steepness and virgin biomass:
#'
#' \itemize{
#' \item rickerSV: Fits a ricker stock-recruitment model
#' parameterized for steepness and virgin biomass.
#' \deqn{a = e^{\frac{b \cdot vbiomass}{spr0}}}{a = exp(b*vbiomass)/spr0}
#' \deqn{b = \frac{\log(5 \cdot steepness)}{0.8 \cdot vbiomass}}{b =
#' log(5*steepness)/(0.8*vbiomass)}
#'
#' \item bevholtSV: Fits a Beverton-Holt stock-recruitment model
#' parameterised for steepness and virgin biomass.
#' \deqn{a = \frac{4 \cdot vbiomass \cdot steepness}{(spr0 \cdot (5 \cdot
#' steepness-1.0}}{a = 4*vbiomass*steepness/(spr0*(5*steepness-1.0))}
#' \deqn{b = \frac{vbiomass (1.0-steepness)}{5 \cdot steepnes-1.0}}{b =
#' vbiomass*(1.0-steepness)/(5*steepness-1.0)}
#'
#' \item sheperdSV: Fits a shepher stock-recruitment model
#' parameterized for steepness and virgin biomass.
#' \deqn{a = \frac{1.0+(\frac{vbiomass}{b})^c}{spr0}}{a = (1.0 +
#' (vbiomass/b)^c)/spr0}
#' \deqn{b = vbiomass (\frac{0.2-steepness}{steepness (0.2)^c - 0.2})^
#' (\frac{-1.0}{c})}{b = vbiomass*((0.2-steepness)/(steepness*0.2^c - 0.2))^
#' (-1.0/c)}
#' }
#'
#' Models fitted using autoregressive residuals of first order:
#'
#' \itemize{
#' \item bevholtAR1, rickerAR1, segregAR1: Beverton-Holt, Ricker and segmented
#' regression stock-recruitment models with autoregressive normal log residuals
#' of first order. In the model fit, the corresponding stock-recruit
#' model is combined with an autoregressive normal log likelihood of first order
#' for the residuals. If \eqn{R_t}{R_t} is the observed recruitment and
#' \eqn{\hat{R}_t}{Rest_t} is the predicted recruitment, an autoregressive model
#' of first order is fitted to the log-residuals, \eqn{x_t =
#' \log(\frac{R_t}{\hat{R}_t})}{x_t = log(R_t/Rest_t)}.
#' \deqn{x_t=\rho x_{t-1} + e}{x_t = rho*x_t-1 + e}
#' where \eqn{e}{e} follows a normal distribution with mean 0: \eqn{e \sim N(0,
#' \sigma^2_{AR})}{e ~ N(0, sigma_ar^2)}.
#' }
#'
#' Ricker model with one covariate. The covariate can be used, for example, to
#' account for an enviromental factor that influences the recruitment dynamics.
#' In the equations, \emph{c} is the shape parameter and \emph{X} is the
#' covariate.
#' 
#' \itemize{
#' \item rickerCa: Ricker stock-recruitment model with one
#' multiplicative covariate.
#' \deqn{R = a (1- c X) S e^{-b S}}{R = a*(1-c*X)*S*e^{-b*S}}
#' }
#'
#' @name SRModels
#' @aliases SRModels ab2sv bevholt bevholt.ar1 bevholt.c.a bevholt.c.b
#' bevholt.d bevholt.ndc bevholt.sv Bevholt.SV geomean logl.ar1 ricker
#' ricker.ar1 ricker.c.a ricker.c.b ricker.d ricker.sv Ricker.SV segreg
#' shepherd shepherd.ar1 shepherd.d shepherd.d.ar1 shepherd.ndc
#' shepherd.ndc.ar1 sv2ab
#' @param rho Autoregression
#' @param sigma2 Autoregression
#' @param obs Observed values
#' @param hat estimated values
#' @param steepness Steepness.
#' @param vbiomass Virgin biomass.
#' @param spr0 Spawners per recruit at F=0, see \code{\link{spr0}}.
#' @param model character vector with model name, either 'bevholt' or 'ricker'.
#' @author The FLR Team
#' @seealso \linkS4class{FLSR}, \linkS4class{FLModel}
#' @references Beverton, R.J.H. and Holt, S.J. (1957) On the dynamics of
#' exploited fish populations. MAFF Fish. Invest., Ser: II 19, 533.
#' 
#' Needle, C.L. Recruitment models: diagnosis and prognosis.  Reviews in Fish
#' Biology and Fisheries 11: 95-111, 2002.
#' 
#' Ricker, W.E. (1954) Stock and recruitment. J. Fish. Res. Bd Can. 11,
#' 559-623.
#' 
#' Shepherd, J.G. (1982) A versatile new stock-recruitment relationship for
#' fisheries and the construction of sustainable yield curves.  J. Cons. Int.
#' Explor. Mer 40, 67-75.
#' @keywords models
#' @examples
#' 
#' # inspect the output of one of the model functions
#'   bevholt()
#'   names(bevholt())
#'   bevholt()$logl
#' 
#' # once an FLSR model is in the workspace ...
#'   data(nsher)
#' 
#' # the three model-definition slots can be modified
#' # at once by calling 'model<-' with
#' # (1) a list
#'   model(nsher) <- bevholt()
#' 
#' # (2) the name of the function returning this list
#'   model(nsher) <- 'bevholt'
#'
#' # or (3) the function itself that returns this list
#'   model(nsher) <- bevholt
#'
NULL

# ricker {{{

#' @name SRModels
#' @aliases ricker
ricker <- function(){
  logl <- function(a, b, rec, ssb)
      loglAR1(log(rec), log(a*ssb*exp(-b*ssb)))

  initial <- structure(function(rec, ssb) {
		# The function to provide initial values
    res  <-coefficients(lm(log(c(rec)/c(ssb))~c(ssb)))
    return(FLPar(a=max(exp(res[1])), b=-max(res[2])))},
    
  # lower and upper limits for optim()
	lower=rep(-Inf, 2),
	upper=rep( Inf, 2))
	
	model  <- rec~a*ssb*exp(-b*ssb)

	return(list(logl=logl, model=model, initial=initial))}
  # }}}

# bevholt {{{

#' @name SRModels
#' @aliases bevholt
bevholt <- function()
  {
  ## log likelihood, assuming normal log.
  logl <- function(a, b, rec, ssb)
      loglAR1(log(rec), log(a*ssb/(b+ssb)))

  ## initial parameter values
  initial <- structure(function(rec, ssb) {
    a <- max(quantile(c(rec), 0.75, na.rm = TRUE))
    b <- max(quantile(c(rec)/c(ssb), 0.9, na.rm = TRUE))
    return(FLPar(a = a, b = a/b))},

  ## bounds
  lower=rep(-Inf, 2),
	upper=rep( Inf, 2))

  ## model to be fitted
  model  <- rec~a*ssb/(b+ssb)
  
	return(list(logl=logl, model=model, initial=initial))
} # }}}

# bevholtss3 {{{

#' @name SRModels
#' @aliases bevholtss3
bevholtss3 <- function() {

  ## log likelihood, assuming normal log.
  logl <- function(s, R0, v, rec, ssb)
      loglAR1(log(rec), log((4 * s * R0 * ssb) / (v * (1 - s) + ssb * (5 * s - 1))))

  ## initial parameter values
  initial <- structure(function(rec, ssb) {
    s <- 0.5
    R0 <- max(rec)
    v <- max(ssb)
    return(FLPar(s=s, R0=R0, v=v))},

  ## bounds
  lower=c(1e-8, 1e-8, 1e-8),
	upper=c(1, Inf, Inf))

  ## model to be fitted
  model  <- rec ~ (4 * s * R0 * ssb) / (v * (1 - s) + ssb * (5 * s - 1)) 
  
	return(list(logl=logl, model=model, initial=initial))
} # }}}

# segreg  {{{

#' @name SRModels
#' @aliases segreg
segreg <- function(){
	logl <- function(a, b, rec, ssb){

    loglAR1(log(rec), FLQuant(log(ifelse(c(ssb)<=b,a*c(ssb),a*b)),dimnames=dimnames(ssb)))}

  model <- rec ~ ifelse(ssb <= b, a * ssb, a * b)

  initial <- structure(function(rec, ssb){
    return(FLPar(a=median(c(rec)/c(ssb),na.rm=TRUE), b=median(c(ssb),na.rm=TRUE)))},
    lower=rep(0, 0),
    upper=rep(Inf, 2))

	return(list(logl=logl, model=model, initial=initial))
} # }}}

# geomean {{{

#' @name SRModels
#' @aliases geomean
geomean<-function() 
    {
    logl <- function(a, rec)
      loglAR1(log(rec), log(FLQuant(rep(a, length(rec)))))
    
    initial <- structure(function(rec) {
        return(FLPar(a = exp(mean(log(rec), na.rm=TRUE))))
        }, 
        lower = c(1e-08), upper = rep(Inf))
    
		# TRICK: 
    model <- rec ~ a + ssb/ssb - 1
    
    return(list(logl = logl, model = model, initial = initial))
    } # }}}

# shepherd  {{{

#' @name SRModels
#' @aliases shepherd
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
    
    lower = c(  0,   0,  1),
    upper = c(Inf, Inf, 10))

  model <- rec ~ a * ssb/(1 + (ssb/b)^c)

  return(list(logl = logl, model = model, initial = initial))
} # }}}

# cushing {{{

#' @name SRModels
#' @aliases cushing
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
  lower=c(-Inf, -Inf),
	upper=c( Inf,  Inf))
#  lower=c(0, 0.0001),
#	upper=c(Inf, 1))

  model  <- rec~a*ssb^b

	return(list(logl=logl, model=model, initial=initial))
}  # }}}

# rickerSV  {{{

#' @name SRModels
#' @aliases rickerSV
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

#' @name SRModels
#' @aliases bevholtSV
bevholtSV <- function()
  {
  logl <- function(s, v, spr0, rec, ssb)
  {
    pars <- FLPar(abPars('bevholt', s=s, v=v, spr0=spr0))
    loglAR1(log(rec), log(pars['a']%*%ssb/(pars['b']%+%ssb)))
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
  model  <- rec~FLPar(abPars('bevholt', s=s, v=v, spr0=spr0))['a']%*%ssb %/% (FLPar(abPars('bevholt', s=s, v=v, spr0=spr0))['b']%+%ssb)
  
	return(list(logl=logl, model=model, initial=initial))
} # }}}

# shepherdSV {{{

#' @name SRModels
#' @aliases shepherdSV
shepherdSV <- function()
  {
  logl <- function(s, v, spr0, c, rec, ssb)
  {
    pars <- FLPar(abPars('shepherd', s=s, v=v, spr0=spr0, c=c))
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
  model  <- rec~FLPar(abPars('shepherd', s=s, v=v, spr0=spr0, c=c))['a']*ssb /
    (1 + (ssb / FLPar(abPars('shepherd', s=s, v=v, spr0=spr0, c=c))['b']) ^ c)
  
	return(list(logl=logl, model=model, initial=initial))
} # }}}

# bevholtAR1 {{{

#' @name SRModels
#' @aliases bevholtAR1
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

# rickerAR1 {{{

#' @name SRModels
#' @aliases rickerAR1
rickerAR1 <- function()
  {
  ## log likelihood, assuming normal log.
  logl <- function(a, b, rho, rec, ssb)
      loglAR1(log(rec), log(a*ssb*exp(-b*ssb)), rho=rho)

  ## initial parameter values
  initial <- structure(function(rec, ssb) {
		# The function to provide initial values
    res  <-coefficients(lm(c(log(rec/ssb))~c(ssb)))
    return(FLPar(a=max(exp(res[1])), b=-max(res[2]), rho=0))
	},
  # lower and upper limits for optim()
	lower=c(rep(1e-10, 2), -1),
	upper=c(rep(Inf, 2), 1)
	)

  ## model to be fitted
	model  <- rec~a*ssb*exp(-b*ssb)
  
	return(list(logl=logl, model=model, initial=initial))
} # }}}

#segregAR1 {{{

#' @name SRModels
#' @aliases segregAR1
segregAR1 <- function(){
    logl <- function(a, b, rho, rec, ssb){
       loglAR1(log(rec), FLQuant(log(ifelse(c(ssb)<=b,a*c(ssb),a*b)),dimnames=dimnames(ssb)),rho=rho)}

    model <- rec ~ ifelse(ssb <= b, a * ssb, a * b)

    initial <- structure(function(rec, ssb){
      return(FLPar(a=median(c(rec/ssb),na.rm=TRUE), b=median(c(ssb),na.rm=TRUE),rho=0))},
      lower=c(0, 0, -1),
      upper=c(Inf, Inf, 1))

return(list(logl=logl, model=model, initial=initial))
} # }}}

# Ricker with covariate  {{{

#' @name SRModels
#' @aliases rickerCa
rickerCa <- function() {
  logl <- function(a, b, c, rec, ssb, covar)
    loglAR1(log(rec), log(a * (1 - c * covar) * ssb * exp(-b * ssb)))
 
  initial <- structure(function(rec, ssb) {
		# The function to provide initial values
    res  <-coefficients(lm(c(log(rec/ssb))~c(ssb)))
    return(FLPar(a=max(exp(res[1])), b=-max(res[2]), c=1))},
    
  # lower and upper limits for optim()
	lower=rep(-Inf, 3),
	upper=rep( Inf, 3))
	
	model  <- rec ~ a * (1 - c * covar) * ssb * exp(-b * ssb)
	return(list(logl=logl, model=model, initial=initial))
} # }}}

# survSRR {{{

#' @name SRModels
#' @aliases survSRR

survRec <- function(ssf, R0, Sfrac, beta, SF0=ssf[,1]) {

  z0 <- log(1 / (SF0 / R0))
  zmax <- z0 + Sfrac * (-z0)

  zsurv <- exp((1 - (ssf %/% SF0) ^ beta) %*% (zmax - z0) %+% z0)

  rec <- ssf * zsurv

  return(rec)
}

survSRR <- function() {

  ## log likelihood, assuming normal log.
  logl <- function(R0, Sfrac, beta, rec, ssf, ...)
      loglAR1(log(rec), log(survRec(ssf, R0, Sfrac, beta, ...)))

  ## initial parameter values
  initial <- structure(function(rec, ssf) {
    R0 <- max(rec)
    Sfrac <- 0.5
    beta <- 0.5
    return(FLPar(R0=R0, Sfrac=Sfrac, beta=beta))},

  ## bounds
  lower=c(1e-8, 1e-8, 1e-8),
	upper=c(Inf, 1, 1))

  ## model to be fitted
  model  <- rec ~ survRec(ssf, R0, Sfrac, beta, SF0=ssf[,1])
  
	return(list(logl=logl, model=model, initial=initial))
}


# }}}

# bevholtsig {{{
# rec = a / ((b / srp) ^c + 1)

#' @name SRModels
#' @aliases bevholtsig
bevholtsig <- function() {
  ## log likelihood, assuming normal log.
  logl <- function(a, b, c, rec, ssb)
      loglAR1(log(rec), log(a / ((b / ssb) ^ c + 1)))

  ## initial parameter values
  initial <- structure(function(rec, ssb) {
    a <- max(quantile(c(rec), 0.75, na.rm = TRUE))
    b <- max(quantile(c(rec)/c(ssb), 0.9, na.rm = TRUE))
    return(FLPar(a = a, b = a/b, c=1))},

  ## bounds
  lower=rep(-Inf, 2),
	upper=rep(Inf, 2))

  ## model to be fitted
  model  <- rec~a/((b/ssb)^c+1)
  
	return(list(logl=logl, model=model, initial=initial))
} # }}}

# mixedsrr {{{

mixed <- function(a, b, m=c(1, 2, 3), ssb) {

  rec <- ssb
  rec[] <- as.numeric(NA)

  # 1 Bevholt
  id <- c(m == 1)
  if(sum(id) > 0)
    iter(rec, id) <- iter(a, id) * iter(ssb, id) /
      (iter(b, id) + iter(ssb, id))

  # 2 Ricker
  id <- c(m == 2)
  if(sum(id) > 0)
    iter(rec, id) <- iter(a, id) * iter(ssb, id) * exp(-(iter(b, id) *
      iter(ssb, id)))

  # 3 Segreg
  id <- c(m == 3)
  if(sum(id) > 0)
    iter(rec, id) <- ifelse(iter(ssb, id) <= iter(b, id), iter(a, id) *
      iter(ssb, id), iter(a, id) * iter(b, id))

  return(rec)
}

#' @name SRModels
#' @aliases mixedsrr

mixedsrr <- function() {

  ## log likelihood, assuming normal log.
  logl <- function(a, b, m, rec, ssb)
      loglAR1(log(rec), log(mixed(a, b, m, ssb)))

  ## initial parameter values
  initial <- structure(function(rec, ssb) {
    a <- max(quantile(c(rec), 0.75, na.rm = TRUE))
    b <- max(quantile(c(rec)/c(ssb), 0.9, na.rm = TRUE))
    return(FLPar(a = a, b = a/b, c=1))},

  ## bounds
  lower=rep(-Inf, 2),
	upper=rep(Inf, 2))

  ## model to be fitted
  model  <- rec~mixed(a, b, m, ssb)
  
	return(list(logl=logl, model=model, initial=initial))
} # }}}

# methods

# spr0  {{{
## calcs spawner per recruit at F=0.0   
setMethod('spr0', signature(ssb='FLQuant', rec='FLQuant', fbar='FLQuant'),
   function(ssb, rec, fbar) {

    if  (any(dim(ssb)[3:5]>1))
      stop("multiple units, seasons, areas not allowed yet")

    if  (any(dim(rec)[3:5]>1))
      stop("multiple units, seasons, areas not allowed yet")

    if  (any(dim(fbar)[3:5]>1))
      stop("multiple units, seasons, areas not allowed yet")

    # years: corrects length if mismatch
    minyear <- max(unlist(lapply(list(fbar=fbar, ssb=ssb, rec=rec),
      function(x) min(as.numeric(dimnames(x)$year)))))
    maxyear <- min(unlist(lapply(list(fbar=fbar, ssb=ssb, rec=rec),
      function(x) max(as.numeric(dimnames(x)$year)))))

    its <- max(c(dim(ssb)[6], dim(rec)[6], dim(fbar)[6]))

    spr0 <- FLPar(NA, dimnames=list(params="spr0", iter=seq(its)))

    # ssb & f
    for(i in seq(dim(ssb)[6])) {
      ss  <- iter(ssb, i)[1, as.character(seq(minyear, maxyear)), drop=TRUE]
      re  <- iter(rec, i)[1, as.character(seq(minyear, maxyear)), drop=TRUE]
      fb <- iter(fbar, i)[1, as.character(seq(minyear, maxyear)), drop=TRUE]

      # spr0
      spr0["spr0", i] <- lm(c(ss/re)~c(fb))$coefficients[1]
    }

    return(spr0)
  }
)

setMethod('spr0', signature(ssb='FLStock', rec='missing', fbar='missing'),
 function(ssb, nyears=3) {

   return(spr0(ssb(ssb), rec(ssb), fbar(ssb)))
    
    ssb <- window(ssb, start=dims(ssb)$maxyear - nyears + 1)

    nages <- dim(ssb)[1]
    
    # FLQuant for NPR0
    npr0 <- stock.n(ssb)[,1,1,1,1]
    npr0[1] <- 1

    for(a in seq(2, nages)){
      npr0[a] <- npr0[a - 1] * exp(-mean(m(ssb)[a - 1, ]))
    }

    # ADD with plusgroup
    npr0[nages] = npr0[nages] / (1 - exp(-mean(m(ssb)[nages, ])))
    
    return(quantSums(npr0 * exp(-(apply(m(ssb), 1, mean) * apply(m.spwn(ssb), 1, mean))) *
  apply(stock.wt(ssb),1,mean) * apply(mat(ssb),1,mean)))
  }
)

setMethod('spr0', signature(ssb='FLStock', rec='missing', fbar='missing'),
  function(ssb) {

    sr <- as.FLSR(ssb)

    # spr0
    spr0(ssb=ssb(ssb), rec=rec(sr), fbar=fbar(ssb))
  }
)

setMethod('spr0', signature(ssb='FLSR', rec='missing', fbar='FLQuant'),
  function(ssb, fbar) {
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
  function(obs, hat, rho=0){

		# HACK
		units(hat) <- units(obs)

    # calculates likelihood for AR(1) process
    n   <- dim(obs)[2]
    rsdl<-(obs[,-1] - rho*obs[,-n] - hat[,-1] + rho*hat[,-n])
    
    s2  <- sum(rsdl^2, na.rm=T)
    s1  <-s2

    if (!all(is.na(rsdl[,1])))
      s1 <- s1+(1-rho^2)*(obs[,1]-hat[,1])^2

    #if (all(is.na(hat))) sigma2<-1e100 else

    sigma2   <- sigma(obs, hat)^2
    
    n        <- length(obs[!is.na(obs)])
    sigma2.a <- (1-rho^2)*sigma2
    res      <- (log(1/(2*pi))-n*log(sigma2.a)+log(1-rho^2)-s1/(2*sigma2.a))/2

    if (!is.finite(res))
      res <- -1e100

    return(res)}) 


setMethod("loglAR1", signature(obs = "numeric", hat = "numeric"),
  function(obs, hat, rho = 0) 
  {
     # calculates likelihood for AR(1) process
     n <- length(obs)
     rsdl <- (obs[-1] - rho * obs[-n] - hat[-1] + rho * hat[-n])
     s2 <- sum(rsdl^2, na.rm = T)
     s1 <- s2
     if (!all(is.na(rsdl[1]))) 
        s1 <- s1 + (1 - rho^2) * (obs[1] - hat[1])^2
     sigma2 <- sum((obs - hat)^2)
     n <- length(obs[!is.na(obs)])
     sigma2.a <- (1 - rho^2) * sigma2
     res <- (log(1/(2 * pi)) - n * log(sigma2.a) + log(1 - rho^2) - s1/(2 * sigma2.a))/2
     if (!is.finite(res)) res <- -1e+100
     return(res)}) # }}}

# SRModelName {{{
SRModelName <- function(model){
  return(switch(gsub(" ", "", as.character(as.list(model)[length(model)])),
      "a*ssb*exp(-b*ssb)"                 = "ricker",
      "a*ssb/(b+ssb)"                     = "bevholt",
      "a*ssb/(1+(ssb/b)^c)"               = "shepherd",
      "a*ssb^b"                           = "cushing",
      "ifelse(ssb<=b,a*ssb,a*b)" = "segreg",
      "FLQuant(ifelse(c(ssb)<=b,a*c(ssb),a*b),dimnames=dimnames(ssb))" = "segreg",
      "FLQuant(ifelse(c(ssb)<=c(b),c(a)*c(ssb),c(a)*c(b)),dimnames=dimnames(ssb))" = "segreg",
      "a+ssb/ssb-1"                       = "mean",
      "FLQuant(a,dimnames=dimnames(rec))" = "mean",
      "a"                                 = "mean",
      "rec"                               = "mean",
			"FLPar(abPars(\"bevholt\",s=s,v=v,spr0=spr0))[\"a\"]%*%ssb%/%(FLPar(abPars(\"bevholt\",s=s,v=v,spr0=spr0))[\"b\"]%+%ssb)" = "bevholtSV",
      'abPars("ricker",s=s,v=v,spr0=spr0)["a"]*ssb*exp(-abPars("ricker",s=s,v=v,spr0=spr0)["b"]*ssb)' = "rickerSV",
      "(4*s*R0*ssb)/(v*(1-s)+ssb*(5*s-1))" = "bevholtss3",
      "(4*s*R0*tep)/(v*(1-s)+tep*(5*s-1))" = "bevholtss3f",
      "survRec(ssf,R0,Sfrac,beta,SF0=ssf[,1])" = "survSRR",
      "a/((b/ssb)^c+1)" = "bevholtsig",
      "mixed(a,b,m,ssb)" = "mixedsrr",
      NULL))} # }}}

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
    "mixedsrr" = 44,
    "bevholtD" = 21,
    "bevholtSV" = 22,
    "bevholtSS3" = 23,
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
abPars <- function(model, spr0, s=NULL, v, c=NULL, d=NULL)
{
  # converts a & b parameterisation into steepness & virgin biomass (s & v)
  switch(model,
    "bevholt"   ={a=(v+(v-s*v)/(5*s-1))/spr0; b=(v-s*v)/(5*s-1)*spr0/spr0},
    "bevholtSV" ={a=(v+(v-s*v)/(5*s-1))/spr0; b=(v-s*v)/(5*s-1)},
    "ricker"    ={b=log(5*s)/(v*0.8); a=exp(v*b)/spr0},
    "rickerSV"  ={b=log(5*s)/(v*0.8); a=exp(v*b)/spr0},
    "cushing"   ={b=log(s)/log(0.2); a=(v^(1-b))/(spr0)},
    "cushingSV" ={b=log(s)/log(0.2); a=(v^(1-b))/(spr0)},
    "shepherd"  ={b=v*(((0.2-s)/(s*0.2^c-0.2))^-(1/c)); a=((v/b)^c+1)/spr0},
    "shepherdSV"={b=v*(((0.2-s)/(s*0.2^c-0.2))^-(1/c)); a=((v/b)^c+1)/spr0},
    "mean"      ={a=v/spr0;b=NULL},
    "meanSV"    ={a=v/spr0;b=NULL},
    "segreg"    ={a=5*s/spr0; b=v/(a*spr0)},
    "segregSV"  ={a=5*s/spr0; b=v/(a*spr0)},
    {stop("model name not recognized")})

  res <- list(a=a, b=b)

  return(res[unlist(lapply(res, function(x) !is.null(x)))])
} # }}}

# svPars {{{
svPars <- function(model, spr0, a, b=NULL, c=NULL, d=NULL)
{
  v <- spr2v(model, spr0, a, b, c, d)
  s <- srr2s(model, ssb=v*.2, a=a, b=b, c=c, d=d) / srr2s(model, ssb=v, a=a,
      b=b, c=c, d=d)
  return(
         list(s=s, v=v, spr0=spr0)
         )
}
# }}}

# abModel {{{
abModel <- function(model)
{
  if(is(model, 'formula'))
    modelname <- SRModelName(model)
  else
    modelname <- model
  res <- switch(modelname,
    'bevholtSV'='bevholt',
    'rickerSV'='ricker',
    'shepherdSV'= 'shepherd')
  if(is(model, 'formula'))
    return(do.call(res, list())$model)
  return(res)
} # }}}

# svModel {{{
svModel <- function(model)
{
  if(is(model, 'formula'))
    modelname <- SRModelName(model)
  else
    modelname <- model
  res <- switch(modelname,
    'bevholt'='bevholtSV',
    'ricker'='rickerSV',
    'shepherd'='shepherdSV')
  if(is(model, 'formula'))
    return(do.call(res, list())$model)
  return(res)
} # }}}
