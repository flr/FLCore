\name{SRModels}
\alias{SRModels}
\alias{ab2sv}
\alias{bevholt}
\alias{bevholt.ar1}
\alias{bevholt.c.a}
\alias{bevholt.c.b}
\alias{bevholt.d}
\alias{bevholt.ndc}
\alias{bevholt.sv}
\alias{Bevholt.SV}
\alias{geomean}
\alias{logl.ar1}
\alias{ricker}
\alias{ricker.ar1}
\alias{ricker.c.a}
\alias{ricker.c.b}
\alias{ricker.d}
\alias{ricker.sv}
\alias{Ricker.SV}
\alias{segreg}
\alias{shepherd}
\alias{shepherd.ar1}
\alias{shepherd.d}
\alias{shepherd.d.ar1}
\alias{shepherd.ndc}
\alias{shepherd.ndc.ar1}
\alias{sv2ab}

\title{Stock-Recruitment models}
\description{
  A range of stock-recruitment (SR) models commonly used in fisheries science are
  provided in FLCore.
  
  Each method is defined as a function returning a list with one or more elements as
  follows:
  \item{model}{Formula for the model, using the slot names (\emph{rec} and \emph{ssb}) to 
  refer to the usual inputs.}
  \item{logl}{Function to calculate the loglikelihood of the given model when estimated
    through MLE (See \code{\link{fmle}}).}
  \item{initial}{Function to provide initial values for all parameters to be minimization
    algorithms called by \code{\link{fmle}} or \code{\link[stats]{nls}}. If required, this function also have 
    two attributes, \code{\link{lower}} and \code{\link{upper}}, that give lower and upper limits for 
    the parameter values, respectively. This is used by some of the methods defined in
    \code{\link[stats]{optim}}, like \code{"L-BFGS-B"}.}
  The \emph{model<-} method for \code{\link{FLModel-class}} can then be called with 
  \emph{value} being a list thus described, the name of the function returning such a 
  list, or the function itself. See the examples below.
}
\usage{
  bevholt()
  bevholt.ar1()
  bevholt.c.a()
  bevholt.c.b()
	bevholt.d()
	bevholt.ndc()
	bevholt.sv()
	geomean()
	logl.ar1(rho, sigma2, obs, hat)
	ricker()
	ricker.ar1()
	ricker.c.a()
	ricker.c.b()
	ricker.d()
	ricker.sv()
	segreg()
	shepherd()
	shepherd.ar1()
	shepherd.d()
	shepherd.d.ar1()
	shepherd.ndc()
	shepherd.ndc.ar1()
	sv2ab(steepness, vbiomass, spr0, model)
}
\arguments{
        \item{rho}{Autoregression}
        \item{sigma2}{Autoregression}
        \item{obs}{Observed values}
        \item{hat}{estimated values}
        \item{steepness}{Steepness.}
        \item{vbiomass}{Virgin biomass.}
        \item{spr0}{Spawners per recruit at F=0, see \code{\link{spr0}}.}
        \item{model}{character vector with model name, either 'bevholt' or 'ricker'.}
}
\details{          
    
    Several functions to fit commonly used SR models are available. They all use
    maximum likelihood to estimate the parameters and except the autoregressive models, 
    all use a normal log distribution for the residuals. Apart from the model equation 
    parameters the variance of the log-residuals is also estimated.     
   
    % bevholt
    \item{bevholt:}{Beverton \& Holt stock-recruitment model fit:
      \deqn{R = \frac{a S}{b + S}}{R = alpha*S / (beta + S)}
      'a' is the maximum recruitment (asymptotically) and 'b' is the
      stock level needed to produce the half of maximum recruitment
      \eqn{\frac{a}{2}}{a/2}. (a, b >0).}

    % geomean
     \item{geomean:}{Constant Recruitment model fit, equal to the historical
      geometric mean recruitment.
      \deqn{(R_1 R_2 \ldots R_n)^{1/n} = e^{\mathbf{mean}(\log(R_1),\ldots ,
      \log(R_n)))}}{R = (R_1*R_2*...*R_n)^(1/n) = exp(mean(log(R_1) + ... +
      log(R_n)))}}

    % ricker
    \item{ricker:}{Ricker stock-recruitment model fit:
      \deqn{R = a S e^{-b S}}{R = a*S*exp(-b*S)}                        
      'a' is related to productivity and 'b' to density dependence.         
      'a' is the recruit per stock unit at small stock levels. (a, b > 0).} 
    
    % segreg
    \item{segreg:}{Segmented regression stock-recruitment model fit:
      \deqn{R = \mathbf{ifelse}(S \leq b, a S, a b)}{ R = ifelse(S <= b, a*S, a*b)}
      'a' is the slope of the recruitment for stock levels below 'b',
      and \eqn{a b}{a*b} is the mean recruitment for stock levels above 'b'. (a, b > 0).}

    % shepherd
    \item{shepherd:}{Sheperd stock-recruitment model fit:
      \deqn{R = \frac{a S}{1+(\frac{S}{b})^c}}{ R = a * S/(1 + (S/b)^c)}}
    Generalized Beverton \& Holt and Ricker models with one covariate. The covariate
    can be used for example to account for an enviromental factor that influences the
    recruitment dynamics. In the equations 'c' is the shape parameter and 'X' is the
    covariate.
    
    % bevholt.c.a
    \item{bevholt.c.a:}{Beverton \& Holt stock-recruitment model with one covariate in
      the numerator.
      \deqn{R = \frac{a (1 - c X) S}{b + S}}{R = a*(1 - c*X)*S / (b + S)}} 
   
    % bevholt.c.b
    \item{bevholt.c.b:}{Beverton \& Holt stock-recruitment model with one covariate
      in thedenominator.
      \deqn{R = \frac{a S}{b (1 - c X) + S}}{R = a*S / (b*(1 - c*X) + S)}}
    
    % ricker.c.a
    \item{ricker.c.a:}{Ricker stock-recruitment model with one multiplicative covariate. 
            \deqn{R = a (1- c X) S e^{-b S}}{R = a*(1-c*X)*S*e^{-b*S}}}

    % ricker.c.b
    \item{ricker.c.b:}{Ricker stock-recruitment model with one covariate in the exponent. 
            \deqn{R = a S e^{-b (1- c X) S}}{R = a*S*e^{-b*(1-c*X)*S}}} 
   
    Depensatory models for Beverton and Holt, Ricker and Shepherd models:
    
    % bevholt.d
    \item{bevholt.d:}{Depensatory Beverton \& Holt stock-recruitment model
        \deqn{R = \frac{a S^c}{b + S^c}}{R = a*S^c / (b + S^c)}
        0< a, b, c < 'Inf'.}

    % bevholt.ndc
    \item{bevholt.ndc:}{Depensatory Beverton \& Holt stock-recruitment model
        \deqn{R = \frac{a S^{1+c}}{b + S^{1+c}}}{R = a*S^(1 + c) / (b + S^(1 + c))}
        0< a, b< 'Inf' and -0.5 < c < 0.5.}

    % ricker.d
    \item{ricker.d:}{Depensatory Ricker stock-recruitment model. 
         \deqn{R = a S^c e^{-b S}}{R = a*S^c*e^{-b*S}}}

    % shepherd.d
    \item{shepherd.d:}{Depensatory Shepherd stock-recruitment model. 
        \deqn{R = \frac{a S^2}{1+(\frac{S}{b})^c}}{ R = a * S^2/(1 + (S /b)^c)}}

    % shepherd.ndc
    \item{shepherd.ndc:}{Depensatory Shepherd stock-recruitment model. 
        \deqn{R = \frac{a (S - d)}{1+(\frac{S - d}{b})^c}}{ R = a * (S - d)/(1 + ((S - d)
        /b)^c)}}
     
   Beverton and Holt and Ricker stock recruitment models parameterized for steepness and virgin biomass:
    
    % bevholt.sv
    \item{bevholt.sv:}{Fits Beverton and Holt stock-recruitment model parameterised for
      steepness and virgin biomass.
      \deqn{a = \frac{4 \cdot vbiomass \cdot steepness}{(spr0 \cdot (5 \cdot steepness-
      1.0))}}{a = vbiomass*4*steepness/(spr0*(5*steepness-1.0))}
      \deqn{b = \frac{a\cdot spr0 (\frac{1.0}{steepness} - 1.0)}{4.0}}{b = a*spr0*(1.0
      /steepness - 1.0)/4.0}}        
    % ricker.sv
    \item{ricker.sv:}{Fits a ricker stock-recruitment model parameterized for steepness
      and virgin biomass.
      \deqn{a = e^{\frac{b \cdot vbiomass}{spro}}}{a = exp(b*vbiomass)/spr0}
      \deqn{b = \frac{\log(5 \cdot steepness)}{0.8 \cdot vbiomass}}{b = log(5*steepness)
      /(vbiomass*0.8)}}
    
    Models fitted using autoregressive residuals of first order:
    
    %
    \item{bevholt.ar1, ricker.ar1, shepherd.ar1, shepherd.d.ar1,
      shepherd.ndc.ar1:}{Beverton and Holt, Ricker  and Shepherd stock-recruitment models 
      with autoregressive normal log residuals of first order. In the model fit the 
      corresponding stock-recruitment model is combined with an autoregressive normal log
      likelihood of first order for the residuals. If \eqn{R_t}{R_t} i
      the observed recruitment and \eqn{\hat{R}_t}{Rest_t}  is the predicted recruitment,
      an autoregressive model of first order is fitted to the log-residuals,
      \eqn{x_t = \log(\frac{R_t}{\hat{R}_t})}{x_t = log(R_t/Rest_t)}.
      \deqn{x_t=\rho x_{t-1} + e}{x_t = rho*x_t-1 + e}
      Where 'e' follows a normal distribution with mean 0,
      \eqn{e \sim N(0, \sigma^2_{AR})}{e ~ N(0, sigma_ar^2)}.} 
              
    Some additional useful functions:
    
      \item{Bevholt.SV, Ricker.SV:}{Returns the predicted values using Beverton and Holt 
        or Ricker stock recruitment model parameterized for steepness and virgin biomass. 
        The function calculates the 'a' and 'b' parameters of the original 
        parameterization for these models for a given steepness and virgin biomass values
        using 'sv2ab' function. This then uses the original parameterization to calculate 
        the predicted recruiment.}
   
      \item{sv2ab:}{Calculates 'a' and 'b' parameters for Beverton and Holt stock
        recruitment models from the corresponding steepness, virgin biomass and spr0 
        parameters, (spr0 = spawning at recruit at F= 0.0parameters).
	
        Beverton and Holt:
          \deqn{a =  \frac{4 \cdot vbiomass \cdot steepness}{spr0*(5*steepness-1)}}
          {a = vbiomass*4*steepness/(spr0*(5*steepness-1))}
          \deqn{b = \frac{a \cdot spr0 (\frac{1}{steepness} - 1)}{4}}
          {b = a*spr0*(1/steepness - 1)/4}
                
        Ricker:
          \deqn{a = e^{\frac{b \cdot vbiomass}{spro}}}{a = exp(b*vbiomass)/spr0}
          \deqn{b = \frac{\log(5 \cdot steepness)}{0.8 \cdot vbiomass}}
          {b = log(5*steepness)/(vbiomass*0.8)}}                             
    
     \item{ab2sv:}{Calculates steepness and virgin biomass parameters for Beverton and
        Holt or Ricker stock-recruitment relationship from the corresponding 'a', 'b' 
        and spr0 parameters.
        
        Beverton and Holt:
        \deqn{steepness = \frac{a \cdot spr0}{4 \cdot b + a \cdot spr0}}
        {steepness = a*spr0/(4*b+a*spr0)}
        \deqn{\frac{vbiomass = spr0 \cdot a (5 \cdot steepness - 1)}
        {4 \cdot steepness}}{vbiomass = (spr0*a*(5*steepness - 1))/(4*steepness)}
                
        Ricker:
        \deqn{steepness = 0.2 e^{0.8 \cdot b \cdot vbiomass}}
        {steepness = 0.2*exp(b*vbiomass*0.8)}
        \deqn{vbiomass = \frac{\log(spr0 \cdot a)}{b}}{vbiomass = log(spr0*a)/b}} 
  
      \item{logl.ar1:}{Log-Likelihood of autoregressive residuals time series of first
        order. If \eqn{Y_t}{Y_t} is the observed time series and \eqn{\hat{Y}_t}{Yest_t}
       is the predicted time series using certain model, the loglikelihood of the
       residuals (\eqn{X_t = Y_t - \hat{Y}_t}{X_t = Y_t - Yest_t}) is calculated using
       the autoregressive model parameters \eqn{\sigma^2}{sigma2}  and \eqn{\rho}{rho}.}
}
\seealso{
    \link{FLSR-class}, \link{FLModel-class}
}
\examples{
# inspect the output of one of the model functions
bevholt()
names(bevholt())
bevholt()$logl

# once an FLSR model is in the workspace ...
data(nsher)

# the three model-definition slots can be modified
# at once by calling 'model<-' with
# (1) a list
model(nsher) <- bevholt()

# (2) a function returning such a list
model(nsher) <- bevholt

# or (3) the name of such a function
model(nsher) <- 'bevholt'
}
\references{
    Beverton, R.J.H. and Holt, S.J. (1957) On the dynamics of exploited
        fish populations. MAFF Fish. Invest., Ser: II 19, 533.
        
    Needle, C.L. Recruitment models: diagnosis and prognosis.   
        Reviews in Fish Biology and Fisheries 11: 95-111, 2002.
        
    Ricker, W.E. (1954) Stock and recruitment. J. Fish. Res. Bd Can. 11, 559-623.
    
    Shepherd, J.G. (1982) A versatile new stock-recruitment relationship
        for fisheries and the construction of sustainable yield curves.
        J. Cons. Int. Explor. Mer 40, 67-75.
}
\author{The FLR Team}
\keyword{models}
