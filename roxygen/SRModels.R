#' Methods SRModels
#'
#' Stock-Recruitment models
#' 
#' A range of stock-recruitment (SR) models commonly used in fisheries science
#' are provided in FLCore.
#' 
#' Each method is defined as a function returning a list with one or more
#' elements as follows: \itemize{ \itemmodelFormula for the model, using the
#' slot names \emph{rec} and \emph{ssb} to refer to the usual inputs
#' \itemloglFunction to calculate the loglikelihood of the given model when
#' estimated through MLE (See \code{\link{fmle}})  \iteminitialFunction to
#' provide initial values for all parameters to the minimization algorithms
#' called by \code{\link{fmle}} or \code{\link[stats]{nls}}. If required, this
#' function also has two attributes, \code{\link{lower}} and
#' \code{\link{upper}}, that give lower and upper limits for the parameter
#' values, respectively. This is used by some of the methods defined in
#' \code{\link[stats]{optim}}, like \code{"L-BFGS-B"}}.
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
#' % ricker \itemricker: Ricker stock-recruitment model fit: \deqn{R = a S e^{-b
#' S}}{R = a*S*exp(-b*S)} \emph{a} is related to productivity (recruits per
#' stock unit at small stock size) and \emph{b} to density dependence.
#' (\emph{a, b} > 0).
#'
#' % bevholt \itembevholt: Beverton-Holt stock-recruitment model
#' fit: \deqn{R = \frac{a S}{b + S}}{R = a*S / (b + S)} \emph{a} is the
#' maximum recruitment (asymptotically) and \emph{b} is the stock level needed
#' to produce the half of maximum recruitment \eqn{\frac{a}{2}}{a/2}.
#' (\emph{a, b} > 0).
#' 
#' % segreg \itemsegreg: Segmented regression stock-recruitment model fit:
#' \deqn{R = \mathbf{ifelse}(S \leq b, a S, a b)}{ R = ifelse(S <= b, a*S, a*b)}
#' \emph{a} is the slope of the recruitment for stock levels below \emph{b}, and
#' \eqn{a b}{a*b} is the mean recruitment for stock levels above \emph{b}.
#' (\emph{a, b} > 0).
#'
#' % geomean \itemgeomean: Constant recruitment model fit, equal to the
#' historical geometric mean recruitment.  \deqn{(R_1 R_2 \ldots R_n)^{1/n} =
#' e^{\mathbf{mean}(\log(R_1),\ldots , }{R = (R_1*R_2*...*R_n)^(1/n) =
#' exp(mean(log(R_1) + ... + log(R_n)))}\deqn{ \log(R_n)))}}{R =
#' (R_1*R_2*...*R_n)^(1/n) = exp(mean(log(R_1) + ... + log(R_n)))}
#' 
#' % shepherd \itemshepherd: Shepherd stock-recruitment model fit: \deqn{R =
#' \frac{a S}{1+(\frac{S}{b})^c}}{ R = a * S/(1 + (S/b)^c)} \emph{a} represents
#' density-independent survival (similar to \emph{a} in the Ricker stock-recruit
#' model), \emph{b} the stock size above which density-dependent processes
#' predominate over density-independent ones (also referred to as the threshold
#' stock size), and \emph{c} the degree of compensation.
#'
#' % cushing \itemcushing: Cushing stock-recruitment model fit: \deqn{R = a S
#' e^{b}}{R = a*S*exp(b)} This model has been used less often, and is limited
#' by the fact that it is unbounded for \emph{b}>=1 as \emph{S} increases.
#' (\emph{a, b} > 0). }
#'
#' Stock recruitment models parameterized for steepness and virgin biomass:
#'
#' \itemize{
#' % rickerSV \itemrickerSV: Fits a ricker stock-recruitment model
#' parameterized for steepness and virgin biomass.
#' \deqn{a = e^{\frac{b \cdot vbiomass}{spr0}}}{a = exp(b*vbiomass)/spr0}
#' \deqn{b = \frac{\log(5 \cdot steepness)}{0.8 \cdot vbiomass}}{b =
#' log(5*steepness)/(0.8*vbiomass)}
#'
#' % bevholtSV \itembevholtSV: Fits a Beverton-Holt stock-recruitment model
#' parameterised for steepness and virgin biomass.
#' \deqn{a = \frac{4 \cdot vbiomass \cdot steepness}{(spr0 \cdot (5 \cdot
#' steepness-1.0}}{a = 4*vbiomass*steepness/(spr0*(5*steepness-1.0))}
#' \deqn{b = \frac{vbiomass (1.0-steepness)}{5 \cdot steepnes-1.0}}{b =
#' vbiomass*(1.0-steepness)/(5*steepness-1.0)}
#'
#' % shepherdSV \itemsheperdSV: Fits a shepher stock-recruitment model
#' parameterized for steepness and virgin biomass.
#' \deqn{a = \frac{1.0+(\frac{vbiomass}{b})^c}{spr0}}{a = (1.0 +
#' (vbiomass/b)^c)/spr0}
#' \deqn{b = vbiomass (\frac{0.2-steepness}{steepness (0.2)^c - 0.2})^
#' (\frac{-1.0}{c})}{b = vbiomass*((0.2-steepness)/(steepness*0.2^c - 0.2))^
#' (-1.0/c)} }
#'
#' Models fitted using autoregressive residuals of first order:
#'
#' \itemize{
#' % \itembevholtAR1, rickerAR1, segregAR1: Beverton-Holt, Ricker and segmented
#' regression stock-recruitment models with autoregressive normal log residuals
#' of first order. In the model fit, the corresponding stock-recruit
#' model is combined with an autoregressive normal log likelihood of first order
#' for the residuals. If \eqn{R_t}{R_t} is the observed recruitment and
#' \eqn{\hat{R}_t}{Rest_t} is the predicted recruitment, an autoregressive model
#' of first order is fitted to the log-residuals, \eqn{x_t =
#' \log(\frac{R_t}{\hat{R}_t})}{x_t = log(R_t/Rest_t)}.
#' \deqn{x_t=\rho x_{t-1} + e}{x_t = rho*x_t-1 + e}
#' where \eqn{e}{e} follows a normal distribution with mean 0: \eqn{e \sim N(0,
#' \sigma^2_{AR})}{e ~ N(0, sigma_ar^2)}. }
#'
#' Ricker model with one covariate. The covariate can be used, for example, to
#' account for an enviromental factor that influences the recruitment dynamics.
#' In the equations, \emph{c} is the shape parameter and \emph{X} is the
#' covariate.
#' 
#' \itemize{
#' % rickerCa \itemrickerCa: Ricker stock-recruitment model with one
#' multiplicative covariate.
#' \deqn{R = a (1- c X) S e^{-b S}}{R = a*(1-c*X)*S*e^{-b*S}} }
#'
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