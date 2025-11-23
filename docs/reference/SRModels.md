<div id="main" class="col-md-9" role="main">

# Stock-Recruitment models

<div class="ref-description section level2">

A range of stock-recruitment (SR) models commonly used in fisheries
science are provided in FLCore.

</div>

<div class="section level2">

## Usage

<div class="sourceCode">

``` r
ricker()

bevholt()

bevholtDa()

bevholtss3()

segreg()

segregDa()

geomean()

shepherd()

cushing()

rickerSV()

bevholtSV()

shepherdSV()

bevholtAR1()

rickerAR1()

segregAR1()

rickerCa()

survRec(ssf, R0, Sfrac, beta, SF0 = ssf[, 1])

bevholtsig()

mixedsrr()
```

</div>

</div>

<div class="section level2">

## Arguments

-   rho:

    Autoregression

-   sigma2:

    Autoregression

-   obs:

    Observed values

-   hat:

    estimated values

-   steepness:

    Steepness.

-   vbiomass:

    Virgin biomass.

-   spr0:

    Spawners per recruit at F=0, see `spr0`.

-   model:

    character vector with model name, either 'bevholt' or 'ricker'.

</div>

<div class="section level2">

## Details

Each method is defined as a function returning a list with one or more
elements as follows:

-   model: Formula for the model, using the slot names *rec* and *ssb*
    to refer to the usual inputs

-   logl: Function to calculate the loglikelihood of the given model
    when estimated through MLE (See `fmle`)

-   initial: Function to provide initial values for all parameters to
    the minimization algorithms called by `fmle` or `nls`. If required,
    this function also has two attributes, `lower` and `upper`, that
    give lower and upper limits for the parameter values, respectively.
    This is used by some of the methods defined in `optim`, like
    `"L-BFGS-B"`.

The *model\<-* method for `FLModel` can then be called with *value*
being a list as described above, the name of the function returning such
a list, or the function itself. See the examples below.

Several functions to fit commonly-used SR models are available. They all
use maximum likelihood to estimate the parameters through the method
`loglAR1`.

-   ricker: Ricker stock-recruitment model fit: $$R = a S e^{-b S}$$ *a*
    is related to productivity (recruits per stock unit at small stock
    size) and *b* to density dependence. (*a, b* \> 0).

-   bevholt: Beverton-Holt stock-recruitment model fit: $$R = \\frac{a
    S}{b + S}$$ *a* is the maximum recruitment (asymptotically) and *b*
    is the stock level needed to produce the half of maximum recruitment
    \\(\\frac{a}{2}\\). (*a, b* \> 0).

-   segreg: Segmented regression stock-recruitment model fit: $$R =
    \\mathbf{ifelse}(S \\leq b, a S, a b)$$ *a* is the slope of the
    recruitment for stock levels below *b*, and \\(a b\\) is the mean
    recruitment for stock levels above *b*. (*a, b* \> 0).

-   geomean: Constant recruitment model fit, equal to the historical
    geometric mean recruitment. $$(R_1 R_2 \\ldots R_n)^{1/n} =
    e^{\\mathbf{mean}(\\log(R_1),\\ldots , }$$$$ \\log(R_n))$$

-   shepherd: Shepherd stock-recruitment model fit: $$R = \\frac{a
    S}{1+(\\frac{S}{b})^c}$$ *a* represents density-independent survival
    (similar to *a* in the Ricker stock-recruit model), *b* the stock
    size above which density-dependent processes predominate over
    density-independent ones (also referred to as the threshold stock
    size), and *c* the degree of compensation.

-   cushing: Cushing stock-recruitment model fit: $$R = a S e^{b}$$ This
    model has been used less often, and is limited by the fact that it
    is unbounded for *b*\>=1 as *S* increases. (*a, b* \> 0).

Stock recruitment models parameterized for steepness and virgin biomass:

-   rickerSV: Fits a ricker stock-recruitment model parameterized for
    steepness and virgin biomass. $$a = e^{\\frac{b \\cdot
    vbiomass}{spr0}}$$ $$b = \\frac{\\log(5 \\cdot steepness)}{0.8
    \\cdot vbiomass}$$

-   bevholtSV: Fits a Beverton-Holt stock-recruitment model
    parameterised for steepness and virgin biomass. $$a = \\frac{4
    \\cdot vbiomass \\cdot steepness}{(spr0 \\cdot (5 \\cdot
    steepness-1.0}$$ $$b = \\frac{vbiomass (1.0-steepness)}{5 \\cdot
    steepnes-1.0}$$

-   sheperdSV: Fits a shepher stock-recruitment model parameterized for
    steepness and virgin biomass. $$a =
    \\frac{1.0+(\\frac{vbiomass}{b})^c}{spr0}$$ $$b = vbiomass
    (\\frac{0.2-steepness}{steepness (0.2)^c - 0.2})^
    (\\frac{-1.0}{c})$$

Models fitted using autoregressive residuals of first order:

-   bevholtAR1, rickerAR1, segregAR1: Beverton-Holt, Ricker and
    segmented regression stock-recruitment models with autoregressive
    normal log residuals of first order. In the model fit, the
    corresponding stock-recruit model is combined with an autoregressive
    normal log likelihood of first order for the residuals. If \\(R_t\\)
    is the observed recruitment and \\(\\hat{R}\_t\\) is the predicted
    recruitment, an autoregressive model of first order is fitted to the
    log-residuals, \\(x_t = \\log(\\frac{R_t}{\\hat{R}\_t})\\).
    $$x_t=\\rho x\_{t-1} + e$$ where \\(e\\) follows a normal
    distribution with mean 0: \\(e \\sim N(0, \\sigma^2\_{AR})\\).

Ricker model with one covariate. The covariate can be used, for example,
to account for an enviromental factor that influences the recruitment
dynamics. In the equations, *c* is the shape parameter and *X* is the
covariate.

-   rickerCa: Ricker stock-recruitment model with one multiplicative
    covariate. $$R = a (1- c X) S e^{-b S}$$

</div>

<div class="section level2">

## References

Beverton, R.J.H. and Holt, S.J. (1957) On the dynamics of exploited fish
populations. MAFF Fish. Invest., Ser: II 19, 533.

Needle, C.L. Recruitment models: diagnosis and prognosis. Reviews in
Fish Biology and Fisheries 11: 95-111, 2002.

Ricker, W.E. (1954) Stock and recruitment. J. Fish. Res. Bd Can. 11,
559-623.

Shepherd, J.G. (1982) A versatile new stock-recruitment relationship for
fisheries and the construction of sustainable yield curves. J. Cons.
Int. Explor. Mer 40, 67-75.

</div>

<div class="section level2">

## See also

<div class="dont-index">

[FLSR](FLSR.md), [FLModel](FLModel.md)

</div>

</div>

<div class="section level2">

## Author

The FLR Team

</div>

<div class="section level2">

## Examples

<div class="sourceCode">

``` r
# inspect the output of one of the model functions
  bevholt()
#> $logl
#> function(a, b, rec, ssb)
#>       loglAR1(log(rec), log(a*ssb/(b+ssb)))
#> <bytecode: 0x563fd70c6b30>
#> <environment: 0x563fcca9da88>
#> 
#> $model
#> rec ~ a * ssb/(b + ssb)
#> <environment: 0x563fcca9da88>
#> 
#> $initial
#> function(rec, ssb) {
#>     a <- max(quantile(c(rec), 0.75, na.rm = TRUE))
#>     b <- max(quantile(c(rec)/c(ssb), 0.9, na.rm = TRUE))
#>     return(FLPar(a = a, b = a/b))}
#> <bytecode: 0x563fd70c85f0>
#> <environment: 0x563fcca9da88>
#> attr(,"lower")
#> [1] -Inf -Inf
#> attr(,"upper")
#> [1] Inf Inf
#> 
  names(bevholt())
#> [1] "logl"    "model"   "initial"
  bevholt()$logl
#> function(a, b, rec, ssb)
#>       loglAR1(log(rec), log(a*ssb/(b+ssb)))
#> <bytecode: 0x563fd70c6b30>
#> <environment: 0x563fcca2ea70>

# once an FLSR model is in the workspace ...
  data(nsher)

# the three model-definition slots can be modified
# at once by calling 'model<-' with
# (1) a list
  model(nsher) <- bevholt()

# (2) the name of the function returning this list
  model(nsher) <- 'bevholt'

# or (3) the function itself that returns this list
  model(nsher) <- bevholt
```

</div>

</div>

</div>
