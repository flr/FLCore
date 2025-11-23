<div id="main" class="col-md-9" role="main">

# Method BIC Bayesian information criterion (BIC) method

<div class="ref-description section level2">

A method to calculate the Bayesian information criterion (BIC), also
known as Schwarz's Bayesian criterion of an [FLModel](FLModel.md) object
from the value of the obtained log-likelihood stored in its `logLik`
slot.

</div>

<div class="section level2">

## Usage

<div class="sourceCode">

``` r
# S4 method for class 'FLModel'
BIC(object)
```

</div>

</div>

<div class="section level2">

## Arguments

-   object:

    a fitted FLModel object for which there exists a 'logLik' method to
    extract the corresponding log-likelihood.

</div>

<div class="section level2">

## Generic function

BIC(object)

</div>

<div class="section level2">

## See also

<div class="dont-index">

[AIC](https://rdrr.io/r/stats/AIC.html),
[BIC](https://rdrr.io/r/stats/AIC.html), [FLModel](FLModel.md),
[logLik](https://rdrr.io/r/stats/logLik.html)

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
data(nsher)
BIC(nsher)
#> [1] -24.11118
```

</div>

</div>

</div>
