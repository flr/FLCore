# Method BIC Bayesian information criterion (BIC) method

A method to calculate the Bayesian information criterion (BIC), also
known as Schwarz's Bayesian criterion of an [FLModel](FLModel.md) object
from the value of the obtained log-likelihood stored in its `logLik`
slot.

## Usage

``` r
# S4 method for class 'FLModel'
BIC(object)
```

## Arguments

- object:

  a fitted FLModel object for which there exists a 'logLik' method to
  extract the corresponding log-likelihood.

## Generic function

BIC(object)

## See also

[AIC](https://rdrr.io/r/stats/AIC.html),
[BIC](https://rdrr.io/r/stats/AIC.html), [FLModel](FLModel.md),
[logLik](https://rdrr.io/r/stats/logLik.html)

## Author

The FLR Team

## Examples

``` r
data(nsher)
BIC(nsher)
#> [1] -24.11118
```
