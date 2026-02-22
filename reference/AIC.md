# Method AIC

Akaike's information criterion (AIC) method A method to calculate
Akaike's 'An Information Criterion' (AIC) of an [FLModel](FLModel.md)
object from the value of the obtained log-likelihood stored in its
`logLik` slot.

## Usage

``` r
# S4 method for class 'FLModel,numeric'
AIC(object, k = 2)
```

## Arguments

- object:

  an FLModel object

- k:

  the penalty per parameter to be used; the default 'k = 2' is the
  classical AIC.

## Generic function

AIC(object, k)

## See also

[AIC](https://rdrr.io/r/stats/AIC.html),
[logLik](https://rdrr.io/r/stats/logLik.html), [FLModel](FLModel.md)

## Author

The FLR Team

## Examples

``` r
data(nsher)
AIC(nsher)
#> [1] -27.7245
```
