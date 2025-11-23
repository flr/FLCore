<div id="main" class="col-md-9" role="main">

# Method AIC

<div class="ref-description section level2">

Akaike's information criterion (AIC) method A method to calculate
Akaike's 'An Information Criterion' (AIC) of an [FLModel](FLModel.md)
object from the value of the obtained log-likelihood stored in its
`logLik` slot.

</div>

<div class="section level2">

## Usage

<div class="sourceCode">

``` r
# S4 method for class 'FLModel,numeric'
AIC(object, k = 2)
```

</div>

</div>

<div class="section level2">

## Arguments

-   object:

    an FLModel object

-   k:

    the penalty per parameter to be used; the default 'k = 2' is the
    classical AIC.

</div>

<div class="section level2">

## Generic function

AIC(object, k)

</div>

<div class="section level2">

## See also

<div class="dont-index">

[AIC](https://rdrr.io/r/stats/AIC.html),
[logLik](https://rdrr.io/r/stats/logLik.html), [FLModel](FLModel.md)

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
AIC(nsher)
#> [1] -27.7245
```

</div>

</div>

</div>
