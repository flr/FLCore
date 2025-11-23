<div id="main" class="col-md-9" role="main">

# Generates a time series of possible bias-corrected lognormal autocorrelated random values

<div class="ref-description section level2">

Thorston, 2020.

</div>

<div class="section level2">

## Usage

<div class="sourceCode">

``` r
ar1rlnorm(
  rho,
  years,
  iters = 1,
  meanlog = 0,
  sdlog = 1,
  bias.correct = TRUE,
  ...
)
```

</div>

</div>

<div class="section level2">

## Arguments

-   rho:

    Autocorrelation coefficient.

-   years:

    Vector of year names.

-   iters:

    Number of iterations.

-   meanlog:

    Mean of the series in log space.

-   sdlog:

    Marginal standard deviation in log space.

-   bias.correct:

    Should bias-correction be applied? Defaults to TRUE.

</div>

<div class="section level2">

## Value

An FLQuant object

</div>

<div class="section level2">

## References

Thorson, J. T. Predicting recruitment density dependence and intrinsic
growth rate for all fishes worldwide using a data-integrated
life-history model. Fish Fish. 2020; 21: 237– 251.
https://doi-org.ezproxy.library.wur.nl/10.1111/faf.12427

</div>

<div class="section level2">

## See also

<div class="dont-index">

[rlnorm](https://rdrr.io/r/stats/Lognormal.html)

</div>

</div>

<div class="section level2">

## Author

Iago Mosqueira (WMR), Henning Winker (JRC).

</div>

<div class="section level2">

## Examples

<div class="sourceCode">

``` r
devs <- ar1rlnorm(rho=0.6, years=2000:2030, iter=500, meanlog=0, sdlog=1)
plot(devs)
```

</div>

</div>

</div>
