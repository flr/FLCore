# Generate an AR(1) lognormal FLQuant time-series

Simulate one or more iterated log-normal AR(1) series across specified
years and return the results as an FLQuant.

## Usage

``` r
rlnormar1(
  n = NULL,
  meanlog = 0,
  sdlog = 1,
  rho = 0,
  years,
  bias.correct = FALSE
)
```

## Arguments

- n:

  Integer. Number of iterations to generate. If `NULL` (default) `n` is
  set to the maximum length of `meanlog`, `sdlog` and `rho`.

- meanlog:

  Numeric. Mean(s) of the normal (log) distribution for the innovations.
  Recycled to length `n` if necessary.

- sdlog:

  Numeric. Standard deviation(s) of the normal (log) distribution for
  the innovations. Recycled to length `n` if necessary.

- rho:

  Numeric. AR(1) autocorrelation parameter(s) in -1, 1. Recycled to
  length `n` if necessary.

- years:

  Integer or character vector. Years (time dimension) for the returned
  FLQuant (length determines number of time steps).

- bias.correct:

  Logical. If TRUE (default FALSE) subtract 0.5 \* sdlog^2 from the
  log-series before exponentiation to correct for the lognormal bias.

## Value

An object of class FLQuant with dimensions year x iter containing the
simulated log-normal AR(1) series.

## Details

The function first draws independent normal samples with given `meanlog`
and `sdlog`, constructs an AR(1) process with correlation parameter
`rho`, and finally exponentiates the series. If `bias.correct = TRUE`
the log-normal bias (0.5 \* sdlog^2) is removed on the log-scale so that
the resulting series have the requested `meanlog` on the log-scale.

The simulation proceeds as follows:

1.  Draw independent normal innovations: eps_t,i ~ N(meanlog_i,
    sdlog_i^2).

2.  Build the AR(1) process: x_t,i = rho_i \* x_t-1,i + sqrt(1 -
    rho_i^2) \* eps_t,i, with x_1,i = eps_1,i.

3.  Optionally apply bias correction on the log scale, then
    exponentiate: y_t,i = exp(x_t,i - 0.5 \* sdlog_i^2) (if bias.correct
    = TRUE).

## References

Thorson, J. T. Predicting recruitment density dependence and intrinsic
growth rate for all fishes worldwide using a data-integrated
life-history model. Fish Fish. 2020; 21: 237– 251.
https://doi-org.ezproxy.library.wur.nl/10.1111/faf.12427

## See also

[rlnorm](https://rdrr.io/r/stats/Lognormal.html)

## Author

Iago Mosqueira (WMR), Henning Winker (JRC).

## Examples

``` r
# 6 years, 5 iterations, rho = 0.5, default meanlog/sdlog
rlnormar1(n = 5, rho = 0.5, years = 2000:2005)
#> An object of class "FLQuant"
#> iters:  5 
#> 
#> , , unit = unique, season = all, area = unique
#> 
#>      year
#> quant 2000          2001          2002          2003          2004         
#>   all 0.963(0.7060) 0.442(0.3261) 0.699(0.4347) 0.368(0.0928) 0.460(0.2223)
#>      year
#> quant 2005         
#>   all 2.657(3.8044)
#> 
#> units:  NA 

# varying sdlog per iteration
rlnormar1(n = 3, sdlog = c(0.5, 1, 1.5), rho = 0.3, years = 1990:1994)
#> An object of class "FLQuant"
#> iters:  3 
#> 
#> , , unit = unique, season = all, area = unique
#> 
#>      year
#> quant 1990         1991         1992         1993         1994        
#>   all 0.331(0.281) 0.148(0.194) 0.318(0.202) 0.389(0.168) 1.449(0.238)
#> 
#> units:  NA 
```
