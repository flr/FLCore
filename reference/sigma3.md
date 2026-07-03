# Compute 3-sigma control limits and a runs test p-value

Computes the 3-sigma control limits (lower and upper) for an `FLQuant`
time series using the average moving range method (Montgomery, 2009),
and performs a non-parametric runs test to assess whether the series
shows a systematic trend.

## Usage

``` r
sigma3(x, mixing = "less", type = "residual")
```

## Arguments

- x:

  An `FLQuant` time series (typically residuals).

- mixing:

  Character; alternative hypothesis for the runs test. One of
  `"two.sided"`, `"less"` (default, tests for trend), or `"greater"`.

- type:

  Character; reserved for future use; currently ignored.

## Value

A named list with elements:

- lcl:

  Lower 3-sigma control limit (negative).

- ucl:

  Upper 3-sigma control limit (positive).

- p.value:

  p-value from the runs test; values \>= 0.05 suggest acceptable
  randomness.

## See also

[runstest](runstest.md), [.runs.test](dot-runs.test.md)

## Author

The FLR Team

## Examples

``` r
data(ple4)
sigma3(catch.n(ple4))
#> $lcl
#> [1] -90490.17
#> 
#> $ucl
#> [1] 90490.17
#> 
#> $p.value
#> [1] 0.001
#> 
data(nsher)
sigma3(residuals(nsher))
#> $lcl
#> [1] -1.189247
#> 
#> $ucl
#> [1] 1.189247
#> 
#> $p.value
#> [1] 0.234
#> 
```
