# Interpolate cohort weights at any time within the year

Interpolates weights-at-age at a fraction of the year, following each
cohort between consecutive season nodes. The node after the last season
is the next year's first season at age + 1. Annual input (one season)
interpolates across years. The plus group and final year have no
successor, so w1 = w0.

## Usage

``` r
wt_at_time(
  x,
  time = 0.5,
  method = c("linear", "cuberoot", "loglin", "power"),
  k = 2
)
```

## Arguments

- x:

  An FLQuant of weights at the start of each season. Annual objects (one
  season) are also accepted.

- time:

  Fraction of the year in 0, 1 at which to interpolate.

- method:

  Interpolation method: one of "linear", "cuberoot", "loglin" or
  "power".

- k:

  Exponent used by the "power" method.

## Value

An FLQuant with the same age, year, unit, area and iter dimensions, and
a single season named after `time`.

## Examples

``` r
x <- FLQuant(NA, dimnames = list(age = 1:3, year = 2000:2002, season = 1:4))
x[] <- 1 + outer(1:3, 2000:2002, function(a, y) 0.2 * a + 0.3 * (y - 2000))
wt_at_time(x, time = 0.25, method = "linear")
#> An object of class "FLQuant"
#> , , unit = unique, season = all, area = unique
#> 
#>    year
#> age 2000 2001 2002
#>   1 1.2  1.5  1.8 
#>   2 1.4  1.7  2.0 
#>   3 1.6  1.9  2.2 
#> 
#> units:  NA 
```
