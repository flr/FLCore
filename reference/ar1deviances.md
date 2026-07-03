# Extend AR(1) log-normal deviances beyond a given year

Replaces the values of an `FLQuant` beyond a specified year with
simulated AR(1) log-normal deviates, using the autocorrelation and
standard deviation estimated from the historical period up to and
including that year.

## Usage

``` r
ar1deviances(x, year)
```

## Arguments

- x:

  An `FLQuant` of log-normal deviances with historical data up to at
  least `year`.

- year:

  Integer or character; the last year of the historical period. Values
  in `x` after this year will be replaced by simulations.

## Value

An `FLQuant` of the same dimensions as `x`, with years after `year`
replaced by AR(1) log-normal simulations estimated from the historical
data.

## See also

[rlnormar1](rlnormar1.md)

## Author

The FLR Team
