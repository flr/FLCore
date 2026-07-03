# Generate an AR(1) lognormal FLQuant time-series (deprecated)

Deprecated. Please use [rlnormar1](rlnormar1.md) instead.

## Usage

``` r
ar1rlnorm(
  rho,
  years,
  iters = 1,
  meanlog = 0,
  sdlog = 1,
  bias.correct = FALSE,
  ...
)
```

## Arguments

- rho:

  Numeric; AR(1) autocorrelation parameter in \\\[-1, 1\]\\.

- years:

  Integer or character vector of years for the time dimension.

- iters:

  Integer; number of iterations to generate; defaults to 1.

- meanlog:

  Numeric; mean of the normal (log) distribution; defaults to 0.

- sdlog:

  Numeric; standard deviation of the normal (log) distribution; defaults
  to 1.

- bias.correct:

  Logical; if `TRUE` apply log-normal bias correction; defaults to
  `FALSE`.

- ...:

  Additional arguments (unused).

## Value

An `FLQuant` with dimensions `1 x length(years) x 1 x 1 x 1 x iters`.

## See also

[rlnormar1](rlnormar1.md)

## Author

The FLR Team
