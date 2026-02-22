# Generate a matrix to compute Mohn's rho for a single metric

A common measure of the strength of stock assessment retrospective
patterns is Mohn's rho. This function does not carry out the calculation
but returns a matrix with the metrics value for the n restrospective
runs, in columns, and n + 2 years, in rows.

## Usage

``` r
mohnMatrix(stocks, metric = "fbar", ...)
```

## Arguments

- stocks:

  An FLStocks object from a restrospective analysis

- metric:

  Metric to be computed, as a character vector or function

## Value

A metrics of n + 2 x n, where n is the numbers of objects in stocks.
