# Generate a matrix to compute Mohn's rho for a single metric

Creates a matrix to compute Mohn's rho for a single metric from
retrospective analysis results. A common measure of the strength of
stock out the calculation but returns a matrix with the metrics value
for the n restrospective runs, in columns, and n + 2 years, in rows.

## Usage

``` r
mohnMatrix(stocks, metric = "fbar", ...)
```

## Arguments

- stocks:

  An FLStocks object from a restrospective analysis

- metric:

  Metric to be computed, as a character vector or function

- ...:

  Additional arguments passed to the metric function

## Value

A metrics of n + 2 x n, where n is the numbers of objects in stocks.

## See also

[FLStocks](FLStocks.md)

## Author

FLR Team

## Examples

``` r
data(ple4)
# Create retrospective runs
stks <- FLStocks(lapply(0:5, function(i) window(ple4, end=2017-i)))
mohnMatrix(stks, "fbar")
#>           base        -1        -2        -3        -4        -5
#> 2010 0.1964252 0.1964252 0.1964252 0.1964252 0.1964252 0.1964252
#> 2011 0.1981524 0.1981524 0.1981524 0.1981524 0.1981524 0.1981524
#> 2012 0.2073830 0.2073830 0.2073830 0.2073830 0.2073830 0.2073830
#> 2013 0.2097138 0.2097138 0.2097138 0.2097138 0.2097138        NA
#> 2014 0.2053652 0.2053652 0.2053652 0.2053652        NA        NA
#> 2015 0.2008396 0.2008396 0.2008396        NA        NA        NA
#> 2016 0.1987834 0.1987834        NA        NA        NA        NA
#> 2017 0.1987090        NA        NA        NA        NA        NA
```
