# Method sweep for FLCore classes

Use R's sweep method on FLCore classes

## Usage

``` r
# S4 method for class 'FLArray'
sweep(x, MARGIN, STATS, FUN = "-", check.margin = TRUE, ...)

# S4 method for class 'FLPar'
sweep(x, MARGIN, STATS, FUN = "-", check.margin = TRUE, ...)
```

## Details

These methods call base R [`sweep`](https://rdrr.io/r/base/sweep.html)
method on **FLCore** classes and then ensure that the returned object is
of same class.

## Generic function

sweep(x, MARGIN, STATS, FUN = "-", check.margin = TRUE, ...)

## See also

[sweep](https://rdrr.io/r/base/sweep.html)

## Author

The FLR Team

## Examples

``` r
flq <- FLQuant(rlnorm(90), dim=c(3,10), units='kg')
# Get ratio of max value by year
sweep(flq, 2, apply(flq, 2, max), "/")
#> An object of class "FLQuant"
#> , , unit = unique, season = all, area = unique
#> 
#>      year
#> quant 1      2      3      4      5      6      7      8      9      10    
#>     1 0.5526 0.7404 0.1213 1.0000 0.9036 0.2039 0.5955 1.0000 1.0000 0.6816
#>     2 1.0000 0.1039 0.9202 0.7770 0.8194 0.6245 1.0000 0.1475 0.0775 0.6528
#>     3 0.0995 1.0000 1.0000 0.2363 1.0000 1.0000 0.8853 0.8409 0.2124 1.0000
#> 
#> units:  kg 
```
