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
#>     1 0.1471 0.5857 1.0000 0.7652 0.1617 0.9179 1.0000 0.1977 0.1772 0.1783
#>     2 1.0000 1.0000 0.1151 0.1424 1.0000 1.0000 0.5166 0.1734 0.2015 1.0000
#>     3 0.8220 0.8964 0.0987 1.0000 0.9129 0.2443 0.4008 1.0000 1.0000 0.5717
#> 
#> units:  kg 
```
