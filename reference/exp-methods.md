# exp and log methods FLCore array-based classes

Compute the exponential and logarithmic functions

## Usage

``` r
# S4 method for class 'FLQuant'
exp(x)

# S4 method for class 'FLQuant'
log(x, ...)
```

## Details

This method simply calls R's base::exp and
[base::drop](https://rdrr.io/r/base/drop.html), but take care of
returning the right units of measurement, that is "" or character(1).

## See also

base::exp base::log

## Author

The FLR Team

## Examples

``` r
x <- FLQuant(c(4,2,7,4,2,9), units="1000")
log(x)
#> An object of class "FLQuant"
#> , , unit = unique, season = all, area = unique
#> 
#>      year
#> quant 1     2     3     4     5     6    
#>   all 1.386 0.693 1.946 1.386 0.693 2.197
#> 
#> units:   
units(log(x))
#> [1] ""
```
