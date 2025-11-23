<div id="main" class="col-md-9" role="main">

# exp and log methods FLCore array-based classes

<div class="ref-description section level2">

Compute the exponential and logarithmic functions

</div>

<div class="section level2">

## Usage

<div class="sourceCode">

``` r
# S4 method for class 'FLQuant'
exp(x)

# S4 method for class 'FLQuant'
log(x, ...)
```

</div>

</div>

<div class="section level2">

## Details

This method simply calls R's base::exp and
[base::drop](https://rdrr.io/r/base/drop.html), but take care of
returning the right units of measurement, that is "" or character(1).

</div>

<div class="section level2">

## See also

<div class="dont-index">

base::exp base::log

</div>

</div>

<div class="section level2">

## Author

The FLR Team

</div>

<div class="section level2">

## Examples

<div class="sourceCode">

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

</div>

</div>

</div>
