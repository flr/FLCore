<div id="main" class="col-md-9" role="main">

# Method jackknife

<div class="ref-description section level2">

Jackknife resampling

</div>

<div class="section level2">

## Usage

<div class="sourceCode">

``` r
# S4 method for class 'FLQuant'
jackknife(object, dim = "year", na.rm = TRUE)

# S4 method for class 'FLQuants'
jackknife(object, ...)

# S4 method for class 'FLModel'
jackknife(object, slot)
```

</div>

</div>

<div class="section level2">

## Details

The `jackknife` method sets up objects ready for jackknifing, i.e. to
systematically recompute a given statistic leaving out one observation
at a time. From this new set of "observations" for the statistic,
estimates for the bias and variance of the statstic can be calculated.

Input objects cannot have length \> 1 along the `iter` dimension, and
the main slot in the resulting object will have as many `iter`s as the
number of elements in the original object that are not `NA`.

</div>

<div class="section level2">

## Generic function

jackknife(object, ...)

</div>

<div class="section level2">

## See also

<div class="dont-index">

`FLQuantJK` `FLParJK`

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
flq <- FLQuant(1:8)
flj <- jackknife(flq)
iters(flj)
#> -- iter:  1 
#>  1  2  3  4  5  6  7  8 
#> NA  2  3  4  5  6  7  8 
#> -- iter:  2 
#>  1  2  3  4  5  6  7  8 
#>  1 NA  3  4  5  6  7  8 
#> -- iter:  3 
#>  1  2  3  4  5  6  7  8 
#>  1  2 NA  4  5  6  7  8 
#> -- iter:  4 
#>  1  2  3  4  5  6  7  8 
#>  1  2  3 NA  5  6  7  8 
#> -- iter:  5 
#>  1  2  3  4  5  6  7  8 
#>  1  2  3  4 NA  6  7  8 
#> -- iter:  6 
#>  1  2  3  4  5  6  7  8 
#>  1  2  3  4  5 NA  7  8 
#> -- iter:  7 
#>  1  2  3  4  5  6  7  8 
#>  1  2  3  4  5  6 NA  8 
#> -- iter:  8 
#>  1  2  3  4  5  6  7  8 
#>  1  2  3  4  5  6  7 NA 
#> 
#> units:  NA 
#> NULL
```

</div>

</div>

</div>
